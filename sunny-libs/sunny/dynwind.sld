(define-library (sunny dynwind)
  (import (sunny core)
          (sunny derived-syntax))
  (export dynamic-wind call/cc)
  (begin
    (define primitive-call/cc call/cc)

    (define *TOP* '())

    (define make-winder cons)

    (define (enter winder)
      ((car winder)))

    (define (leave winder)
      ((cdr winder)))

    (define (push-winder winder)
      (set! *TOP* (cons winder *TOP*)))

    (define (pop-winder)
      (set! *TOP* (cdr *TOP*)))

    (define (call/cc proc)
      (let ((old-top *TOP*))
        (primitive-call/cc
          (lambda (cont)
            (proc (lambda (arg)
                    (wind *TOP* old-top)
                    (cont arg)))))))

    (define (dynamic-wind before body after)
      (before)
      (push-winder (make-winder before after))
      (let ((result (body)))
        (pop-winder)
        (after)
        result))

    (define (wind from to)
      (display "(wind ")
      (display from)
      (display " ")
      (display to)
      (display ")")
      (newline)
      (set! *TOP* from)
      (cond ((eq? from to) 'ok)
            ((null? from) (wind from (cdr to))
                          (enter (car to)))
            ((null? to) (leave (car from))
                        (wind (cdr from) to))
            (else (leave (car from))
                  (wind (cdr from) (cdr to))
                  (enter (car to))))
      (set! *TOP* to))))
