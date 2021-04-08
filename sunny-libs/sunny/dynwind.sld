(define-library (sunny dynwind)
  (import (sunny core))
  (export dynamic-wind call/cc)
  (begin
    (define builtin-call/cc call/cc)

    (define *TOP* '())

    (define (make-winder in out)
      (cons in out))

    (define (push-winder winder)
      (set! *TOP* (cons winder *TOP*)))

    (define (pop-winder)
      (set! *TOP* (cdr *TOP*)))

    (define (call/cc proc)
      (let ((current *TOP*))
        (builtin-call/cc
          (lambda (cont)
            (proc (lambda (arg)
                    (wind-to *TOP* current)
                    (cont arg)))))))

    (define (dynamic-wind before body after)
      (before)
      (push-winder (make-wind before after))
      (let ((result (body)))
        (pop-winder)
        (after)
        result))

    (define (wind-to from to)
      (set! *TOP* from)
      (cond ((eq? from to) 'ok)
            ((null? from) (wind-to from (cdr to))
                          ((winder-in to)))
            ((null? to) ((winder-out from))
                        (wind-to (cdr from) to))
            (else (winder-out from)
                  (wind-to (cdr from) (cdr to))
                  (winder-in to)))
      (set! *TOP* to))))
