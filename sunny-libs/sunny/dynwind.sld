(define-library (sunny dynwind)
  (import (sunny core)
          (sunny derived-syntax))
  (export dynamic-wind call/cc dummy/cc)
  (begin
    (define builtin-call/cc call/cc)

    (define *TOP* '())

    (define (make-winder in out)
      (cons in out))

    (define winder-in car)
    (define winder-out cdr)

    (define (push-winder winder)
      (set! *TOP* (cons winder *TOP*)))

    (define (pop-winder)
      (set! *TOP* (cdr *TOP*)))

    (define (call/cc proc)
      (let ((current *TOP*))
        (builtin-call/cc
          (lambda (cont)
            (proc (lambda (arg)
                    (wind *TOP* current)
                    (cont arg)))))))

    (define (dummy/cc proc)
      (builtin-call/cc
        proc))

    (define (dynamic-wind before body after)
      (before)
      (push-winder (make-wind before after))
      (let ((result (body)))
        (pop-winder)
        (after)
        result))

    (define (wind from to)
      (set! *TOP* from)
      (cond ((eq? from to) 'ok)
            ((null? from) (wind from (cdr to))
                          ((winder-in to)))
            ((null? to) ((winder-out from))
                        (wind (cdr from) to))
            (else (winder-out from)
                  (wind (cdr from) (cdr to))
                  (winder-in to)))
      (set! *TOP* to))))
