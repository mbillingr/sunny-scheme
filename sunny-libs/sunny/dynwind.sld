(define-library (sunny dynwind)
  (import (sunny core)
          (sunny derived-syntax))
  (export dynamic-wind call/cc *TOP*)
  (begin
    (define primitive-call/cc call/cc)

    (define *TOP* '())

    (define make-winder cons)

    (define (enter winder)
      (display "ENTER")
      ((car winder)))

    (define (leave winder)
      (display "LEAVE")
      ((cdr winder)))

    (define (push-winder winder)
      (display "PUSH")
      (set! *TOP* (cons winder *TOP*)))

    (define (pop-winder)
      (display "POP")
      (set! *TOP* (cdr *TOP*)))

    (define (call/cc proc)
      (let ((old-top *TOP*))
        (primitive-call/cc
          (lambda (cont)
            (proc (lambda (arg)
                    (display "TOP ")
                    (display *TOP*)
                    (newline)
                    (display "old-top ")
                    (display old-top)
                    (newline)
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
      (display "done")
      (set! *TOP* to))

    '(tests
      (begin
        (define re 0)
        (+ 1 (call/cc (lambda (c) (set! re c) (c 2))) 3))
      (begin
        (define re 0)
        (dynamic-wind (lambda () (display " in "))
                      (lambda () (display " pre ")
                                 (call/cc (lambda (c) (set! re c)))
                                 (display " post ")
                                 0)
                      (lambda () (display " out ")))
        (dynamic-wind (lambda () (display " a "))
                      (lambda () (display " b ")
                                 (re 42)
                                 (display " c ")
                                 0)
                      (lambda () (display " d "))))

      (define (wrap/cc proc)
        (call/cc proc))

      (define (a)
        (b)
        'a)

      (define (b)
        (c)
        'b)

      (define (c)
        (d)
        'c)

      (define (d)
        (call/cc (lambda (k) (set! re k)))
        'd)

      'end)))
