(define-library (sunny utils)

  (export any
          atom?
          bor
          dotted-list?
          filter
          last-cdr
          proper-list-part
          reduce)

  (import (scheme base))

  (begin
    (define (atom? x)
      (if (pair? x)
          #f
          #t))

    (define (dotted-list? seq)
      (not (null? (last-cdr seq))))

    (define (last-cdr seq)
      (if (pair? seq)
          (last-cdr (cdr seq))
          seq))

    (define (proper-list-part seq)
      (if (pair? seq)
          (cons (car seq)
                (proper-list-part (cdr seq)))
          '()))

    (define (filter f seq)
      (if (pair? seq)
          (if (f (car seq))
              (cons (car seq)
                    (filter f (cdr seq)))
              (filter f (cdr seq)))
          '()))

    (define (reduce f init seq)
      (if (pair? seq)
          (reduce f (f init (car seq)) (cdr seq))
          init))

    (define (any f seq)
      (if (pair? seq)
          (if (f (car seq))
              #t
              (any f (cdr seq)))
          #f))

    (define (bor first . args)
      (if first
          first
          (if (null? args)
              #f
              (apply bor args))))))
