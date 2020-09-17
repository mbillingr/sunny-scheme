(define-library (sunny utils)

  (export dotted-list?
          filter
          last-cdr
          proper-list-part)

  (import (scheme base))

  (begin
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
          '()))))
