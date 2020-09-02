(define-library (sunny utils)

  (export dotted-list?
          last-cdr)

  (import (scheme base))

  (begin
    (define (dotted-list? seq)
      (not (null? (last-cdr seq))))

    (define (last-cdr seq)
      (if (pair? seq)
          (last-cdr (cdr seq))
          seq))))
