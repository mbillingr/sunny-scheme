(define-library (sunny utils)

  (export any
          atom?
          bor
          dotted-list?
          filter
          last-cdr
          proper-list-part
          reduce
          same-name?
          sort)

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
              (apply bor args))))

    (define (sort cmp ass)
      (if (pair? ass)
          (let ((pivot (car ass)))
            (append (sort cmp
                          (filter (lambda (x) (cmp x pivot))
                                  (cdr ass)))
                    (cons pivot
                          (sort cmp
                                (filter (lambda (x) (not (cmp x pivot)))
                                        (cdr ass))))))
          '()))

    (define (same-name? a b)
      (cond ((and (symbol? a) (symbol? b))
             (eq? a b))
            ((and (string? a) (string? b))
             (equal? a b))
            ((symbol? a)
             (same-name? (symbol->string a) b))
            ((symbol? b)
             (same-name? a (symbol->string b)))
            (else (error "invalid names" a b))))))
