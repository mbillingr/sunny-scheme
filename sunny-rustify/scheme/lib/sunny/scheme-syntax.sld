(define-library (sunny scheme-syntax)
  (export abstraction?
          and-args
          cond-clauses
          cond-clause-condition
          cond-clause-sequence
          definition?
          definition-variable
          definition-value
          if-condition
          if-consequence
          if-alternative
          import?
          import-libnames
          library?
          scan-out-defines)

  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (sunny utils))

  (begin
    (define (abstraction? expr)
     (and (pair? expr)
          (eq? 'lambda (car expr))))


    (define (and-args expr)
      (cdr expr))


    (define (cond-clauses expr)
      (cdr expr))

    (define (cond-clause-condition clause)
      (car clause))

    (define (cond-clause-sequence clause)
      (cdr clause))


    (define (definition? expr)
      (and (pair? expr)
           (eq? (car expr) 'define)))

    (define (definition-variable expr)
      (if (pair? (cadr expr))
          (caadr expr)
          (cadr expr)))

    (define (definition-value expr)
      (if (pair? (cadr expr))
          (cons 'lambda
                (cons (cdadr expr)
                      (cddr expr)))
          (caddr expr)))


    (define (if-condition expr)
      (cadr expr))

    (define (if-consequence expr)
      (caddr expr))

    (define (if-alternative expr)
      (if (pair? (cdddr expr))
          (cadddr expr)
          ''*UNSPECIFIED*))


    (define (import? expr)
      (and (pair? expr)
           (eq? (car expr) 'import)))

    (define (import-libnames exp*)
      (filter
        (lambda (libname)
           (not (equal? libname '(sunny testing))))
        (map importset-libname (cdr exp*))))

    (define (importset-libname expr)
      (cond ((eq? 'only (car expr))
             (importset-libname (cadr expr)))
            ((eq? 'except (car expr))
             (importset-libname (cadr expr)))
            (else expr)))


    (define (library? exp*)
      (and (pair? exp*)
           (eq? 'define-library (car exp*))))


    (define (scan-out-defines body)
      (define (initializations exp*)
        (cond ((null? exp*)
               '())
              ((definition? (car exp*))
               (cons (list (definition-variable (car exp*))
                           (definition-value (car exp*)))
                     (initializations (cdr exp*))))
              (else (initializations (cdr exp*)))))
      (define (transform exp*)
        (cond ((null? exp*)
               '())
              ((definition? (car exp*))
               (transform (cdr exp*)))
              (else (cons (car exp*)
                          (transform (cdr exp*))))))
      (list (cons 'letrec
                  (cons (initializations body)
                        (transform body)))))))
