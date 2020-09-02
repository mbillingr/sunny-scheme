(define-library (syntax)

  (export cond-clauses
          cond-clause-condition
          cond-clause-sequence
          definition?
          definition-value
          definition-variable
          if-condition
          if-consequence
          if-alternative
          import?
          import-libnames
          importset-libname
          library?
          library-decls
          library-exports
          library-name)

  (import (scheme base)
          (scheme cxr))

  (body
    (define (library? exp*)
      (and (pair? exp*)
           (eq? 'define-library (car exp*))))


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


    (define (cond-clauses expr)
      (cdr expr))

    (define (cond-clause-condition clause)
      (car clause))

    (define (cond-clause-sequence clause)
      (cdr clause))


    (define (import? expr)
      (and (pair? expr)
           (eq? (car expr) 'import)))

    (define (import-libnames exp*)
      (map importset-libname (cdr exp*)))

    (define (importset-libname expr)
      (cond ((eq? 'only (car expr))
             (importset-libname (cadr expr)))
            ((eq? 'except (car expr))
             (importset-libname (cadr expr)))
            (else expr)))

    (define (library-name expr)
      (cadr expr))

    (define (library-decls expr)
      (cddr expr))

    (define (library-exports lib-decl*)
      (cond ((null? lib-decl*) '())
            ((eq? 'export (caar lib-decl*))
             (append (cdar lib-decl*)
                     (library-exports (cdr lib-decl*))))
            (else (library-exports (cdr lib-decl*)))))))
