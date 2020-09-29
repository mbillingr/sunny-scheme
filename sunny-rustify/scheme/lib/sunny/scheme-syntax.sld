(define-library (sunny scheme-syntax)
  (export abstraction?
          and-args
          begin-statements
          cond-clauses
          cond-clause-condition
          cond-clause-sequence
          cond-else-clause?
          definition?
          definition-signature
          definition-variable
          definition-value
          if-condition
          if-consequence
          if-alternative
          import?
          import-libnames
          lambda-body
          lambda-params
          let-args
          let-body
          let-vars
          let*-bindings
          library?
          scan-out-defines
          set!-variable
          set!-value)

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


    (define (begin-statements expr)
      (cdr expr))


    (define (cond-clauses expr)
      (cdr expr))

    (define (cond-clause-condition clause)
      (car clause))

    (define (cond-clause-sequence clause)
      (cdr clause))

    (define (cond-else-clause? clause)
      (eq? 'else (car clause)))


    (define (definition? expr)
      (and (pair? expr)
           (eq? (car expr) 'define)))

    (define (definition-signature expr)
      (if (pair? (cadr expr))
          (list (cadr expr) '...)
          (cdr expr)))

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


    (define (lambda-body expr)
      (cddr expr))

    (define (lambda-params expr)
      (cadr expr))


    (define (let-args expr)
      (map cadr (cadr expr)))

    (define (let-body expr)
      (cddr expr))

    (define (let-vars expr)
      (map car (cadr expr)))

    (define (let*-bindings expr)
      (cadr expr))


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
      (let ((ini (initializations body)))
        (if (null? ini)
            body
            (list (cons 'letrec
                        (cons ini
                              (transform body)))))))

    (define (set!-variable expr)
      (cadr expr))

    (define (set!-value expr)
      (caddr expr))))
