(define-library (sunny astify-toplevel)
  (export astify-library
          astify-program
          astify-toplevel)

  (import (scheme base)
          (scheme cxr)
          (sunny ast)
          (sunny ast-transforms boxify)
          (sunny ast-transforms close-procedures)
          (sunny ast-transforms extract-definitions)
          (sunny astify)
          (sunny library)
          (sunny scheme-syntax)
          (sunny sets)
          (sunny syntax)
          (sunny utils)
          (sunny variable))

  (begin
    (define (astify-toplevel exp* ast-transform)
      (if (library? (car exp*))
          (astify-library (library-name (car exp*))
                          (library-decls (car exp*))
                          (list '())
                          ast-transform)
          (astify-program exp* ast-transform)))

    (define (astify-library name exp* library-env ast-transform)
      (define init (make-set))
      (define body (make-nop))
      (define global-env (make-core-env))
      (define imports '())
      (define exports '())
      (define (process-library-decls exp*)
        (cond ((null? exp*)
               'DONE)
              ((eq? 'export (caar exp*))
               (set! exports (append exports (cdar exp*)))
               (process-library-decls (cdr exp*)))
              ((import? (car exp*))
               (register-libraries (import-libnames (car exp*)) library-env ast-transform)
               (set! init (set-add* init (import-libnames (car exp*))))
               (set! imports (append imports (astify-import (cdar exp*) global-env)))
               (process-library-decls (cdr exp*)))
              ((eq? 'begin (caar exp*))
               (set! body (make-sequence body
                                         (astify-sequence (cdar exp*)
                                                          global-env #f)))
               (process-library-decls (cdr exp*)))))
      (process-library-decls exp*)
      (let* ((globals (sort (lambda (a b)
                              (string<? (variable-name a)
                                        (variable-name b)))
                            (cdr global-env))))
        (ast-transform
          (make-library name globals init body imports (astify-export exports global-env)))))


    (define (astify-program exp* ast-transform)
      (define global-env (make-core-env))
      (define library-env (list '()))

      (define (process-imports exp* imports init)
        (cond ((import? (car exp*))
               (register-libraries (import-libnames (car exp*))
                                   library-env
                                   ast-transform)
               (process-imports (cdr exp*)
                                (append imports
                                        (astify-import (cdar exp*) global-env))
                                (set-add* init (import-libnames (car exp*)))))

              (else (let* ((body (astify-sequence exp* global-env #f))
                           (globals (sort (lambda (a b)
                                            (string<? (variable-name a)
                                                      (variable-name b)))
                                          (cdr global-env))))
                      (ast-transform
                        (make-program globals
                                      imports
                                      init
                                      body
                                      (filter cdr (car library-env))))))))

      (process-imports exp* '() (make-set)))

    (define (register-libraries libs library-env ast-transform)
      (cond ((null? libs) 'DONE)
            ((equal? '(sunny testing) (car libs))  ; ignore testing library
             (register-libraries (cdr libs) library-env ast-transform))
            ((assoc (car libs) (car library-env))
             (register-libraries (cdr libs) library-env ast-transform))
            (else
              (let* ((lib (get-lib (car libs)))
                     (libast (if (library? lib)
                                 (astify-library (library-name lib)
                                                 (library-decls lib)
                                                 library-env
                                                 ast-transform)
                                 #f)))
                (set-car! library-env
                          (cons (cons (car libs)
                                      libast)
                                (car library-env))))
              (register-libraries (cdr libs) library-env ast-transform))))))
