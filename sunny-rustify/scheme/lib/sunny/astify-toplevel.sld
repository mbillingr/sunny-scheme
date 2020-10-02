(define-library (sunny astify-toplevel)
  (export astify-library
          astify-program
          astify-toplevel)

  (import (scheme base)
          (scheme cxr)
          (sunny ast)
          (sunny ast-transforms boxify)
          (sunny ast-transforms close-procedures)
          (sunny astify)
          (sunny library)
          (sunny scheme-syntax)
          (sunny sets)
          (sunny syntax)
          (sunny utils))

  (begin
    (define (astify-toplevel exp*)
      (if (library? (car exp*))
          (astify-library (library-name (car exp*))
                          (library-decls (car exp*))
                          (list '()))
          (astify-program exp*)))

    (define (astify-library name exp* library-env)
      (astify-library-decls name exp* (make-set) (make-nop) (make-core-env) library-env '() '()))

    (define (astify-library-decls name exp* init body global-env library-env imports exports)
      (cond ((null? exp*)
             (make-library
               name
               (cdr global-env)
               init
               (boxify (close-procedures body))
               imports
               exports))
            ((eq? 'export (caar exp*))
             (astify-library-decls name
                                   (cdr exp*)
                                   init
                                   body
                                   global-env
                                   library-env
                                   imports
                                   (append exports
                                           (astify-export (cdar exp*) global-env))))
            ((import? (car exp*))
             (register-libraries (import-libnames (car exp*))
                                 library-env)
             (astify-library-decls name
                                   (cdr exp*)
                                   (set-add* init (import-libnames (car exp*)))
                                   body
                                   global-env
                                   library-env
                                   (append imports
                                           (astify-import (cdar exp*) global-env))
                                   exports))
            ((eq? 'begin (caar exp*))
             (astify-library-decls name
                                   (cdr exp*)
                                   init
                                   (make-sequence body
                                                  (astify-sequence (cdar exp*)
                                                                   global-env #f))
                                   global-env
                                   library-env
                                   imports
                                   exports))))

    (define (astify-program exp*)
      (define global-env (make-core-env))
      (define library-env (list '()))

      (define (process-imports exp* imports init)
        (cond ((import? (car exp*))
               (register-libraries (import-libnames (car exp*))
                                   library-env)
               (process-imports (cdr exp*)
                                (append imports
                                        (astify-import (cdar exp*) global-env))
                                (set-add* init (import-libnames (car exp*)))))

              (else (let* ((ast (astify-sequence exp* global-env #f))
                           (main (boxify (close-procedures ast)))
                           (globals (sort (lambda (a b)
                                            (string<? (symbol->string (car a))
                                                      (symbol->string (car b))))
                                          (cdr global-env))))
                      (make-program globals
                                    imports
                                    init
                                    main
                                    (filter cdr (car library-env)))))))

      (process-imports exp* '() (make-set)))

    (define (register-libraries libs library-env)
      (cond ((null? libs) 'DONE)
            ((equal? '(sunny testing) (car libs))  ; ignore testing library
             (register-libraries (cdr libs) library-env))
            ((assoc (car libs) (car library-env))
             (register-libraries (cdr libs) library-env))
            (else
              (let* ((lib (get-lib (car libs)))
                     (libast (if (library? lib)
                                 (astify-library (library-name lib)
                                                 (library-decls lib)
                                                 library-env)
                                 #f)))
                (set-car! library-env
                          (cons (cons (car libs)
                                      libast)
                                (car library-env))))
              (register-libraries (cdr libs) library-env))))))
