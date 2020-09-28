(define-library (sunny translate)

  (export rust-gen-in-module scm->ast)


  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (only (scheme read) read)
          (only (scheme file) file-exists?
                              open-input-file
                              open-output-file)
          (chibi filesystem)
          (sunny ast-transforms boxify)
          (sunny ast)
          (sunny env)
          (sunny library)
          (sunny rust codegen)
          (sunny rust module)
          (sunny rust module-tree)
          (sunny rust rustify)
          (sunny scheme-syntax)
          (sunny sets)
          (sunny sexpr-ast)
          (sunny syntax)
          (sunny utils)
          (sunny variable))

  (begin
    (define (scm->ast exp*)
      (if (library? (car exp*))
          (library->ast (library-name (car exp*))
                        (library-decls (car exp*))
                        (list '()))
          (program->ast exp*)))

    (define (program->ast exp*)
      (define global-env (make-core-env))
      (define library-env (list '()))

      (define (process-imports exp* imports init)
        (cond ((import? (car exp*))
               (register-libraries (import-libnames (car exp*))
                                   library-env)
               (process-imports (cdr exp*)
                                (append imports
                                        (sexpr->import (cdar exp*) global-env))
                                (set-add* init (import-libnames (car exp*)))))

              (else (let* ((ast (sexpr->sequence exp* global-env #f))
                           (main (boxify ast))
                           (globals (sort (lambda (a b)
                                            (string<? (symbol->string (car a))
                                                      (symbol->string (car b))))
                                          (cdr global-env))))
                      (make-program globals
                                    imports
                                    init
                                    main
                                    (filter cdr
                                            (car library-env)))))))

      (process-imports exp* '() (make-set)))

    (define (library->ast name exp* library-env)
      (library-decls->ast name exp* (make-set) (make-nop) (make-core-env) library-env '() '()))

    (define (library-decls->ast name exp* init body global-env library-env imports exports)
      (cond ((null? exp*)
             (make-library name (cdr global-env) init body imports exports))
            ((eq? 'export (caar exp*))
             (library-decls->ast name
                                 (cdr exp*)
                                 init
                                 body
                                 global-env
                                 library-env
                                 imports
                                 (append exports
                                         (sexpr->export (cdar exp*) global-env))))
            ((import? (car exp*))
             (register-libraries (import-libnames (car exp*))
                                 library-env)
             (library-decls->ast name
                                 (cdr exp*)
                                 (set-add* init (import-libnames (car exp*)))
                                 body
                                 global-env
                                 library-env
                                 (append imports
                                         (sexpr->import (cdar exp*) global-env))
                                 exports))
            ((eq? 'begin (caar exp*))
             (library-decls->ast name
                                 (cdr exp*)
                                 init
                                 (make-sequence body
                                                (sexpr->sequence (cdar exp*)
                                                                 global-env #f))
                                 global-env
                                 library-env
                                 imports
                                 exports))))


    (define (register-libraries libs library-env)
      (cond ((null? libs) 'DONE)
            ((equal? '(sunny testing) (car libs))  ; ignore testing library
             (register-libraries (cdr libs) library-env))
            ((assoc (car libs) (car library-env))
             (register-libraries (cdr libs) library-env))
            (else
              (let* ((lib (get-lib (car libs)))
                     (libast (if (library? lib)
                                 (library->ast (library-name lib)
                                               (library-decls lib)
                                               library-env)
                                 #f)))
                (set-car! library-env
                          (cons (cons (car libs)
                                      libast)
                                (car library-env))))
              (register-libraries (cdr libs) library-env))))




    ; ======================================================================
    ; Syntax




    ;--------------------------------------------------
    ; std library stand-ins


    (define (assoc obj seq)
      (if (pair? seq)
          (if (equal? obj (caar seq))
              (car seq)
              (assoc obj (cdr seq)))
          #f))

    (define (append seq-a seq-b)
      (if (pair? seq-a)
          (cons (car seq-a)
                (append (cdr seq-a) seq-b))
          seq-b))

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
          '()))))
