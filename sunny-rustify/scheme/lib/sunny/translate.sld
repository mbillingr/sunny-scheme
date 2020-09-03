(define-library (sunny translate)

  (export scm->ast)

  (import (scheme base)
          (scheme write)
          (only (scheme cxr) caadr
                             cadar
                             caddr
                             cdadr
                             cddar
                             cdddr
                             cadddr)
          (only (scheme read) read)
          (only (scheme file) file-exists?
                              open-input-file
                              open-output-file)
          (chibi filesystem)
          (sunny utils))

  (begin
    (define (scm->ast exp*)
      (if (library? (car exp*))
          (library->ast (library-name (car exp*))
                        (library-decls (car exp*))
                        (list '()))
          (program->ast exp*)))

    (define (program->ast exp*)
      (define global-env (make-global-env))
      (define library-env (list '()))

      (define (process-imports exp* imports init)
        (cond ((import? (car exp*))
               (register-libraries (import-libnames (car exp*))
                                   library-env)
               (process-imports (cdr exp*)
                                (append imports
                                        (sexpr->import (cdar exp*) global-env))
                                (set-add* init (import-libnames (car exp*)))))

              (else (let* ((main (boxify (sexpr->sequence exp* global-env #f)))
                           (globals (sort (lambda (a b)
                                            (string<? (symbol->string (car a))
                                                      (symbol->string (car b))))
                                          (cdr global-env))))
                      (display library-env)
                      (newline)
                      (make-program globals
                                    imports
                                    init
                                    main
                                    (filter cdr
                                            (car library-env)))))))

      (process-imports exp* '() (make-set)))

    (define (library->ast name exp* library-env)
      (library-decls->ast name exp* (make-set) (make-nop) (make-global-env) library-env '() '()))

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


    (define (sexpr->ast exp env tail?)
      (if (atom? exp)
          (if (symbol? exp)
              (sexpr->reference exp env)
              (sexpr->constant exp env))
          (cond ((eq? 'quote (car exp)) (sexpr->constant (cadr exp) env))
                ((eq? 'set! (car exp)) (sexpr->assignment (cadr exp)
                                                          (caddr exp)
                                                          env))
                ((eq? 'define (car exp)) (wrap-sexpr exp
                                           (sexpr->definition exp env)))
                ((eq? 'lambda (car exp)) (sexpr->abstraction (cadr exp)
                                                             (cddr exp)
                                                             env))
                ((eq? 'begin (car exp)) (sexpr->sequence (cdr exp)
                                                         env tail?))
                ((eq? 'let (car exp)) (wrap-sexpr exp
                                        (sexpr->scope-let (cadr exp)
                                                          (cddr exp)
                                                          env tail?)))
                ((eq? 'let* (car exp)) (wrap-sexpr exp
                                         (sexpr->scope-seq (cadr exp)
                                                           (cddr exp)
                                                           env tail?)))
                ((eq? 'letrec (car exp)) (wrap-sexpr exp
                                           (sexpr->scope-rec (cadr exp)
                                                             (cddr exp)
                                                             env tail?)))
                ((eq? 'if (car exp)) (sexpr->alternative (if-condition exp)
                                                         (if-consequence exp)
                                                         (if-alternative exp)
                                                         env tail?))
                ((eq? 'cond (car exp)) (wrap-sexpr exp
                                         (sexpr->cond (cond-clauses exp)
                                                      env tail?)))
                ((eq? 'and (car exp)) (wrap-sexpr exp
                                        (sexpr->and (cdr exp) env tail?)))
                (else (wrap-sexpr exp (sexpr->application (car exp)
                                                          (cdr exp)
                                                          env tail?))))))

    (define (wrap-sexpr exp node)
      (make-comment exp node))

    (define (sexpr->constant exp env)
      (make-constant exp))

    (define (sexpr->reference name env)
      (let ((var (ensure-var! name env)))
        (make-reference name var)))

    (define (sexpr->assignment name exp env)
      (let ((val (sexpr->ast exp env #f))
            (var (ensure-var! name env)))
        (variable-set-mutable! var)
        (make-assignment name var val)))

    (define (sexpr->definition exp env)
      (let* ((name (definition-variable exp))
             (value (definition-value exp))
             (var (ensure-var! name env))
             (val (sexpr->ast value env #f)))
        (make-assignment name var val)))

    (define (sexpr->alternative condition consequent alternative env tail?)
      (let* ((x (sexpr->ast condition env #f))
             (a (sexpr->ast consequent env tail?))
             (b (sexpr->ast alternative env tail?)))
        (make-alternative x a b)))

    (define (sexpr->application func arg* env tail?)
      (if (and (pair? func)
               (eq? (car func) 'lambda))
          (sexpr->fixlet (cadr func) (cddr func) arg* env tail?)
          (sexpr->regular-application func arg* env tail?)))

    (define (sexpr->regular-application func arg* env tail?)
      (let ((func (sexpr->ast func env #f)))
        (let ((args (sexpr->args arg* env)))
          (make-application func args tail?))))

    (define (sexpr->fixlet param* body arg* env tail?)
      (let* ((local-env (adjoin-local-env param* env))
             (args      (sexpr->args arg* env))
             (func-body (sexpr->sequence body local-env tail?)))
        (make-fixlet param* func-body args)))

    (define (sexpr->args arg* env)
      (if (null? arg*)
          (make-null-arg)
          (make-args (sexpr->ast (car arg*) env #f)
                     (sexpr->args (cdr arg*) env))))

    (define (sexpr->scope-seq bindings body env tail?)
      (if (null? bindings)
          (sexpr->sequence body env tail?)
          (sexpr->scope-let (list (car bindings))
                            (list (cons 'let*
                                        (cons (cdr bindings)
                                              body)))
                            env
                            tail?)))


    (define (sexpr->scope-rec bindings body env tail?)
      (let* ((params (map (lambda (b) (car b)) bindings))
             (body-env (adjoin-boxed-env params env))
             (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings)))
        (make-scope params
                    (sexpr->sequence body body-env tail?)
                    args)))

    (define (sexpr->scope-let bindings body env tail?)
      (let* ((param* (map (lambda (b) (car b)) bindings))
             (arg* (map (lambda (b) (cadr b)) bindings)))
        (sexpr->fixlet param* body arg* env tail?)))

    (define (sexpr->abstraction param* body env)
      (let ((local-env (adjoin-local-env param* env))
            (body (scan-out-defines body)))
        (if (dotted-list? param*)
            (make-vararg-abstraction (proper-list-part param*)
                                    (last-cdr param*)
                                    (map (lambda (p) (lookup p local-env)) (proper-list-part param*))
                                    (lookup (last-cdr param*) local-env)
                                    (sexpr->sequence body local-env #t))
            (make-abstraction param*
                              (map (lambda (p) (lookup p local-env)) param*)
                              (sexpr->sequence body local-env #t)))))

    (define (sexpr->sequence expr* env tail?)
      (if (null? expr*)
          (error "empty sequence"))

      (if (null? (cdr expr*))
          (sexpr->ast (car expr*) env tail?)
          (let ((first (sexpr->ast (car expr*) env #f)))
            (make-sequence first
                           (sexpr->sequence (cdr expr*) env tail?)))))

    (define (sexpr->cond clauses env tail?)
      (cond ((null? clauses) (make-constant '*UNSPECIFIED*))
            ((eq? 'else (cond-clause-condition (car clauses)))
             (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?))
            ((pair? clauses)
             (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f))
                    (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?))
                    (rest (sexpr->cond (cdr clauses) env tail?)))
               (make-alternative condition sequence rest)))))

    (define (sexpr->and args env tail?)
      (cond ((null? args) (make-constant #t))
            ((null? (cdr args))
             (sexpr->ast (car args) env tail?))
            (else (make-alternative (sexpr->ast (car args) env #f)
                                    (sexpr->and (cdr args) env tail?)
                                    (make-constant #f)))))


    (define (sexpr->import stmt* env)
      (cond ((null? stmt*)
             '())
            ((eq? 'only (caar stmt*))
             (cons (import-only (cadar stmt*) (cddar stmt*) env)
                   (sexpr->import (cdr stmt*) env)))
            (else (cons (import-all (car stmt*) env)
                        (sexpr->import (cdr stmt*) env)))))

    (define (sexpr->export export-spec* env)
      (cond ((null? export-spec*)
             '())
            (else (cons (make-export env (car export-spec*) (car export-spec*))
                        (sexpr->export (cdr export-spec*) env)))))

    (define (import-all lib env)
      (adjoin-import*!
        (library-exports (library-decls (get-lib lib)))
        env)
      (make-import lib))

    (define (import-only lib names env)
      (check-imports names
                     (library-exports (library-decls (get-lib lib)))
                     lib)
      (adjoin-import*! names env)
      (make-import-only lib names))

    (define (get-lib lib)
      (let ((full-path (find-library
                         '("." "./lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")
                         (library-path lib)
                         '(".sld" ".slx"))))
        (if full-path
            (read (open-input-file full-path))
            (error "Unknown library" lib))))


    (define (find-library base-path* relative-path extension*)
      (if (null? base-path*)
          #f
          (let* ((path (string-append (car base-path*) relative-path))
                 (full-path (find-library-ext path extension*)))
            (if full-path
                full-path
                (find-library (cdr base-path*) relative-path extension*)))))

    (define (find-library-ext path extension*)
      (if (null? extension*)
          #f
          (let ((full-path (string-append path (car extension*))))
            (if (file-exists? full-path)
                full-path
                (find-library-ext path (cdr extension*))))))

    (define (library-path lib)
      (reduce (lambda (left right)
                (string-append left
                               (string-append "/" right)))
              ""
              (map symbol->string lib)))

    (define (check-imports imports exports lib)
      (if (null? imports)
          #t
          (if (memq (car imports) exports)
              (check-imports (cdr imports) exports lib)
              (error "Invalid import" (car imports) lib))))


    ; ======================================================================
    ; Syntax

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
            (else (library-exports (cdr lib-decl*)))))


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
                        (transform body)))))

    ; ======================================================================
    ; AST

    (define (make-comment comment node)
      (define (repr)
        (cons 'COMMENT
              (cons comment
                    (node 'repr))))
      (define (transform func)
        (func self (lambda () (make-comment comment
                                            (node 'transform func)))))
      (define (free-vars)
        (node 'free-vars))
      (define (gen-rust port)
        (newline port)
        (display "// " port)
        (write comment port)
        (newline port)
        (node 'gen-rust port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'COMMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message COMMENT" msg))))
      self)

    (define (make-nop)
      (define (repr) '(NOP))
      (define (transform func) (func self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust port) (display "(/*NOP*/)" port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'NOP)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message NOP" msg))))
      self)

    (define (make-constant val)
      (define (repr)
        (cons 'CONSTANT
              val))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (make-set))
      (define (gen-constant port val)
        (cond ((null? val) (display "Scm::Nil" port))
              ((eq? val #t) (display "Scm::True" port))
              ((eq? val #f) (display "Scm::False" port))
              ((symbol? val) (print port "Scm::symbol(\"" val "\")"))
              ((char? val) (print port "Scm::char('" val "')"))
              ((pair? val) (display "Scm::pair(" port)
                           (gen-constant port (car val))
                           (display ", " port)
                           (gen-constant port (cdr val))
                           (display ")" port))
              (else (display "Scm::from(" port)
                    (write val port)
                    (display ")" port))))
      (define (gen-rust port)
        (gen-constant port val))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'CONSTANT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message CONSTANT" msg))))
      self)

    (define (make-reference name var)
      (define (global?)
        (if (eq? 'GLOBAL-REF (variable-getter var))
            #t
            (eq? 'IMPORT-REF (variable-getter var))))
      (define (repr)
        (list (variable-getter var) name))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (if (global?)
            (make-set)
            (set-add (make-set)
                     name)))
      (define (gen-rust port)
        (let ((getter (variable-getter var)))
          (cond ((eq? 'GLOBAL-REF getter)
                 (print port
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'IMPORT-REF getter)
                 (print port
                        "imports::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'BOXED-REF getter)
                 (print port (rustify-identifier name) ".get()"))
                (else
                  (print port (rustify-identifier name) ".clone()")))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'REFERENCE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message REFERENCE" msg))))
      self)

    (define (make-assignment name var val)
      (define (repr)
        (list (variable-setter var) name (val 'repr)))
      (define (transform func)
        (func self
              (lambda () (make-assignment name
                                          var
                                          (val 'transform func)))))
      (define (free-vars)
        (set-add (val 'free-vars)
                 name))
      (define (gen-rust port)
        (let ((setter (variable-setter var)))
          (cond ((eq? 'GLOBAL-SET setter)
                 (print port
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.set(")
                 (val 'gen-rust port)
                 (print port "))"))
                ((eq? 'BOXED-SET setter)
                 (print port (rustify-identifier name) ".set(")
                 (val 'gen-rust port)
                 (print port ")"))
                (else (error "set! on unboxed variable")))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ASSIGNMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ASSIGNMENT" msg))))
      self)

    (define (make-alternative condition consequent alternative)
      (define (repr)
        (list 'IF (condition 'repr) (consequent 'repr) (alternative 'repr)))
      (define (transform func)
        (func self (lambda ()
                     (make-alternative (condition 'transform func)
                                       (consequent 'transform func)
                                       (alternative 'transform func)))))
      (define (free-vars)
        (set-union
          (set-union (condition 'free-vars)
                     (consequent 'free-vars))
          (alternative 'free-vars)))
      (define (gen-rust port)
        (display "if (" port)
        (condition 'gen-rust port)
        (display ").is_true() {" port)
        (consequent 'gen-rust port)
        (display "} else {" port)
        (alternative 'gen-rust port)
        (display "}" port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ALTERNATIVE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ALTERNATIVE" msg))))
      self)

    (define (make-application func args tail?)
      (define (repr)
        (cons (if tail? 'APPLY-TC 'APPLY)
              (cons (func 'repr)
                    (args 'repr))))
      (define (transform fnc)
        (fnc self
             (lambda () (make-application
                          (func 'transform fnc)
                          (args 'transform fnc)
                          tail?))))
      (define (free-vars)
        (set-union (func 'free-vars)
                   (args 'free-vars)))
      (define (gen-rust port)
        (func 'gen-rust port)
        (display ".invoke(&[" port)
        (args 'gen-rust port)
        (display "])" port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'APPLICATION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message APPLICATION" msg))))
      self)

    (define (make-null-arg)
      (define (repr) (list 'NULL-ARG))
      (define (transform fnc) (fnc self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust port) (display "" port))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'NULL-ARG)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message NULL-ARG" msg))))
      self)

    (define (make-args arg next)
      (define (repr)
        (cons 'ARG
              (cons arg
                    next)))
      (define (transform fnc)
        (fnc self
             (lambda () (make-args (arg 'transform fnc)
                                   (next 'transform fnc)))))
      (define (free-vars)
        (set-union (arg 'free-vars)
                   (next 'free-vars)))
      (define (gen-rust port)
        (arg 'gen-rust port)
        (display ", " port)
        (next 'gen-rust port))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ARG)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ARG" msg))))
      self)

    (define (make-fixlet params body args)
      (define (repr)
        (cons 'FIXLET
              (cons params
                    (cons (args 'repr)
                          (body 'repr)))))
      (define (transform fnc)
        (fnc self
             (lambda () (make-fixlet
                          params
                          (body 'transform fnc)
                          (args 'transform fnc)))))
      (define (free-vars)
        (set-union (set-remove* (body 'free-vars)
                                params)
                   (args 'free-vars)))

      (define (gen-rust port)
        (define (gen-params p*)
          (if (pair? p*)
              (begin (print port (rustify-identifier (car p*)) ", ")
                     (gen-params (cdr p*)))))
        (rust-block port
          (lambda ()
            (display "let [" port)
            (gen-params params)
            (display "] = [" port)
            (args 'gen-rust port)
            (display "];" port)
            (body 'gen-rust port))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'FIXLET)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message FIXLET" msg))))
      self)

    (define (make-scope params body args)
      (define (repr)
        (cons 'SCOPE
              (cons params
                    (cons (args 'repr)
                          (body 'repr)))))
      (define (transform fnc)
        (fnc self
             (lambda () (make-scope
                          params
                          (body 'transform fnc)
                          (map (lambda (a) (a 'transform fnc)) args)))))
      (define (free-vars-args args)
        (if (null? args)
            (make-set)
            (set-union ((car args) 'free-vars)
                       (free-vars-args (cdr args)))))
      (define (free-vars)
        (set-remove* (set-union (body 'free-vars)
                                (free-vars-args args))
                     params))
      (define (gen-rust port)
        (rust-block port
          (lambda ()
            (for-each (lambda (p) (println port
                                    "let "
                                    (rustify-identifier p)
                                    " = Scm::uninitialized().into_boxed();"))
                      params)
            (for-each (lambda (p a) (print port
                                           (rustify-identifier p)
                                           ".set(")
                                    (a 'gen-rust port)
                                    (println port ");"))
                      params
                      args)
            (body 'gen-rust port))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'SCOPE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message SCOPE" msg))))
      self)

    (define (make-sequence first next)
      (define (repr)
        (list 'SEQUENCE (first 'repr) (next 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (make-sequence (first 'transform func)
                               (next 'transform func)))))
      (define (free-vars)
        (set-union (first 'free-vars)
                   (next 'free-vars)))
      (define (gen-rust-inner port)
        (first 'gen-rust port)
        (display ";" port)
        (if (eq? 'SEQUENCE (next 'kind))
            (next 'gen-rust-inner port)
            (next 'gen-rust port)))
      (define (gen-rust port)
        (display "{" port)
        (gen-rust-inner port)
        (display "}" port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'SEQUENCE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'gen-rust-inner msg) (gen-rust-inner (car args)))
              (else (error "Unknown message SEQUENCE" msg))))
      self)

    (define (make-abstraction params vars body)
      (define (repr)
        (cons 'ABSTRACTION
              (cons params
                    (body 'repr))))
      (define (transform func)
        (func self
              (lambda ()
                (make-abstraction params vars (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     params))
      (define (prepare-closure port free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (display "let " port)
              (display (rustify-identifier name) port)
              (display " = " port)
              (display (rustify-identifier name) port)
              (display ".clone();" port)
              (prepare-closure port (cdr free-vars)))))
      (define (gen-rust port)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (display "let " port)
                     (display (rustify-identifier (car p*)) port)
                     (display " = args[" port)
                     (display k port)
                     (display "].clone();" port)
                     (gen-params (cdr p*) (+ k 1)))))
        (rust-block port
          (lambda ()
            (prepare-closure port (free-vars))
            (display "Scm::func(move |args: &[Scm]|" port)
            (rust-block port
              (lambda ()
                (display "if args.len() != " port)
                (display (length params) port)
                (display "{panic!(\"invalid arity\")}" port)
                (gen-params params 0)
                (body 'gen-rust port)))
            (display ")" port))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) params)
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-body msg) body)
              (else (error "Unknown message ABSTRACTION" msg))))
      self)

    (define (make-vararg-abstraction params vararg vars varvar body)
      (define (repr)
        (cons 'VARARG-ABSTRACTION
              (cons params
                    (body 'repr))))
      (define (transform func)
        (func self
              (lambda ()
                (make-vararg-abstraction params vararg vars varvar (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     (cons vararg params)))
      (define (prepare-closure port free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (display "let " port)
              (display (rustify-identifier name) port)
              (display " = " port)
              (display (rustify-identifier name) port)
              (display ".clone();" port)
              (prepare-closure port (cdr free-vars)))))
      (define (gen-rust port)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (display "let " port)
                     (display (rustify-identifier (car p*)) port)
                     (display " = args[" port)
                     (display k port)
                     (display "].clone();" port)
                     (gen-params (cdr p*) (+ k 1)))
              (begin (display "let " port)
                     (display (rustify-identifier vararg) port)
                     (display " = Scm::list(&args[" port)
                     (display k port)
                     (display "..]);" port))))
        (rust-block port
          (lambda ()
            (prepare-closure port (free-vars))
            (display "Scm::func(move |args: &[Scm]|" port)
            (rust-block port
              (lambda ()
                (display "if args.len() < " port)
                (display (length params) port)
                (display "{panic!(\"not enough args\")}" port)
                (gen-params params 0)
                (body 'gen-rust port)))
            (display ")" port))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'VARARG-ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) params)
              ((eq? 'get-vararg msg) vararg)
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-varvar msg) varvar)
              ((eq? 'get-body msg) body)
              (else (error "Unknown message VARARG-ABSTRACTION" msg))))
      self)

    (define (make-program globals imports init body libraries)
      (define (repr)
        (cons 'PROGRAM
              (cons globals
                    (cons imports
                          (body 'repr)))))
      (define (transform func)
        (func self
              (lambda ()
                (make-program globals imports init (body 'transform func)))))
      (define (gen-imports port)
        (for-each (lambda (i)
                    (i 'gen-rust port))
                  imports))
      (define (gen-rust port)
        (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
        (display "mod imports" port)
        (rust-block port
          (lambda ()
            (gen-imports port)))
        (newline port)
        (newline port)
        (display "mod globals" port)
        (rust-block port
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println port "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs port globals)))
        (newline port)
        (newline port)
        (display "pub fn main()" port)
        (rust-block port
          (lambda ()
            (newline port)
            (println port "eprintln!(\"built with\");")
            (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
            (newline port)
            (for-each (lambda (lib)
                        (display "crate::" port)
                        (for-each (lambda (l)
                                    (display (rustify-libname l) port)
                                    (display "::" port))
                                  lib)
                        (display "initialize();" port)
                        (newline port))
                      init)
            (body 'gen-rust port)
            (println port ";")))
        (newline port)
        (rust-gen-modules port libraries))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'PROGRAM)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message PROGRAM" msg))))
      self)

    (define (make-library name globals init body imports exports)
      (define (repr)
        (append 'LIBRARY name exports imports globals (body 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (make-library globals
                              init
                              (body 'transform func)
                              imports
                              exports))))
      (define (gen-exports port exports)
        (for-each (lambda (expo) (expo 'gen-rust port))
                  exports))
      (define (gen-rust port)
        (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
        (display "mod imports" port)
        (rust-block port
          (lambda ()
            (for-each (lambda (i) (i 'gen-rust port))
                      imports)))
        (newline port)
        (newline port)
        (display "pub mod exports" port)
        (rust-block port
          (lambda ()
            (gen-exports port exports)))
        (newline port)
        (newline port)
        (display "mod globals" port)
        (rust-block port
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println port "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs port globals)))
        (newline port)
        (newline port)
        (if (eq? 'NOP (body 'kind))
            (println port "pub fn initialize() {")
            (begin
              (println port "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
              (newline port)
              (println port "pub fn initialize() {")
              (println port "if INITIALIZED.with(|x| x.get()) { return }")
              (println port "INITIALIZED.with(|x| x.set(true));")
              (newline port)))
        (for-each (lambda (lib)
                    (display "crate::" port)
                    (for-each (lambda (l) (print port (rustify-libname l) "::"))
                              lib)
                    (println port "initialize();"))
                  init)
        (body 'gen-rust port)
        (println port ";}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'LIBRARY)
              ((eq? 'libname msg) name)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message LIBRARY" msg))))
      self)

    (define (make-boxify name body)
      (define (repr)
        (cons 'BOXIFY (cons name (body 'repr))))
      (define (transform func)
        (func self (lambda () (make-boxify name (body 'transform func)))))
      (define (free-vars)
        (body 'free-vars))
      (define (gen-rust port)
        (rust-block port
          (lambda ()
            (display "let " port)
            (display (rustify-identifier name port))
            (display " = " port)
            (display (rustify-identifier name port))
            (display ".into_boxed( port);")
            (body 'gen-rust port))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'BOXIFY)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message BOXIFY" msg))))
      self)

    (define (make-export env name exname)
      (define (repr)
        (list 'EXPORT name 'AS exname))
      (define (transform func)
        (func self (lambda () self)))
      (define (gen-rust port)
        (display "pub use super::" port)
        (let ((var (lookup name env)))
          (cond ((not var)
                 (error "undefined export" name))
                ((eq? 'GLOBAL-REF (variable-getter var))
                 (display "globals::" port))
                ((eq? 'IMPORT-REF (variable-getter var))
                 (display "imports::" port))
                (else (error "invalid export variable" var name))))
        (println port
          (rustify-identifier name)
          " as "
          (rustify-identifier exname)
          ";"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'EXPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message EXPORT" msg))))
      self)

    (define (make-import lib)
      (define (repr)
        (cons 'IMPORT lib))
      (define (transform func)
        (func self (lambda () (make-import lib))))
      (define (free-vars)
        (make-set))
      (define (gen-libname port lib)
        (if (null? lib)
            (display "" port)
            (begin (display (rustify-libname (car lib)) port)
                   (if (null? (cdr lib))
                       (display "" port)
                       (display "::" port))
                   (gen-libname port (cdr lib)))))
      (define (gen-rust port)
        (display "pub use crate::" port)
        (gen-libname port lib)
        (display "::exports::*;" port)
        (newline port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'IMPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message IMPORT" msg))))
      self)

    (define (make-import-only lib names)
      (define (repr)
        (cons 'IMPORT-ONLY (cons lib names)))
      (define (transform func)
        (func self (lambda () (make-import-only lib names))))
      (define (free-vars)
        (make-set))
      (define (gen-libname port lib)
        (if (null? lib)
            (display "" port)
            (begin (display (rustify-libname (car lib)) port)
                   (if (null? (cdr lib))
                       (display "" port)
                       (display "::" port))
                   (gen-libname port (cdr lib)))))
      (define (gen-imports port names)
        (if (null? names)
            'DONE
            (begin (display (rustify-identifier (car names)) port)
                   (display ", " port)
                   (gen-imports port (cdr names)))))
      (define (gen-rust port)
        (display "pub use crate::" port)
        (gen-libname port lib)
        (display "::exports::{" port)
        (gen-imports port names)
        (display "};" port)
        (newline port))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'IMPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message IMPORT" msg))))
      self)

    (define (print-list seq)
      (if (pair? seq)
          (cons ((car seq) 'repr)
                (print-list (cdr seq)))
          '()))

    (define (transform-list seq func)
      (if (pair? seq)
          (cons ((car seq) 'transform func)
                (transform-list (cdr seq) func))
          '()))

    (define (list-find-free-vars seq local-env)
      (if (pair? seq)
          (append ((car seq) 'free-vars local-env)
                  (list-find-free-vars (cdr seq) local-env))
          '()))


    (define (rust-gen-global-defs port g)
      (if (null? g)
          (newline port)
          (if (global-imported? (cdar g))
              (rust-gen-global-defs port (cdr g))
              (begin (println port
                       "thread_local!{#[allow(non_upper_case_globals)] pub static "
                       (rustify-identifier (caar g))
                       ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL "
                       (caar g)
                       "\"))}")
                     (rust-gen-global-defs port (cdr g))))))


    (define (rust-gen-modules port libs)
      (let ((module-tree (make-module-tree-node 'root)))
        (for-each (lambda (lib)
                    (module-tree-insert! module-tree (car lib) (cdr lib)))
                  libs)

        (rust-gen-module-tree-list port "." (module-tree-children module-tree))))

    (define (rust-gen-module-tree port path node)
      (println port "pub mod "
                    (rustify-libname (module-tree-name node))
                    " {")
      (if (module-tree-leaf? node)
          (begin
            (create-directory* path)
            (let ((file (open-output-file
                          (string-append
                            path
                            (string-append
                              "/"
                              (string-append
                                (rustify-libname (module-tree-name node))
                                ".rs"))))))
              ((module-tree-libobj node) 'gen-rust file)
              (close-port file)))
          (rust-gen-module-tree-list port
                                     (string-append
                                       (string-append path "/")
                                       (rustify-libname (module-tree-name node)))
                                     (module-tree-children node)))
      (println port "}"))

    (define (rust-gen-module-tree-list port path nodes)
      (create-directory* path)
      (display path) (newline)
      (let ((file (open-output-file
                    (string-append
                      path
                      "/mod.rs"))))
        (close-port file))
      (for-each (lambda (child)
                  (rust-gen-module-tree port path child))
                nodes))


    (define (make-module-tree-node name)
      (cons name '()))

    (define (make-module-tree-leaf name lib)
      (cons name lib))

    (define (module-tree-leaf? node)
      (and (pair? node)
           (symbol? (car node))
           (not (null? (cdr node)))
           (not (pair? (cdr node)))))

    (define (module-tree-name node)
      (car node))

    (define (module-tree-children node)
      (cdr node))

    (define (module-tree-libobj node)
      (cdr node))

    (define (module-tree-set-children! node children)
      (set-cdr! node children))

    (define (module-tree-find-child node name)
      (if (module-tree-leaf? node)
          (error "called (module-tree-find-child) on leaf node" name node))
      (assq name (module-tree-children node)))

    (define (module-tree-append-child! node child)
      (module-tree-set-children!
        node
        (cons child (module-tree-children node))))

    (define (module-tree-insert! tree libname libobj)
      (if (null? libname)
          (error "invalid insert"))
      (let ((child (module-tree-find-child tree (car libname))))
        (if child
            (module-tree-insert! child (cdr libname) libobj)
            (if (null? (cdr libname))
                (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
                (let ((new-node (make-module-tree-node (car libname))))
                  (module-tree-insert! new-node (cdr libname) libobj)
                  (module-tree-append-child! tree new-node))))))





    (define (rust-block port code)
      (display "{" port)
      (code)
      (display "}" port))


    (define (rustify-identifier name)
      (define (char-map ch)
        (cond ((eq? ch #\_) "__")
              ((eq? ch #\?) "_p")
              ((eq? ch #\!) "_i")
              ((eq? ch #\<) "_l_")
              ((eq? ch #\>) "_g_")
              ((eq? ch #\=) "_e_")
              ((eq? ch #\-) "_minus_")
              ((eq? ch #\+) "_plus_")
              ((eq? ch #\*) "_star_")
              ((eq? ch #\/) "_slash_")
              (else (list->string (list ch)))))
      (define (append-all strs)
        (if (null? strs)
            ""
            (string-append (car strs) (append-all (cdr strs)))))
      (cond ((eq? name 'args) "args_")
            ((eq? name 'fn) "fn_")
            ((eq? name 'loop) "loop_")
            ((eq? name 'let) "let_")
            ((eq? name 'mut) "mut_")
            ((eq? name 'ref) "ref_")
            ((eq? name 'self) "self_")
            (else (append-all (map char-map (string->list (symbol->string name)))))))

    (define (rustify-libname name)
      (define (char-map ch)
        (cond ((eq? ch #\_) "__")
              ((eq? ch #\-) "_")
              (else (list->string (list ch)))))
      (define (append-all strs)
        (if (null? strs)
            ""
            (string-append (car strs) (append-all (cdr strs)))))
      (cond ((eq? name 'fn) "fn_")
            (else (append-all (map char-map (string->list (symbol->string name)))))))

    (define (make-global-env)
      (list 'GLOBAL-MARKER
            (new-import 'assert-eq)
            (new-import 'assert-equal)))

    (define (ensure-var! name env)
      (let ((var (lookup name env)))
        (if var
            var
            (adjoin-global! name env))))

    (define (lookup name env)
      (cond ((null? env)
             #f)
            ((eq? 'GLOBAL-MARKER (car env))
             (lookup name (cdr env)))
            ((eq? name (caar env))
             (cdar env))
            (else (lookup name (cdr env)))))

    (define (find-globals env)
      (if (eq? 'GLOBAL-MARKER (car env))
          env
          (find-globals (cdr env))))

    (define (adjoin-global! name env)
      (adjoin-global-var! (new-global name) env))

    (define (adjoin-import! name env)
      (adjoin-global-var! (new-import name) env))

    (define (adjoin-global-var! var env)
      (let ((genv (find-globals env)))
        (set-cdr! genv (cons var (cdr genv)))
        (cdr var)))

    (define (adjoin-local name env)
      (cons (new-local name) env))

    (define (adjoin-local-env name* env)
      (cond ((null? name*) env)
            ((pair? name*) (adjoin-local-env (cdr name*)
                                             (adjoin-local (car name*) env)))
            (else (adjoin-local name* env))))

    (define (adjoin-import*! name* env)
      (define (loop name* genv)
        (if (null? name*)
            '()
            (begin
              (set-cdr! genv (cons (new-import (car name*))
                                   (cdr genv)))
              (loop (cdr name*) genv))))
      (loop name* (find-globals env)))

    (define (adjoin-boxed name env)
      (cons (new-boxed name) env))

    (define (adjoin-boxed-env name* env)
      (cond ((null? name*) env)
            ((pair? name*) (adjoin-boxed-env (cdr name*)
                                             (adjoin-boxed (car name*) env)))
            (else (adjoin-boxed name* env))))


    (define (new-import name)
      (cons name (variable 'IMPORT-REF 'IMPORT-SET #f)))

    (define (new-global name)
      (cons name (variable 'GLOBAL-REF 'GLOBAL-SET #f)))

    (define (new-local name)
      (cons name (variable 'LOCAL-REF 'LOCAL-SET #f)))

    (define (new-boxed name)
      (cons name (variable 'BOXED-REF 'BOXED-SET #f)))

    (define (variable getter setter mut?)
      (list getter setter mut?))

    (define (variable-getter var)
      (car var))

    (define (variable-setter var)
      (cadr var))

    (define (variable-mut? var)
      (caddr var))

    (define (variable-set-mutable! var)
      (set-car! (cddr var) #t))

    (define (variable-set-getter! var getter)
      (set-car! var getter))

    (define (variable-set-setter! var setter)
      (set-car! (cdr var) setter))

    (define (global-imported? var)
      (eq? 'IMPORT-REF
           (car var)))

    (define (global-regular? var)
      (eq? 'GLOBAL-REF
           (car var)))


    (define (atom? x)
      (if (pair? x)
          #f
          #t))


    ;--------------------------------------------------------------
    ; AST transformations

    (define (boxify node)
      (define (transform node ignore)
        (cond ((eq? (node 'kind) 'ABSTRACTION)
               (boxify-abstraction (node 'get-params)
                                   (node 'get-vars)
                                   (node 'get-params)
                                   (node 'get-vars)
                                   (node 'get-body)))
              ((eq? (node 'kind) 'VARARG-ABSTRACTION)
               (boxify-vararg-abstraction (node 'get-params)
                                          (node 'get-vararg)
                                          (node 'get-vars)
                                          (node 'get-varvar)
                                          (cons (node 'get-vararg)
                                                (node 'get-params))
                                          (cons (node 'get-varvar)
                                                (node 'get-vars))
                                          (node 'get-body)))
              (else (ignore))))
      (node 'transform transform))


    (define (boxify-abstraction params vars param* var* body)
      (if (null? var*)
          (make-abstraction params vars body)
          (if (variable-mut? (car var*))
              (begin (variable-set-setter! (car var*) 'BOXED-SET)
                     (variable-set-getter! (car var*) 'BOXED-REF)
                     (boxify-abstraction params vars (cdr param*) (cdr var*)
                                         (make-boxify (car param*) body)))
              (boxify-abstraction params vars (cdr param*) (cdr var*) body))))

    (define (boxify-vararg-abstraction params vararg vars varvar param* var* body)
      (if (null? var*)
          (make-vararg-abstraction params vararg vars varvar body)
          (if (variable-mut? (car var*))
              (begin (variable-set-setter! (car var*) 'BOXED-SET)
                     (variable-set-getter! (car var*) 'BOXED-REF)
                     (boxify-vararg-abstraction params vararg vars varvar
                                                (cdr param*) (cdr var*)
                                                (make-boxify (car param*) body)))
              (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body))))

    ;------------------------------------------------------------
    ; quick and dirty implementation of sets as a unordered list

    (define (make-set)
      '())

    (define (set-add set item)
      (cond ((null? set)
             (cons item '()))
            ((equal? (car set) item)
             set)
            (else (cons (car set)
                        (set-add (cdr set) item)))))

    (define (set-remove set item)
      (cond ((null? set)
             '())
            ((equal? (car set) item)
             (cdr set))
            (else (cons (car set)
                        (set-remove (cdr set) item)))))

    (define (set-add* set item*)
      (set-do* set-add set item*))

    (define (set-remove* set item*)
      (set-do* set-remove set item*))

    (define (set-do* func set item*)
      (if (null? item*)
          set
          (set-do* func
                   (func set (car item*))
                   (cdr item*))))

    (define (set-union set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            (else (set-add* set1 set2))))

    ;------------------------------------------------------------
    ; Utils

    (define (println port . args)
      (for-each (lambda (a) (display a port))
                args)
      (newline port))

    (define (print port . args)
      (for-each (lambda (a) (display a port))
                args))

    (define (writeln port . args)
      (for-each (lambda (a) (write a port))
                args)
      (newline port))

    ;--------------------------------------------------
    ; std library stand-ins

    (define (filter f seq)
      (if (pair? seq)
          (if (f (car seq))
              (cons (car seq)
                    (filter f (cdr seq)))
              (filter f (cdr seq)))
          '()))

    (define (any f seq)
      (if (pair? seq)
          (if (f (car seq))
              #t
              (any f (cdr seq)))
          #f))

    (define (reduce f init seq)
      (if (pair? seq)
          (reduce f (f init (car seq)) (cdr seq))
          init))

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
