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
          (sunny utils)
          (sunny sets)
          (sunny ast)
          (sunny rust module)
          (sunny rust module-tree)
          (sunny rust rustify))

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
                ((and (eq? 'testsuite (car exp))
                      (not (lookup 'testsuite env)))
                 (sexpr->testsuite (cadr exp) (cddr exp) env))
                ((and (eq? 'assert (car exp))
                      (not (lookup 'assert env)))
                 (sexpr->assert (cadr exp) env))
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
            ((equal? '(sunny testing) (car stmt*))  ; ignore the testing library
             (sexpr->import (cdr stmt*) env))
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
                         '("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")
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

    (define (sexpr->testsuite name cases env)
      (make-testsuite
        name
        (map (lambda (case) (sexpr->testcase case env))
             cases)))

    (define (sexpr->testcase case env)
      (define (given stmt body)
        (list 'let*
          (map (lambda (assignment)
                 (list (car assignment)
                       (caddr assignment)))
               (cdr stmt))
          body))

      (define (when stmt body)
        (define (loop stmt*)
          (cond ((null? stmt*)
                 body)
                ((eq? '<- (cadar stmt*))
                 (list
                   'let (list (list (caar stmt*)
                                    (caddar stmt*)))
                   (loop (cdr stmt*))))
                (else
                  (list 'begin (car stmt*) (loop (cdr stmt*))))))
        (loop (cdr stmt)))

      (define (then stmt body)
        (cons 'begin
              (append
                (map (lambda (pred)
                       (list 'assert pred))
                     (cdr stmt))
                body)))

      (define (dispatch section* body)
        (cond ((null? section*)
               body)
              ((eq? 'given (caar section*))
               (given (car section*) (dispatch (cdr section*) body)))
              ((eq? 'when (caar section*))
               (when (car section*) (dispatch (cdr section*) body)))
              ((eq? 'then (caar section*))
               (then (car section*) (dispatch (cdr section*) body)))
              (else (error "invalid testcase"))))

      (let ((body (dispatch (cddr case) '())))
        (make-testcase (cadr case) (sexpr->ast body env #f))))

    (define (sexpr->assert cond env)
      (make-assert (sexpr->ast cond env #f)))


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

    (define (make-constant val)
      (define (repr)
        (cons 'CONSTANT
              val))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (make-set))
      (define (gen-constant module val)
        (cond ((null? val) (print module "Scm::Nil"))
              ((eq? val #t) (print module "Scm::True"))
              ((eq? val #f) (print module "Scm::False"))
              ((symbol? val) (print module "Scm::symbol(\"" val "\")"))
              ((eq? val #\') (print module "Scm::char('\\'')"))
              ((char? val) (print module "Scm::char('" val "')"))
              ((pair? val) (print module "Scm::pair(")
                           (gen-constant module (car val))
                           (print module ", ")
                           (gen-constant module (cdr val))
                           (print module ")"))
              (else (print module "Scm::from(")
                    (show module val)
                    (print module ")"))))
      (define (gen-rust module)
        (gen-constant module val))
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
      (define (gen-rust module)
        (let ((getter (variable-getter var)))
          (cond ((eq? 'GLOBAL-REF getter)
                 (print module
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'IMPORT-REF getter)
                 (print module
                        "imports::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'BOXED-REF getter)
                 (print module (rustify-identifier name) ".get()"))
                (else
                  (print module (rustify-identifier name) ".clone()")))))
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
      (define (gen-rust module)
        (let ((setter (variable-setter var)))
          (cond ((eq? 'GLOBAL-SET setter)
                 (print module
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.set(")
                 (val 'gen-rust module)
                 (print module "))"))
                ((eq? 'BOXED-SET setter)
                 (print module (rustify-identifier name) ".set(")
                 (val 'gen-rust module)
                 (print module ")"))
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
      (define (gen-rust module)
        (print module "if (")
        (condition 'gen-rust module)
        (print module ").is_true() {")
        (consequent 'gen-rust module)
        (print module "} else ")
        (if (eq? (alternative 'kind) 'ALTERNATIVE)
            (alternative 'gen-rust module)
            (begin
              (print module "{")
              (alternative 'gen-rust module)
              (print module "}"))))
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
      (define (gen-rust module)
        (func 'gen-rust module)
        (print module ".invoke(&[")
        (args 'gen-rust module)
        (print module "])"))
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
      (define (gen-rust module) (print module ""))

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
      (define (gen-rust module)
        (arg 'gen-rust module)
        (print module ",")
        (next 'gen-rust module))

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

      (define (gen-rust module)
        (define (gen-params p*)
          (if (pair? p*)
              (begin (print module (rustify-identifier (car p*)) ", ")
                     (gen-params (cdr p*)))))
        (rust-block module
          (lambda ()
            (print module "let [")
            (gen-params params)
            (print module "] = [")
            (args 'gen-rust module)
            (print module "];")
            (body 'gen-rust module))))
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
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (for-each (lambda (p) (println module
                                    "let "
                                    (rustify-identifier p)
                                    " = Scm::uninitialized().into_boxed();"))
                      params)
            (for-each (lambda (p a) (print module
                                           (rustify-identifier p)
                                           ".set(")
                                    (a 'gen-rust module)
                                    (println module ");"))
                      params
                      args)
            (body 'gen-rust module))))
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
      (define (gen-rust-inner module)
        (first 'gen-rust module)
        (print module ";")
        (if (eq? 'SEQUENCE (next 'kind))
            (next 'gen-rust-inner module)
            (next 'gen-rust module)))
      (define (gen-rust module)
        (print module "{")
        (gen-rust-inner module)
        (print module "}"))
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
      (define (prepare-closure module free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (print module "let ")
              (print module (rustify-identifier name))
              (print module " = ")
              (print module (rustify-identifier name))
              (print module ".clone();")
              (prepare-closure module (cdr free-vars)))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let ")
                     (print module (rustify-identifier (car p*)))
                     (print module " = args[")
                     (print module k)
                     (print module "].clone();")
                     (gen-params (cdr p*) (+ k 1)))))
        (rust-block module
          (lambda ()
            (prepare-closure module (free-vars))
            (print module "Scm::func(move |args: &[Scm]|")
            (rust-block module
              (lambda ()
                (print module "if args.len() != ")
                (print module (length params))
                (print module "{panic!(\"invalid arity\")}")
                (gen-params params 0)
                (body 'gen-rust module)))
            (print module ")"))))
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
      (define (prepare-closure module free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (print module "let ")
              (print module (rustify-identifier name))
              (print module " = ")
              (print module (rustify-identifier name))
              (print module ".clone();")
              (prepare-closure module (cdr free-vars)))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let "
                                   (rustify-identifier (car p*))
                                   " = args["
                                   k
                                   "].clone();")
                     (gen-params (cdr p*) (+ k 1)))
              (begin (print module "let "
                                   (rustify-identifier vararg)
                                   " = Scm::list(&args["
                                   k
                                   "..]);"))))
        (rust-block module
          (lambda ()
            (prepare-closure module (free-vars))
            (print module "Scm::func(move |args: &[Scm]|")
            (rust-block module
              (lambda ()
                (print module "if args.len() < "
                              (length params)
                              "{panic!(\"not enough args\")}")
                (gen-params params 0)
                (body 'gen-rust module)))
            (print module ")"))))
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
      (define (gen-imports module)
        (for-each (lambda (i)
                    (i 'gen-rust module))
                  imports))
      (define (gen-rust module)
        (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
        (print module "mod imports")
        (rust-block module
          (lambda ()
            (gen-imports module)))
        (println module)
        (println module)
        (print module "mod globals")
        (rust-block module
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println module "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs module globals)))
        (println module)
        (println module)
        (print module "pub fn main()")
        (rust-block module
          (lambda ()
            (println module)
            (println module "eprintln!(\"built with\");")
            (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
            (println module)
            (for-each (lambda (lib)
                        (print module "crate::")
                        (for-each (lambda (l)
                                    (print module (rustify-libname l))
                                    (print module "::"))
                                  lib)
                        (print module "initialize();")
                        (println module))
                      init)
            (body 'gen-rust module)
            (println module ";")))
        (println module)
        (rust-gen-modules module libraries))
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
      (define (gen-exports module exports)
        (for-each (lambda (expo) (expo 'gen-rust module))
                  exports))
      (define (gen-rust module)
        (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
        (print module "mod imports")
        (rust-block module
          (lambda ()
            (for-each (lambda (i) (i 'gen-rust module))
                      imports)))
        (println module)
        (println module)
        (print module "pub mod exports")
        (rust-block module
          (lambda ()
            (gen-exports module exports)))
        (println module)
        (println module)
        (print module "mod globals")
        (rust-block module
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println module "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs module globals)))
        (println module)
        (println module)
        (if (eq? 'NOP (body 'kind))
            (println module "pub fn initialize() {")
            (begin
              (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
              (println module)
              (println module "pub fn initialize() {")
              (println module "if INITIALIZED.with(|x| x.get()) { return }")
              (println module "INITIALIZED.with(|x| x.set(true));")
              (println module)))
        (for-each (lambda (lib)
                    (print module "crate::")
                    (for-each (lambda (l) (print module (rustify-libname l) "::"))
                              lib)
                    (println module "initialize();"))
                  init)
        (let ((tests (list 'dummy)))
          ((body 'transform (lambda (node ignore)
                              (if (eq? (node 'kind) 'TESTSUITE)
                                  (begin
                                    (set-cdr! tests (cons node (cdr tests)))
                                    (make-constant '*UNSPECIFIED*))
                                  (ignore))))
           'gen-rust module)
          (println module ";}")

          (for-each (lambda (test) (test 'gen-rust module))
                    (cdr tests))))
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
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (print module "let ")
            (print module (rustify-identifier name))
            (print module " = ")
            (print module (rustify-identifier name))
            (print module ".into_boxed();")
            (body 'gen-rust module))))
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
      (define (gen-rust module)
        (print module "pub use super::")
        (let ((var (lookup name env)))
          (cond ((not var)
                 (error "undefined export" name))
                ((eq? 'GLOBAL-REF (variable-getter var))
                 (print module "globals::"))
                ((eq? 'IMPORT-REF (variable-getter var))
                 (print module "imports::"))
                (else (error "invalid export variable" var name))))
        (println module
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
      (define (gen-libname module lib)
        (if (null? lib)
            (print module "")
            (begin (print module (rustify-libname (car lib)))
                   (if (null? (cdr lib))
                       (print module "")
                       (print module "::"))
                   (gen-libname module (cdr lib)))))
      (define (gen-rust module)
        (print module "pub use crate::")
        (gen-libname module lib)
        (print module "::exports::*;")
        (println module))
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
      (define (gen-libname module lib)
        (if (null? lib)
            (print module "")
            (begin (print module (rustify-libname (car lib)))
                   (if (null? (cdr lib))
                       (print module "")
                       (print module "::"))
                   (gen-libname module (cdr lib)))))
      (define (gen-imports module names)
        (if (null? names)
            'DONE
            (begin (print module (rustify-identifier (car names)))
                   (print module ", ")
                   (gen-imports module (cdr names)))))
      (define (gen-rust module)
        (print module "pub use crate::")
        (gen-libname module lib)
        (print module "::exports::{")
        (gen-imports module names)
        (print module "};")
        (println module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'IMPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message IMPORT" msg))))
      self)

    (define (make-testcase description body)
      (define (repr)
        (list 'TESTCASE description body))
      (define (transform func)
        (func self (lambda () (make-testcase description
                                             (body 'transform func)))))
      (define (gen-rust module)
        (println module "#[test]")
        (println module "fn " (rustify-testname description) "() {")
        (println module "super::initialize();")
        (body 'gen-rust module)
        (println module "}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'TESTCASE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message TESTCASE" msg))))
      self)

    (define (make-testsuite name cases)
      (define (repr)
        (list 'TESTSUITE name cases))
      (define (transform func)
         (func self
               (lambda ()
                (make-testsuite name
                                (map (lambda (c) (c 'transform func))
                                     cases)))))
      (define (gen-rust module)
         (println module "#[cfg(test)]")
         (println module "mod tests {")
         (println module "use super::*;")
         (for-each (lambda (c)
                     (c 'gen-rust module))
                   cases)
         (println module "}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'TESTSUITE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message TESTSUITE" msg))))
      self)

    (define (make-assert condition)
      (define (repr)
        (list 'ASSERT condition))
      (define (transform func)
        (func self (lambda () (make-assert (condition 'transform func)))))
      (define (free-vars)
        (condition 'free-vars))
      (define (gen-rust module)
        (print module "assert!(")
        (condition 'gen-rust module)
        (println module ".is_true());"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ASSERT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ASSERT" msg))))
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


    (define (rust-gen-global-defs module g)
      (if (null? g)
          (println module)
          (if (global-imported? (cdar g))
              (rust-gen-global-defs module (cdr g))
              (begin (println module
                       "thread_local!{#[allow(non_upper_case_globals)] pub static "
                       (rustify-identifier (caar g))
                       ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL "
                       (caar g)
                       "\"))}")
                     (rust-gen-global-defs module (cdr g))))))


    (define (rust-gen-modules module libs)
      (let ((module-tree (make-module-tree-node 'root)))
        (for-each (lambda (lib)
                    (module-tree-insert! module-tree (car lib) (cdr lib)))
                  libs)

        (rust-gen-module-tree-list module (module-tree-children module-tree))))

    (define (rust-gen-module-tree module node)
      (println module
        "pub mod " (rustify-libname (module-tree-name node)) ";")
      (if (module-tree-leaf? node)
          (rust-gen-in-submodule (module-tree-name node) module
            (lambda (submod)
              ((module-tree-libobj node) 'gen-rust submod)))
          (rust-gen-in-submodule (module-tree-name node) module
            (lambda (submod)
              (rust-gen-module-tree-list submod (module-tree-children node))))))

    (define (rust-gen-module-tree-list module nodes)
      (for-each (lambda (child)
                  (rust-gen-module-tree module child))
                nodes))


    (define (rust-gen-in-module name base-path body)
      (let ((module (open-module name base-path)))
        (body module)
        (close-module module)))

    (define (rust-gen-in-submodule name parent body)
      (let ((module (open-submodule name parent)))
        (body module)
        (close-module module)))


    (define (rust-block module code)
      (print module "{")
      (code)
      (print module "}"))

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

    ;--------------------------------------------------
    ; std library stand-ins

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
