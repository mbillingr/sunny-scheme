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
          (sunny ast)
          (sunny env)
          (sunny rust codegen)
          (sunny rust module)
          (sunny rust module-tree)
          (sunny rust rustify)
          (sunny sets)
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
