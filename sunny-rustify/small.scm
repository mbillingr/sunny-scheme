(import (scheme base)
        (only (scheme cxr) caadr
                           cadar
                           caddr
                           cdadr
                           cddar
                           cdddr
                           cadddr)
        (only (scheme read) read)
        (only (scheme write) display
                             newline
                             write))

(define (scm->ast exp*)
  (if (library? exp*)
      (library->ast (cddr exp*))
      (program->ast exp*)))

(define (program->ast exp*)
  (define global-env (make-global-env))

  (define (process-imports exp* imports)
    (cond ((import? (car exp*))
           (process-imports (cdr exp*)
                            (append imports
                                    (sexpr->import (cdar exp*) global-env))))

          (else (make-program (cdr global-env)
                              imports
                              (boxify (sexpr->sequence exp* global-env #f))))))

  (process-imports exp* '()))

(define (library->ast exp*)
  (library-decls->ast exp* (make-nop) (make-global-env) '() '()))

(define (library-decls->ast exp* body global-env imports exports)
  (cond ((null? exp*)
         (make-library (cdr global-env) body imports exports))
        ((eq? 'export (caar exp*))
         (library-decls->ast (cdr exp*)
                             body
                             global-env
                             imports
                             (cons (library-export->ast (cdar exp*))
                                   exports)))
        ((import? (car exp*))
         (library-decls->ast (cdr exp*)
                             body
                             global-env
                             (append imports
                                     (sexpr->import (cdar exp*) global-env))
                             exports))
        ((eq? 'begin (caar exp*))
         (library-decls->ast (cdr exp*)
                             (make-sequence body
                                            (sexpr->sequence (cdar exp*)
                                                             global-env #f))
                             imports
                             exports))))


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
            ((eq? 'cond (car exp)) (sexpr->cond (cond-clauses exp)
                                                env tail?))
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
  (let ((name (definition-variable exp))
        (value (definition-value exp)))
    (let ((val (sexpr->ast value env #f))
          (var (ensure-var! name env)))
      (make-assignment name var val))))

(define (sexpr->alternative condition consequent alternative env tail?)
  (make-alternative (sexpr->ast condition env #f)
                    (sexpr->ast consequent env tail?)
                    (sexpr->ast alternative env tail?)))

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
  (let ((local-env (adjoin-local-env param* env)))
    (let ((args      (sexpr->args arg* env))
          (func-body (sexpr->sequence body local-env tail?)))
      (make-fixlet param* func-body args))))

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
    (make-abstraction param*
                      (map (lambda (p) (lookup p local-env)) param*)
                      (sexpr->sequence body local-env #t))))

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
         (make-alternative (sexpr->ast (cond-clause-condition (car clauses)) env #f)
                           (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
                           (sexpr->cond (cdr clauses) env tail?)))))

(define (sexpr->import stmt* env)
  (cond ((null? stmt*)
         '())
        ((eq? 'only (caar stmt*))
         (cons (import-only (cadar stmt*) (cddar stmt*) env)
               (sexpr->import (cdr stmt*) env)))
        (else (cons (import-all (car stmt*) env)
                    (sexpr->import (cdr stmt*) env)))))

(define (import-all lib env)
  (cond ((equal? lib '(scheme base))
         (adjoin-import! '= env)
         (adjoin-import! '> env)
         (adjoin-import! '< env)
         (adjoin-import! '- env)
         (adjoin-import! '+ env)
         (adjoin-import! 'car env)
         (adjoin-import! 'cdr env)
         (adjoin-import! 'cons env)
         (adjoin-import! 'eq? env)
         (adjoin-import! 'null? env)
         (adjoin-import! 'pair? env))
        ((equal? lib '(scheme write))
         (adjoin-import! 'display env)
         (adjoin-import! 'newline env)
         (adjoin-import! 'write env))
        (else (error "unknown library" lib)))
  (make-import lib))

(define (import-only lib names env)
  (adjoin-import*! names env)
  (make-import-only lib names))

; ======================================================================
; Syntax

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
                    (transform body)))))


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
      '*UNSPECIFIED*))


(define (cond-clauses expr)
  (cdr expr))

(define (cond-clause-condition clause)
  (car clause))

(define (cond-clause-sequence clause)
  (cdr clause))


(define (import? expr)
  (and (pair? expr)
       (eq? (car expr) 'import)))


; ======================================================================
; AST


(define (make-nop)
  (define (print) 'NOP)
  (define (transform func) (func self (lambda () self)))
  (define (free-vars) (make-set))
  (define (gen-rust) (display ""))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'NOP)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message NOP" msg))))
  self)
