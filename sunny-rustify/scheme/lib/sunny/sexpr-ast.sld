(define-library (sunny sexpr-ast)
  (export sexpr->ast
          sexpr->export
          sexpr->import
          sexpr->sequence)

  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (sunny ast)
          (sunny env)
          (sunny library)
          (sunny scheme-syntax)
          (sunny utils)
          (sunny variable))

  (begin
    (define (sexpr->ast exp env tail?)
      (cond ((keyword? exp) exp)
            ((ast-node? exp) exp)
            ((pair? exp)
             (cond ((and (eq? 'testsuite (car exp))
                         (not (lookup 'testsuite env)))
                    (sexpr->testsuite (cadr exp) (cddr exp) env))
                   (else
                     (let ((f-obj (sexpr->ast (car exp) env #f)))
                       (if (keyword? f-obj)
                           ((keyword-handler f-obj) exp env tail?)
                           (wrap-sexpr exp
                             (sexpr->application f-obj (cdr exp) env tail?)))))))
            ((symbol? exp) (objectify-symbol exp env))
            (else (sexpr->constant exp env))))

    (define (wrap-sexpr exp node)
      (make-comment exp node))

    (define (sexpr->constant exp env)
      (make-constant exp))

    (define (objectify-symbol name env)
      (let ((var (ensure-var! name env)))
        (if (keyword? var)
            var
            (make-reference name var))))

    (define (sexpr->reference name env)
      (let ((var (ensure-var! name env)))
        (make-reference name var)))

    (define (sexpr->application func arg* env tail?)
      (if (eq? 'ABSTRACTION (func 'kind))
          (sexpr->fixlet (sexpr->ast func env #f) arg* env tail?)
          (sexpr->regular-application func arg* env tail?)))

    (define (sexpr->regular-application func arg* env tail?)
      (let ((args (sexpr->args arg* env)))
        (make-application func args tail?)))

    (define (sexpr->fixlet func arg* env tail?)
      (let* ((args      (sexpr->args arg* env)))
        (make-fixlet (func 'get-params) (func 'get-vars) args (func 'get-body))))

    (define (sexpr->args arg* env)
      (if (null? arg*)
          (make-null-arg)
          (make-args (sexpr->ast (car arg*) env #f)
                     (sexpr->args (cdr arg*) env))))

    (define (sexpr->abstraction param* body env)
      (let ((local-env (adjoin-local-env param* env))
            (body (scan-out-defines body)))
        (if (dotted-list? param*)
            (make-vararg-abstraction (proper-list-part param*
                                       (last-cdr param*)
                                       (map (lambda (p) (lookup p local-env)) (proper-list-part param*))
                                       (lookup (last-cdr param*) local-env)
                                       (sexpr->sequence body local-env #t)))
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

    (define (sexpr->import stmt* env)
      (cond ((null? stmt*)
             '())
            ((equal? '(sunny testing) (car stmt*))  ; ignore the testing library
             (sexpr->import (cdr stmt*) env))
            ((eq? 'only (caar stmt*))
             (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env)
                   (sexpr->import (cdr stmt*) env)))
            (else (cons (sexpr->import-all (car stmt*) env)
                        (sexpr->import (cdr stmt*) env)))))

    (define (sexpr->export export-spec* env)
      (cond ((null? export-spec*)
             '())
            (else (cons (make-export env (car export-spec*) (car export-spec*))
                        (sexpr->export (cdr export-spec*) env)))))

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

    (define (sexpr->import-all lib env)
      (adjoin-import*!
        (library-exports (library-decls (get-lib lib)))
        env)
      (make-import lib))

    (define (sexpr->import-only lib names env)
      (check-imports names
                     (library-exports (library-decls (get-lib lib)))
                     lib)
      (adjoin-import*! names env)
      (make-import-only lib names))))
