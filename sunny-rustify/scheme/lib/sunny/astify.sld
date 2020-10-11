(define-library (sunny astify)
  (export astify
          astify-abstraction
          astify-alternative
          astify-and
          astify-application
          astify-assert
          astify-assignment
          astify-comment
          astify-cond
          astify-constant
          astify-definition
          astify-export
          astify-import
          astify-sequence
          astify-symbol
          astify-testsuite
          make-syntactic-closure)

  (import (scheme base)
          (scheme cxr)
          (sunny ast)
          (sunny library)
          (sunny env)
          (sunny scheme-syntax)
          (sunny table)
          (sunny utils)
          (sunny variable))

  (begin
    (define (astify exp env tail?)
      (cond ((syntactic-closure? exp) (astify-syntactic-closure exp env tail?))
            ((pair? exp)
             (let ((f-obj (astify (car exp) env #f)))
               (if (keyword? f-obj)
                   ((keyword-handler f-obj) exp env tail?)
                   (astify-comment exp
                     (astify-application f-obj (cdr exp) env tail?)))))
            ((symbol? exp) (astify-symbol exp env))
            (else (astify-constant exp env))))

    (define (astify-abstraction param* body env)
      (let* ((local-env (adjoin-local-env param* env))
             (body-sexpr (scan-out-defines body))
             (body-ast (astify-sequence body-sexpr local-env #t)))
        (if (dotted-list? param*)
            (let ((fix-param (proper-list-part param*))
                  (var-param (last-cdr param*)))
              (make-vararg-abstraction fix-param
                                         var-param
                                         (lookup* fix-param local-env)
                                         (lookup var-param local-env)
                                         body-ast))
            (make-abstraction param*
                              (lookup* param* local-env)
                              body-ast))))

    (define (astify-alternative condition consequent alternative env tail?)
      (make-alternative
        (astify condition env #f)
        (astify consequent env tail?)
        (astify alternative env tail?)))

    (define (astify-and arg* env tail?)
      (cond ((null? arg*) (make-constant #t))
            ((null? (cdr arg*))
             (astify (car arg*) env tail?))
            (else
             (make-alternative
               (astify (car arg*) env #f)
               (astify-and (cdr arg*) env tail?)
               (astify-constant #f env)))))

    (define (astify-application proc arg* env tail?)
      (cond ((eq? 'ABSTRACTION (proc 'kind))
             (make-fixlet (proc 'get-params)
                          (proc 'get-vars)
                          (astify-args arg* env)
                          (proc 'get-body)))
            ((and (eq? 'REFERENCE (proc 'kind))
                  (bor (import-variable? (proc 'get-var))
                       (global-function? (proc 'get-var))))
             (make-function-application (proc 'get-name)
                                        (proc 'get-var)
                                        (astify-args arg* env)
                                        tail?))
            (else (make-application proc (astify-args arg* env) tail?))))

    (define (astify-args arg* env)
      (if (null? arg*)
          (make-null-arg)
          (make-args (astify (car arg*) env #f)
                     (astify-args (cdr arg*) env))))

    (define (astify-assert cond env)
      (make-assert (astify cond env #f)))

    (define (astify-assignment var-name value env)
      (let ((var (ensure-var! var-name env))
            (val (astify value env #f)))
        (variable-set-mutable! var)
        (make-assignment var val)))

    (define (astify-comment exp node)
      (make-comment exp node))

    (define (astify-cond clause* env tail?)
      (cond ((null? clause*)
             (astify-unspecified))
            ((cond-else-clause? (car clause*))
             (astify-sequence (cond-clause-sequence (car clause*))
                              env tail?))
            (else
              (let* ((i (astify (cond-clause-condition (car clause*)) env #f))
                     (t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))
                     (e (astify-cond (cdr clause*) env tail?)))
                (make-alternative i t e)))))

    (define (astify-constant exp env)
      (make-constant exp))

    (define (astify-definition var-name value env)
      (let ((var (ensure-var! var-name env))
            (val (astify value env #f)))
        (global-add-definition! var val)
        (make-definition var val)))

    (define (astify-export export-spec* env)
      (cond ((null? export-spec*)
             '())
            (else (cons (make-export env (car export-spec*) (car export-spec*))
                        (astify-export (cdr export-spec*) env)))))

    (define (astify-import stmt* env)
      (if (null? stmt*)
          '()
          (let ((libname (importset-libname (car stmt*))))
            (cond ((equal? '(sunny testing) libname)  ; ignore the testing library
                   (astify-import (cdr stmt*) env))
                  ((importset-only? (car stmt*))
                   (cons (astify-import-only libname (importset-only-names (car stmt*)) env)
                         (astify-import (cdr stmt*) env)))
                  (else (cons (astify-import-all libname env)
                              (astify-import (cdr stmt*) env)))))))

    (define (astify-import-all libname env)
      (adjoin-import*!
        (library-exports (library-decls (get-lib libname)))
        env)
      (make-import libname))

    (define (astify-import-only libname names env)
      (check-imports names
                     (library-exports (library-decls (get-lib libname)))
                     libname)
      (adjoin-import*! names env)
      (make-import-only libname names))

    (define (astify-sequence exp* env tail?)
      (cond ((null? exp*)
             (error "empty sequence"))
            ((null? (cdr exp*))
             (astify (car exp*) env tail?))
            (else (let* ((first (astify (car exp*) env #f))
                         (rest (astify-sequence (cdr exp*) env tail?)))
                    (make-sequence first rest)))))

    (define (astify-symbol name env)
      (let ((var (ensure-var! name env)))
        (if (keyword? var)
            var
            (make-reference var))))

    (define (astify-testsuite name cases env)
      (make-testsuite name
                      (map (lambda (c) (astify-testcase c env))
                           cases)))

    (define (astify-testcase case env)
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

      (let ((body (dispatch (testcase-body case) '())))
        (make-testcase (testcase-description case)
                       (astify body env #f))))

    (define (astify-unspecified)
      (make-constant '*UNSPECIFIED*))

    ; --- syntactic closure ---

    (define SyntacticClosure (make-table))
    (set-field! SyntacticClosure '__name__ 'SyntacticClosure)

    (define (make-syntactic-closure env free-names exp)
      (let ((sc (clone SyntacticClosure)))
        (set-field! sc 'closure
          (lambda (free-names-env tail?)
            (astify exp
                    (filter-syntactic-env free-names
                                          free-names-env
                                          env)
                    tail?)))
        sc))

    (define (syntactic-closure? obj)
      (and (table? obj)
           (ancestor? obj SyntacticClosure)))

    (define (astify-syntactic-closure sc env tail?)
      ((get-field sc 'closure) env tail?))

    (define (filter-syntactic-env names names-env else-env)
      (if (null? names)
          else-env
          (cons (env-find (car names) names-env)
                (filter-syntactic-env (cdr names) names-env else-env))))))
