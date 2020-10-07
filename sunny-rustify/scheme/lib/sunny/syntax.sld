(define-library (sunny syntax)
  (export make-core-env)

  (import (scheme base)
          (scheme cxr)
          (sunny astify)
          (sunny env)
          (sunny scheme-syntax)
          (sunny variable))

  (begin
    (define (make-core-env)
      (list 'GLOBAL-MARKER
            (new-keyword 'and expand-and)
            (new-keyword 'assert expand-assert)
            (new-keyword 'begin expand-begin)
            (new-keyword 'cond expand-cond)
            (new-keyword 'define expand-define)
            (new-keyword 'if expand-if)
            (new-keyword 'lambda expand-lambda)
            (new-keyword 'let expand-let)
            (new-keyword 'let* expand-let*)
            (new-keyword 'letrec expand-letrec)
            (new-keyword 'or expand-or)
            (new-keyword 'quote expand-quote)
            (new-keyword 'set! expand-set!)
            (new-keyword 'testsuite expand-testsuite)))

    (define (expand-and exp env tail?)
      (astify-comment exp
        (astify-and (and-args exp) env tail?)))

    (define (expand-assert exp env tail?)
      (astify-assert (assert-condition exp) env))

    (define (expand-begin exp env tail?)
      (astify-sequence (begin-statements exp) env tail?))

    (define (expand-cond exp env tail?)
      (astify-comment '(cond ...)
        (astify-cond (cond-clauses exp) env tail?)))

    (define (expand-define exp env tail?)
      (astify-comment (cons 'define (definition-signature exp))
        (astify-definition (definition-variable exp) (definition-value exp) env)))

    (define (expand-if exp env tail?)
      (astify-alternative
        (if-condition exp)
        (if-consequence exp)
        (if-alternative exp)
        env tail?))

    (define (expand-lambda exp env tail?)
      (astify-abstraction (lambda-params exp) (lambda-body exp) env))

    (define (expand-let exp env tail?)
      (astify-comment exp
        (astify-application
          (astify-abstraction (let-vars exp) (let-body exp) env)
          (let-args exp)
          env tail?)))

    (define (expand-let* exp env tail?)
      (define (loop bindings)
        (if (null? bindings)
            (cons 'begin (let-body exp))
            (list 'let (list (car bindings)) (loop (cdr bindings)))))
      (astify-comment exp
        (expand-let (loop (let*-bindings exp))
                env tail?)))

    (define (expand-letrec exp env tail?)
      (astify-comment exp
        (expand-let
          (list 'let
                (map (lambda (name) (list name ''*uninitialized*))
                     (let-vars exp))
                (cons 'begin
                      (append (map (lambda (name val) (list 'set! name val))
                                   (let-vars exp)
                                   (let-args exp))
                              (let-body exp))))
          env tail?)))

    (define (expand-or exp env tail?)
      (let ((x1 (make-syntactic-closure env '() (cadr exp)))
            (x2 (make-syntactic-closure env '() (caddr exp))))
        (astify (make-syntactic-closure (make-core-env)
                                        '()
                                        (list (list 'lambda (list 'tmp) (list 'if 'tmp 'tmp x2))
                                              x1))
                env tail?)))

    (define (expand-quote exp env tail?)
      (astify-constant (cadr exp) env))

    (define (expand-set! exp env tail?)
      (astify-assignment (set!-variable exp) (set!-value exp) env))

    (define (expand-testsuite exp env tail?)
      (astify-testsuite (testsuite-name exp)
                        (testsuite-cases exp)
                        env))))
