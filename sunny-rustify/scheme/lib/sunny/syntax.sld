(define-library (sunny syntax)
  (export make-core-env)

  (import (scheme base)
          (scheme write)
          (sunny astify)
          (sunny env)
          (sunny scheme-syntax)
          (sunny sexpr-ast)
          (sunny variable))

  (begin
    (define (make-core-env)
      (list 'GLOBAL-MARKER
            (new-keyword 'and expand-and)
            (new-keyword 'begin expand-begin)
            (new-keyword 'cond expand-cond)
            (new-keyword 'define expand-define)
            (new-keyword 'if expand-if)
            (new-keyword 'lambda expand-lambda)
            (new-keyword 'let expand-let)
            (new-keyword 'let* expand-let*)
            (new-keyword 'quote expand-quote)
            (new-keyword 'set! expand-set!)
            (new-import 'assert-eq)
            (new-import 'assert-equal)))

    (define (expand-and exp env tail?)
      (astify-comment exp
        (astify-and (and-args exp) env tail?)))

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

    (define (expand-quote exp env tail?)
      (astify-constant (cadr exp) env))

    (define (expand-set! exp env tail?)
      (astify-assignment (set!-variable exp) (set!-value exp) env))))
