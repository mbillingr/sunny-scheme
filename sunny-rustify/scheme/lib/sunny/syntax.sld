(define-library (sunny syntax)
  (export make-core-env)

  (import (scheme base)
          (scheme write)
          (sunny astify)
          (sunny env)
          (sunny scheme-syntax)
          (sunny variable))

  (begin
    (define (make-core-env)
      (list 'GLOBAL-MARKER
            (new-keyword 'and expand-and)
            (new-keyword 'if expand-if)
            (new-keyword 'quote expand-quote)
            (new-import 'assert-eq)
            (new-import 'assert-equal)))

    (define (expand-and exp env tail?)
      (astify-comment exp
        (astify-and (and-args exp) env tail?)))

    (define (expand-if exp env tail?)
      (astify-alternative
        (if-condition exp)
        (if-consequence exp)
        (if-alternative exp)
        env tail?))

    (define (expand-quote exp env tail?)
      (astify-constant (cadr exp) env))))
