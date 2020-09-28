(define-library (sunny syntax)
  (export make-core-env)

  (import (scheme base)
          (scheme write)
          (sunny astify)
          (sunny env)
          (sunny variable))

  (begin
    (define (make-core-env)
      (list 'GLOBAL-MARKER
            (new-keyword 'quote expand-quote)
            (new-import 'assert-eq)
            (new-import 'assert-equal)))

    (define (expand-quote exp env tail?)
      (astify-constant (cadr exp) env))))
