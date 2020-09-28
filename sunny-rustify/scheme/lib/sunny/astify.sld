(define-library (sunny astify)
  (export astify-constant)

  (import (scheme base)
          (sunny ast))

  (begin
    (define (astify-constant exp env)
      (make-constant exp))))
