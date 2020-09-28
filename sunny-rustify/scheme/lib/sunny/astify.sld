(define-library (sunny astify)
  (export astify-alternative
          astify-constant)

  (import (scheme base)
          (sunny ast)
          (sunny sexpr-ast))

  (begin
    (define astify sexpr->ast)
    
    (define (astify-alternative condition consequent alternative env tail?)
      (make-alternative
        (astify condition env #f)
        (astify consequent env tail?)
        (astify alternative env tail?)))

    (define (astify-constant exp env)
      (make-constant exp))))
