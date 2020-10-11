(define-library (sunny ast-transforms extract-definitions)
  (export extract-definitions)

  (import (scheme base)
          (sunny ast)
          (sunny variable))

  (begin
    ; This should be the last transform because it assigns the function
    ; body to the definitions' variables. This effectively moves the body out
    ; of the main AST. Further transformations will  probably not be applied to
    ; these functions.
    (define (extract-definitions node)

      (define (transform node transform-children)
        (cond ((eq? (node 'kind) 'DEFINITION)
               (extract-definition node))
              (else (transform-children))))

      (define (extract-definition node)
        (let ((val (node 'get-val)))
          (cond ((eq? 'CLOSURE (val 'kind))
                 (if (not (null? (val 'free-vars)))
                     (error "Definition with free variables" (variable-name (node 'get-var)) (val 'free-vars)))
                 (global-function-set-value!
                   (node 'get-var)
                   (val 'inner-function))
                 (make-nop))
                (else
                  (make-definition (node 'get-var)
                                   val)))))

      (node 'transform transform))))
