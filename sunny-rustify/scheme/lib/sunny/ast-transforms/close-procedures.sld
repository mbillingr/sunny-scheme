(define-library (sunny ast-transforms close-procedures)
  (export close-procedures)

  (import (scheme base)
          (sunny ast)
          (sunny variable))

  (begin
    (define (close-procedures node)
      (define (transform node transform-children)
        (cond ((eq? (node 'kind) 'ABSTRACTION)
               (make-closure (transform-children)))
              ((eq? (node 'kind) 'VARARG-ABSTRACTION)
               (make-closure (transform-children)))
              ((eq? (node 'kind) 'CLOSURE)
               node)
              (else (transform-children))))
      (node 'transform transform))))
