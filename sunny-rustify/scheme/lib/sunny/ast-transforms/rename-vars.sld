(define-library (sunny ast-transforms rename-vars)
  (export rename-vars)

  (import (scheme base)
          (sunny ast)
          (sunny variable))

  (begin
    (define (rename-vars rename node)
      (define renamed '())

      (define (renamed? var)
        (memq var renamed))

      (define (do-rename! var)
        (if (renamed? var)
            'DONE
            (begin
              (variable-set-name! var (rename (variable-name var) var))
              (set! renamed (cons var renamed)))))

      (define (transform node transform-children)
        (cond ((eq? (node 'kind) 'REFERENCE)
               (do-rename! (node 'get-var)))
              ((eq? (node 'kind) 'ASSIGNMENT)
               (do-rename! (node 'get-var)))
              ((eq? (node 'kind) 'DEFINITION)
               (do-rename! (node 'get-var)))
              ((eq? (node 'kind) 'FN-APPLICATION)
               (do-rename! (node 'get-var)))
              ((eq? (node 'kind) 'EXPORT)
               (do-rename! (node 'get-var)))
              ((eq? (node 'kind) 'FIXLET)
               (for-each do-rename! (node 'get-vars)))
              ((eq? (node 'kind) 'ABSTRACTION)
               (for-each do-rename! (node 'get-vars)))
              ((eq? (node 'kind) 'VARARG-ABSTRACTION)
               (for-each do-rename! (cons (node 'get-varvar)
                                          (node 'get-vars)))))
        (transform-children))

      (node 'transform transform))))
