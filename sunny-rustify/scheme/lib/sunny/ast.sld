(define-library (sunny ast)
  (export make-comment)

  (import (scheme base)
          (sunny rust module))

  (begin
    (define (make-comment comment node)
      (define (repr)
        (cons 'COMMENT
              (cons comment
                    (node 'repr))))
      (define (transform func)
        (func self (lambda () (make-comment comment
                                            (node 'transform func)))))
      (define (free-vars)
        (node 'free-vars))
      (define (gen-rust module)
        (println module)
        (print module "// ")
        (showln module comment)
        (node 'gen-rust module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'COMMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message COMMENT" msg))))
      self)))
