(define-library (sunny arity)
  (import (sunny core))
  (export procedure-arity
          procedure-arity-max
          procedure-arity-min)
  (begin
    (define procedure-arity-min car)
    (define procedure-arity-max cdr)))
