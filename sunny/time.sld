(define-library (sunny time)
  (import (scheme base))
  (import (sunny extra))
  (export timeit)
  (begin
    (define (timeit thunk)
      ((lambda (start)
         (thunk)
         (- (now) start))
       (now))
    )
  )
)
