(define-library (testsuite)
  (import (scheme base)
          (sunny testing))

  (begin
    (define (run-tests)
      (testsuite "Scheme Tests"
        (testcase "the empty list"
          (given (x <- '()))
          (then (null? x)))

        (testcase "integers"
          (given (x <- 1)
                 (y <- '1))
          (then (= x y)))))))
