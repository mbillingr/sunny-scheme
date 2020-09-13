(import (scheme base)
        (sunny testing))

(testsuite "Scheme Tests"
  (testcase "The empty list"
    (given (x <- '()))
    (then (null? x))))
