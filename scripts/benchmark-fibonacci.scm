(import (scheme base)
        (sunny time))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(display
  (report-time
    (timeit (lambda ()
              (fib 30)))))
(newline)
