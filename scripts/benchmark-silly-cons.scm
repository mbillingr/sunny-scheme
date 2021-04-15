(import (sunny core)
        (sunny time))

(define (cons-a-lot n l)
  (if (= n 0)
      'ok
      (cons-a-lot (- n 1)
                  (cons n l))))

(display
  (report-time
    (timeit (lambda ()
              (cons-a-lot 1000000 '())))))
(newline)
