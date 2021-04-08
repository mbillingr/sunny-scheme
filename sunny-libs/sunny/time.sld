(define-library (sunny time)
  (import (scheme base)
          (sunny extra))
  (export timeit
          report-time)
  (begin
    (define (timeit thunk)
      ((lambda (start)
         (thunk)
         (- (now) start))
       (now))
    )

    (define (report-time t)
      (define (report t unit)
        (if (>= t 1000)
            (report (/ t 1000) (* unit 1000))
            (begin
              (display t)
              (display (time-unit->string unit))
              (newline))))
      (report t 1))

    (define (time-unit->string unit)
      (if (= unit 1) "µs"
      (if (= unit 1000) "ms"
      (if (= unit 1000000) "s"
      (if (= unit 1000000000) "ks"
          "oo")))))
  )
)
