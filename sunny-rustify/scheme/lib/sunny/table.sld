(define-library (sunny table)
  (export make-table
          table?
          run-tests)

  (import (scheme base)
          (sunny testing))

  (begin
    (define TABLE-ID (cons '<table> '()))

    (define (make-table)
      (list TABLE-ID))

    (define (table? obj)
      (and (pair? obj)
           (eq? (car obj) TABLE-ID))))

  (begin
    (define (run-tests)
      (testsuite "table tests"
        (testcase "new table"
          (given (t <- (make-table)))
          (then (table? t)))))))
