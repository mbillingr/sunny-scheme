(define-library (sunny testing)
  (export testcase testsuite report-tests)

  (import (scheme base)
          (scheme cxr)
          (scheme write))

  (begin

    (define (testsuite description . testcases)
      (display description)
      (display ": ")
      (run-tests testcases 0 0 '()))

    (define (run-tests case* n-pass n-fail failures)
      (if (null? case*)
          (begin
            (newline)
            (list n-pass n-fail failures))
          (let ((result ((car case*))))
            (if result
                (begin
                  (display "F")
                  (run-tests (cdr case*)
                             n-pass
                             (+ 1 n-fail)
                             (cons result failures)))
                (begin
                  (display ".")
                  (run-tests (cdr case*)
                             (+ 1 n-pass)
                             n-fail
                             failures))))))

    (define (report-tests . result*)
      (define (loop results n-pass n-fail failed)
        (if (null? results)
            (report-testresult (list n-pass n-fail failed))
            (loop (cdr results)
                  (+ n-pass (caar results))
                  (+ n-fail (cadar results))
                  (append failed (caddar results)))))
      (loop result* 0 0 '()))

    (define (report-testresult result)
      (let ((n-pass (car result))
            (n-fail (cadr result))
            (failed (caddr result)))
        (for-each
           (lambda (f)
             (println "  " (car f) ": ")
             (println "    " (cadr f))
             (println "    " (caddr f)))
           failed)
        (if (= 0 n-fail)
            (println "ALL " n-pass " tests passed.")
            (println n-fail " failed, " n-pass " passed."))))

    (define (println . args)
      (define (loop arg*)
        (if (null? arg*)
            (newline)
            (begin
              (display (car arg*))
              (loop (cdr arg*)))))
      (loop args))

    (define-syntax testcase
      (syntax-rules (given when then <-)
        ((testcase description
           (given (var <- val) ...)
           (then (pred arg ...) ...))
         (testcase description
           (given (var <- val) ...)
           (when)
           (then (pred arg ...) ...)))

        ((testcase description
           (given (var <- val) ...)
           (when statement ...)
           (then (pred arg ...) ...))
         (lambda ()
           (let* ((var val) ...)
             (testcase "when" statement ...
               (begin
                 (cond ((not (pred arg ...))
                        `(description (pred arg ...) (pred ,arg ...)))
                       ...
                       (else #f)))))))

        ((testcase "when" body)
         body)

        ((testcase "when" (var <- val) body)
         (let ((var val))
           body))

        ((testcase "when" statement body)
         (begin
           statement
           body))

        ((testcase "when" (var <- val) more ... body)
         (let ((var val))
           (testcase "when" more ... body)))

        ((testcase "when" statement more ... body)
         (begin
           statement
           (testcase "when" more ... body)))))))
