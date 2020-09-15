(define-library (sunny table)
  (export make-table
          table?
          clone
          parent
          get-field
          set-field!
          fields
          call-method
          run-tests)

  (import (scheme base)
          (sunny testing))

  (begin
    (define TABLE-ID (cons '<table> '()))

    (define (table? obj)
      (and (pair? obj)
           (eq? (car obj) TABLE-ID)))

    (define (make-table)
      (list TABLE-ID #f))

    (define (clone table)
      (list TABLE-ID table))

    (define (parent table)
      (cadr table))

    (define (fields table)
      (cddr table))

    (define (set-fields! table fields)
      (set-cdr! (cdr table) fields))

    (define (get-field table key)
      (let ((entry (assq key (fields table))))
        (cond (entry (cdr entry))
              ((parent table) (get-field (parent table) key))
              (else #f))))

    (define (set-field! table key value)
      (let ((entry (assq key (fields table))))
        (if entry
            (set-cdr! entry value)
            (set-fields!
              table
              (cons (cons key value)
                    (fields table))))))

    (define (call-method table key . args)
      (apply (get-field table key) table args)))

  (begin
    (define (run-tests)
      (testsuite "table tests"

        (testcase "new table"
          (given (t <- (make-table)))
          (then (table? t)))

        (testcase "can't fake tables"
          (given (t <- (list (cons '<table> '()))))
          (then (not (table? t))))

        (testcase "empty table has no parent"
          (given (t <- (make-table)))
          (then (not (parent t))))

        (testcase "cloned table has parent"
          (given (t <- (make-table)))
          (when (s <- (clone t)))
          (then (eq? (parent s) t)))

        (testcase "empty table has no fields"
          (given (t <- (make-table)))
          (when (f <- (fields t)))
          (then (null? f)))

        (testcase "access missing field"
          (given (t <- (make-table)))
          (when (value <- (get-field t 'x)))
          (then (not value)))

        (testcase "insert and retrieve field"
          (given (t <- (make-table)))
          (when (set-field! t 'x 1))
          (then (= (get-field t 'x) 1)))

        (testcase "inherit field from parent"
          (given (t <- (make-table)))
          (when (set-field! t 'x 1)
                (s <- (clone t)))
          (then (= (get-field s 'x) 1)))

        (testcase "setting child field does not affect parent"
          (given (t <- (make-table)))
          (when (set-field! t 'x 1)
                (s <- (clone t))
                (set-field! s 'x 2))
          (then (= (get-field t 'x) 1)))

        (testcase "call unary method"
          (given (t <- (let ((t (make-table)))
                         (set-field! t 'count 0)
                         (set-field! t 'inc (lambda (self)
                                              (set-field! self 'count
                                                (+ 1 (get-field self 'count)))))
                         t)))
          (when (call-method t 'inc))
          (then (= (get-field t 'count) 1)))

        (testcase "call binary method"
          (given (t <- (let ((t (make-table)))
                         (set-field! t 'value 1)
                         (set-field! t 'add (lambda (self other)
                                              (set-field! self 'value
                                                (+ (get-field self 'value)
                                                   (get-field other 'value)))))
                         t)))
          (when (call-method t 'add t))
          (then (= (get-field t 'value) 2)))

        (testcase "big example"
          (given (goblin <- (let ((goblin (make-table)))
                              (set-field! goblin 'health 30)
                              (set-field! goblin 'armor 10)
                              (set-field! goblin 'alive?
                                          (lambda (self)
                                            (> (get-field self 'health) 0)))
                              (set-field! goblin 'take-damage!
                                          (lambda (self amount)
                                            (if (> amount (get-field self 'armor))
                                                (set-field! self 'health
                                                  (- (get-field self 'health)
                                                     (- amount
                                                        (get-field self 'armor)))))))
                              (set-field! goblin 'spawn
                                          (lambda (self)
                                            (clone self)))
                              goblin))
                 (goblin-wizard <- (let ((goblin-wizard (clone goblin)))
                                     (set-field! goblin-wizard 'health 20)
                                     (set-field! goblin-wizard 'armor 0)
                                     goblin-wizard)))
          (when (krog <- (call-method goblin 'spawn))
                (kold <- (call-method goblin 'spawn))
                (vard <- (call-method goblin 'spawn))
                (dega <- (call-method goblin-wizard 'spawn))

                (call-method krog 'take-damage! 15)
                (call-method kold 'take-damage! 30)
                (call-method vard 'take-damage! 45)
                (call-method dega 'take-damage! 20))
          (then (call-method krog 'alive?)
                (call-method kold 'alive?)
                (not (call-method vard 'alive?))
                (not (call-method dega 'alive?))))))))
