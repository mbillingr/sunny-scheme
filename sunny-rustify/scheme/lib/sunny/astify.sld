(define-library (sunny astify)
  (export astify
          astify-alternative
          astify-and
          astify-assignment
          astify-comment
          astify-cond
          astify-constant
          astify-sequence)

  (import (scheme base)
          (sunny ast)
          (sunny env)
          (sunny scheme-syntax)
          (sunny sexpr-ast)
          (sunny variable))

  (begin
    (define astify sexpr->ast)

    (define (astify-alternative condition consequent alternative env tail?)
      (make-alternative
        (astify condition env #f)
        (astify consequent env tail?)
        (astify alternative env tail?)))

    (define (astify-and arg* env tail?)
      (cond ((null? arg*) (make-constant #t))
            ((null? (cdr arg*))
             (astify (car arg*) env tail?))
            (else
             (make-alternative
               (astify (car arg*) env #f)
               (astify-and (cdr arg*) env tail?)
               (astify-constant #f env)))))

    (define (astify-assignment var-name value env)
      (let ((var (ensure-var! var-name env))
            (val (astify value env #f)))
        (variable-set-mutable! var)
        (make-assignment var-name var val)))

    (define astify-comment make-comment)

    (define (astify-cond clause* env tail?)
      (cond ((null? clause*)
             (astify-unspecified))
            ((cond-else-clause? (car clause*))
             (astify-sequence (cond-clause-sequence (car clause*))
                              env tail?))
            (else
              (let* ((i (astify (cond-clause-condition (car clause*)) env #f))
                     (t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))
                     (e (astify-cond (cdr clause*) env tail?)))
                (make-alternative i t e)))))

    (define (astify-constant exp env)
      (make-constant exp))

    (define (astify-sequence exp* env tail?)
      (cond ((null? exp*)
             (error "empty sequence"))
            ((null? (cdr exp*))
             (astify (car exp*) env tail?))
            (else (let* ((first (astify (car exp*) env #f))
                         (rest (astify-sequence (cdr exp*) env tail?)))
                    (make-sequence first rest)))))

    (define (astify-unspecified)
      (make-constant '*UNSPECIFIED*))))
