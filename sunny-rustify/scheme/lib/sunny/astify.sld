(define-library (sunny astify)
  (export astify-alternative
          astify-and
          astify-comment
          astify-constant
          astify-sequence)

  (import (scheme base)
          (sunny ast)
          (sunny sexpr-ast))

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

    (define astify-comment make-comment)

    (define (astify-constant exp env)
      (make-constant exp))

    (define (astify-sequence exp* env tail?)
      (cond ((null? exp*)
             (error "empty sequence"))
            ((null? (cdr exp*))
             (astify (car exp*) env tail?))
            (else (let* ((first (astify (car exp*) env #f))
                         (rest (astify-sequence (cdr exp*) env tail?)))
                    (make-sequence first rest)))))))
