(define-library (sunny env)
  (export adjoin-boxed-env
          adjoin-import!
          adjoin-import*!
          adjoin-local-env
          ensure-var!
          lookup
          make-global-env)

  (import (scheme base)
          (sunny variable))

  (begin
    (define (make-global-env)
      (list 'GLOBAL-MARKER
            (new-import 'assert-eq)
            (new-import 'assert-equal)))

    (define (ensure-var! name env)
      (let ((var (lookup name env)))
        (if var
            var
            (adjoin-global! name env))))

    (define (lookup name env)
      (cond ((null? env)
             #f)
            ((eq? 'GLOBAL-MARKER (car env))
             (lookup name (cdr env)))
            ((eq? name (caar env))
             (cdar env))
            (else (lookup name (cdr env)))))

    (define (find-globals env)
      (if (eq? 'GLOBAL-MARKER (car env))
          env
          (find-globals (cdr env))))

    (define (adjoin-global! name env)
      (adjoin-global-var! (new-global name) env))

    (define (adjoin-import! name env)
      (adjoin-global-var! (new-import name) env))

    (define (adjoin-global-var! var env)
      (let ((genv (find-globals env)))
        (set-cdr! genv (cons var (cdr genv)))
        (cdr var)))

    (define (adjoin-local name env)
      (cons (new-local name) env))

    (define (adjoin-local-env name* env)
      (cond ((null? name*) env)
            ((pair? name*) (adjoin-local-env (cdr name*)
                                             (adjoin-local (car name*) env)))
            (else (adjoin-local name* env))))

    (define (adjoin-import*! name* env)
      (define (loop name* genv)
        (if (null? name*)
            '()
            (begin
              (set-cdr! genv (cons (new-import (car name*))
                                   (cdr genv)))
              (loop (cdr name*) genv))))
      (loop name* (find-globals env)))

    (define (adjoin-boxed name env)
      (cons (new-boxed name) env))

    (define (adjoin-boxed-env name* env)
      (cond ((null? name*) env)
            ((pair? name*) (adjoin-boxed-env (cdr name*)
                                             (adjoin-boxed (car name*) env)))
            (else (adjoin-boxed name* env))))))
