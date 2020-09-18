(define-library (sunny variable)
  (export global-imported?
          global-regular?
          variable
          variable-getter
          variable-mut?
          variable-set-getter!
          variable-set-mutable!
          variable-set-setter!
          variable-setter)

  (import (scheme base)
          (scheme cxr))

  (begin
    (define (variable getter setter mut?)
      (list getter setter mut?))

    (define (variable-getter var)
      (car var))

    (define (variable-setter var)
      (cadr var))

    (define (variable-mut? var)
      (caddr var))

    (define (variable-set-mutable! var)
      (set-car! (cddr var) #t))

    (define (variable-set-getter! var getter)
      (set-car! var getter))

    (define (variable-set-setter! var setter)
      (set-car! (cdr var) setter))

    (define (global-imported? var)
      (eq? 'IMPORT-REF
           (car var)))

    (define (global-regular? var)
      (eq? 'GLOBAL-REF
           (car var)))))
