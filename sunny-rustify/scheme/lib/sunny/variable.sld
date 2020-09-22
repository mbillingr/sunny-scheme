(define-library (sunny variable)
  (export global-imported?
          global-regular?
          new-boxed
          new-global
          new-import
          new-local
          variable
          variable-add-definition
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
           (car var)))

    (define (variable-add-definition var lambda?)
      (if (variable-mut? var)
          (variable-set-mutable! var)
          (set-car! (cddr var) (if lambda? 'FUNCTION 'VALUE))))

    (define (new-import name)
      (cons name (variable 'IMPORT-REF 'IMPORT-SET #f)))

    (define (new-global name)
      (cons name (variable 'GLOBAL-REF 'GLOBAL-SET #f)))

    (define (new-local name)
      (cons name (variable 'LOCAL-REF 'LOCAL-SET #f)))

    (define (new-boxed name)
      (cons name (variable 'BOXED-REF 'BOXED-SET #f)))))
