(define-library (sunny variable)
  (export new-boxed
          new-global
          new-import
          new-local
          variable?
          global-variable?
          import-variable?
          local-variable?
          boxed-variable?
          global-add-definition!
          local-boxify!
          variable-mutable?
          variable-set-mutable!)

  (import (scheme base)
          (scheme cxr)
          (sunny table))

  (begin
    (define Variable (make-table))
    (set-field! Variable 'mut #f)
    (set-field! Variable 'mutable? (lambda (self) (get-field self 'mut)))
    (set-field! Variable 'set-mutable! (lambda (self) (set-field! self 'mut #t)))

    (define GlobalVariable (clone Variable))
    (set-field! GlobalVariable 'status 'UNDEFINED)  ; 'PROC 'VAL 'MUTABLE
    (set-field! GlobalVariable 'mutable? (lambda (self) (eq? 'MUTABLE (get-field self 'status))))
    (set-field! GlobalVariable 'set-mutable! (lambda (self) (set-field! self 'status 'MUTABLE)))
    (set-field! GlobalVariable 'add-definition!
      (lambda (self proc?)
        (if (eq? 'UNDEFINED (get-field self 'status))
            (set-field! self 'status (if proc? 'PROC 'VAL))
            (set-field! self 'status 'MUTABLE))))

    (define ImportedVariable (clone Variable))

    (define LocalVariable (clone Variable))
    (set-field! LocalVariable 'into-boxed! (lambda (self) (set-parent! self BoxedVariable)))

    (define BoxedVariable (clone LocalVariable))

    (define (variable? obj)
      (ancestor? obj Variable))

    (define (global-variable? obj)
      (ancestor? obj GlobalVariable))

    (define (import-variable? obj)
      (ancestor? obj ImportedVariable))

    (define (local-variable? obj)
      (ancestor? obj LocalVariable))

    (define (boxed-variable? obj)
      (ancestor? obj BoxedVariable))

    (define (variable-mutable? var)
      (call-method var 'mutable?))

    (define (variable-set-mutable! var)
      (call-method var 'set-mutable!))

    (define (global-add-definition! var lambda?)
      (call-method var 'add-definition! lambda?))

    (define (local-boxify! var)
      (call-method var 'into-boxed!))

    (define (new-import name)
      (cons name (clone ImportedVariable)))

    (define (new-global name)
      (cons name (clone GlobalVariable)))

    (define (new-local name)
      (cons name (clone LocalVariable)))

    (define (new-boxed name)
      (cons name (clone BoxedVariable)))))
