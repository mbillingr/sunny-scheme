(define-library (sunny variable)
  (export new-boxed
          new-global
          new-import
          new-local
          replace-var!
          variable?
          global-variable?
          import-variable?
          local-variable?
          boxed-variable?
          global-add-definition!
          global-function?
          global-function-get-value
          global-function-set-value!
          local-boxify!
          variable-mutable?
          variable-set-mutable!
          new-keyword
          make-keyword
          keyword?
          keyword-name
          keyword-handler
          undefined-global-variable?
          variable-name
          variable-set-name!)

  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (sunny table))

  (begin
    (define Keyword (make-table))
    (set-field! Keyword '__name__ 'Keyword)

    (define Variable (make-table))
    (set-field! Variable '__name__ 'Variable)
    (set-field! Variable 'mut #f)
    (set-field! Variable 'mutable? (lambda (self) (get-field self 'mut)))
    (set-field! Variable 'set-mutable! (lambda (self) (set-field! self 'mut #t)))

    (define GlobalVariable (clone Variable))
    (set-field! GlobalVariable '__name__ 'GlobalVariable)
    (set-field! GlobalVariable 'mutable? (lambda (self) #t))
    (set-field! GlobalVariable 'add-definition!
      (lambda (self value)
        'IGNORED))

    (define GlobalFunction (clone Variable))
    (set-field! GlobalFunction '__name__ 'GlobalFunction)
    (set-field! GlobalFunction 'add-definition!
      (lambda (self value)
        (set-parent! self GlobalVariable)))

    (define UndefinedGlobal (clone Variable))
    (set-field! UndefinedGlobal '__name__ 'UndefinedGlobal)
    (set-field! UndefinedGlobal 'new
      (lambda (self name)
        (let ((var (clone UndefinedGlobal)))
          (set-field! var 'name name)
          var)))
    (set-field! UndefinedGlobal 'add-definition!
      (lambda (self value)
        (cond ((eq? (value 'kind) 'ABSTRACTION)
               (set-parent! self GlobalFunction)
               (set-field! self 'value value))
              ((eq? (value 'kind) 'VARARG-ABSTRACTION)
               (set-parent! self GlobalFunction)
               (set-field! self 'value value))
              (else
                (set-parent! self GlobalVariable)))))

    (define ImportedVariable (clone Variable))
    (set-field! ImportedVariable '__name__ 'ImportedVariable)
    (set-field! ImportedVariable 'new
      (lambda (self name)
        (let ((var (clone ImportedVariable)))
          (set-field! var 'name name)
          var)))

    (define LocalVariable (clone Variable))
    (set-field! LocalVariable '__name__ 'LocalVariable)
    (set-field! LocalVariable 'into-boxed! (lambda (self) (set-parent! self BoxedVariable)))
    (set-field! LocalVariable 'new
      (lambda (self name)
        (let ((var (clone LocalVariable)))
          (set-field! var 'name name)
          var)))

    (define BoxedVariable (clone LocalVariable))
    (set-field! BoxedVariable '__name__ 'BoxedVariable)
    (set-field! BoxedVariable 'new
      (lambda (self name)
        (let ((var (clone BoxedVariable)))
          (set-field! var 'name name)
          var)))

    (define (variable-name var)
      (let ((name (get-field var 'name)))
        (if (string? name)
            name
            (symbol->string name))))

    (define (variable-set-name! var name)
      (set-field! var 'name name))

    (define (keyword? obj)
      (and (table? obj)
           (ancestor? obj Keyword)))

    (define (make-keyword name handler)
      (let ((keyword (clone Keyword)))
        (set-field! keyword 'name name)
        (set-field! keyword 'handler handler)
        keyword))

    (define (keyword-name kw)
      (get-field kw 'name))

    (define (keyword-handler kw)
      (get-field kw 'handler))

    (define (variable? obj)
      (ancestor? obj Variable))

    (define (global-variable? obj)
      (ancestor? obj GlobalVariable))

    (define (undefined-global-variable? obj)
      (ancestor? obj UndefinedGlobal))

    (define (global-function? obj)
      (ancestor? obj GlobalFunction))

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

    (define (global-add-definition! var val)
      (call-method var 'add-definition! val))

    (define (global-function-get-value var)
      (get-field var 'value))

    (define (global-function-set-value! var val)
      (set-field! var 'value val))

    (define (local-boxify! var)
      (call-method var 'into-boxed!))

    (define (new-keyword name handler)
      (make-keyword name handler))

    (define (new-import name)
      (call-method ImportedVariable 'new name))

    (define (new-global name)
      (call-method UndefinedGlobal 'new name))

    (define (new-local name)
      (call-method LocalVariable 'new name))

    (define (new-boxed name)
      (call-method BoxedVariable 'new name))

    (define (replace-var! var new-var)
      (replace-table! var new-var))))

    ;(define UNIQUE-COUNT 0)
    ;(define (unique-name name)
    ;  (set! UNIQUE-COUNT (+ 1 UNIQUE-COUNT))
    ;  (string-append ((if (symbol? name) (symbol->string name) name))
    ;                 "_"
    ;                 (number->string UNIQUE-COUNT)))))
