(import (scheme base)
        (scheme cxr)
        (scheme read)
        (scheme write))

(define (sexpr->ast exp env tail?)
  (if (atom? exp)
      (if (symbol? exp)
          (sexpr->reference exp env)
          (sexpr->constant exp env))
      (cond ((eq? 'set! (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env))
            ((eq? 'lambda (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env))
            ((eq? 'begin (car exp)) (sexpr->sequence (cdr exp) env tail?))
            (else (sexpr->application (car exp) (cdr exp) env tail?)))))

(define (sexpr->constant exp env)
  (make-constant exp))

(define (sexpr->reference name env)
  (let ((var (lookup name env)))
    (make-reference name var)))

(define (sexpr->assignment name exp env)
  (let ((val (sexpr->ast exp env #f))
        (var (lookup name env)))
    (variable-set-mutable! var)
    (make-assignment name var val)))

(define (sexpr->application func arg* env tail?)
  (let ((func (sexpr->ast func env #f))
        (args (sexpr->args arg* env)))
    (make-application func args tail?)))

(define (sexpr->args arg* env)
  (if (null? arg*)
      '()
      (cons (sexpr->ast (car arg*) env #f)
            (sexpr->args (cdr arg*) env))))

(define (sexpr->abstraction param* body env)
  (let ((local-env (adjoin-local-env param* env)))
    ;(list 'LAMBDA name* (meaning-sequence body local-env #t))))
    (make-abstraction param* (sexpr->sequence body local-env #t))))

(define (sexpr->sequence expr* env tail?)
  (define (convert-all exprs)
    (if (pair? exprs)
        (cons (sexpr->ast (car exprs)
                          env
                          (and tail? (null? (cdr exprs))))
              (convert-all (cdr exprs)))
        '()))
  (if (= 1 (length expr*))
      (sexpr->ast (car expr*) env tail?)
      (make-sequence (convert-all expr*))))


(define (make-constant val)
  (define (print)
    `(CONSTANT ,val))
  (define (transform func)
    (func self (lambda () self)))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'CONSTANT)
          (else (error "Unknown message CONSTANT" msg))))
  self)

(define (make-reference name var)
  (define (print)
    (list (variable-getter var) name))
  (define (transform func)
    (func self (lambda () self)))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'REFERENCE)
          (else (error "Unknown message REFERENCE" msg))))
  self)

(define (make-assignment name var val)
  (define (print)
    (list (variable-setter var) name (val 'print)))
  (define (transform func)
    (func self
          (lambda () (make-assignment name var (val 'transform func)))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'ASSIGNMENT)
          (else (error "Unknown message SET" msg))))
  self)

(define (make-application func args tail?)
  (define (print)
    (cons (if tail? 'APPLY-TC 'APPLY)
          (cons (func 'print)
                (print-list args))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-application
                      (func 'transform fnc)
                      (transform-list args fnc)
                      tail?))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'APPLICATION)
          (else (error "Unknown message APPLICATION" msg))))
  self)

(define (make-sequence nodes)
  (define (print)
    (cons 'SEQUENCE
          (print-list nodes)))
  (define (transform func)
    (func self
          (lambda ()
            (make-sequence (transform-list nodes func)))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'SEQUENCE)
          (else (error "Unknown message SEQUENCE" msg))))
  self)

(define (make-abstraction params body)
  (define (print)
    (cons 'ABSTRACTION
          (cons params
                (body 'print))))
  (define (transform func)
    (func self
          (lambda ()
            (make-abstraction params (body 'transform func)))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'ABSTRACTION)
          (else (error "Unknown message ABSTRACTION" msg))))
  self)

(define (print-list seq)
  (if (pair? seq)
      (cons ((car seq) 'print)
            (print-list (cdr seq)))
      '()))

(define (transform-list seq func)
  (if (pair? seq)
      (cons ((car seq) 'transform func)
            (transform-list (cdr seq) func))
      '()))


(define (lookup name env)
  (cond ((null? env)
         (adjoin-global name))
        ((eq? name (caar env))
         (cdar env))
        (else (lookup name (cdr env)))))

(define (adjoin-global name)
  (let ((var (new-global name)))
    (set! global-env (cons var global-env))
    (cdr var)))

(define (adjoin-local name env)
  (cons (new-local name) env))

(define (adjoin-local-env name* env)
  (cond ((null? name*) env)
        ((pair? name*) (adjoin-local-env (cdr name*)
                                         (adjoin-local (car name*) env)))
        (else (adjoin-local name* env))))


(define (new-global name)
  (cons name (variable 'GLOBAL-REF 'GLOBAL-SET #f)))

(define (new-local name)
  (cons name (variable 'LOCAL-REF 'LOCAL-SET #f)))

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


(define (atom? x)
  (not (pair? x)))


(define (CONSTANT value)
  (display "scm![")
  (write value)
  (display "]"))


(define program (read))

(define global-env (list (new-global 'x) (new-global '+)))

(define ast (sexpr->ast program global-env #t))


(display global-env)
(newline)

(newline)
(display (ast 'print))
(newline)

(define (zero-out ast ignore)
  (display "zero:")
  (display (ast 'kind))
  (newline)
  (if (eq? 'CONSTANT (ast 'kind))
      (make-constant '<zero>)
      (ignore)))
(set! ast (ast 'transform zero-out))

(newline)
(display (ast 'print))
(newline)

(display "=====================")
(newline)
