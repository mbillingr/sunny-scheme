(import (scheme base)
        (scheme cxr)
        (scheme read)
        (scheme write))

(define (program->ast exp*)
  (define global-env (make-global-env))
  (define ast (sexpr->sequence exp* global-env #f))
  (make-program (cdr global-env) ast))

(define (sexpr->ast exp env tail?)
  (if (atom? exp)
      (if (symbol? exp)
          (sexpr->reference exp env)
          (sexpr->constant exp env))
      (cond ((eq? 'set! (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env))
            ((eq? 'define (car exp)) (sexpr->definition exp env))
            ((eq? 'lambda (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env))
            ((eq? 'begin (car exp)) (sexpr->sequence (cdr exp) env tail?))
            (else (sexpr->application exp (car exp) (cdr exp) env tail?)))))

(define (sexpr->constant exp env)
  (make-constant exp))

(define (sexpr->reference name env)
  (let ((var (lookup name env)))
    (if (not var)
        (error "Undefined variable" name))
    (make-reference name var)))

(define (sexpr->assignment name exp env)
  (let ((val (sexpr->ast exp env #f))
        (var (lookup name env)))
    (if (not var)
        (error "Undefined variable" name))
    (variable-set-mutable! var)
    (make-assignment #f name var val)))

(define (sexpr->definition exp env)
  (let ((name (if (symbol? (cadr exp))
                  (cadr exp)
                  (caadr exp)))
        (value (if (symbol? (cadr exp))
                   (caddr exp)
                   (cons 'lambda
                         (cons (cdadr exp)
                               (cddr exp))))))
    (let ((val (sexpr->ast value env #f))
          (var (ensure-var name env)))
      (make-assignment exp name var val))))

(define (sexpr->application expr func arg* env tail?)
  (if (and (pair? func)
           (eq? (car func) 'lambda))
      (sexpr->fixlet expr (cadr func) (cddr func) arg* env tail?)
      (sexpr->regular-application expr func arg* env tail?)))

(define (sexpr->regular-application expr func arg* env tail?)
  (let ((func (sexpr->ast func env #f))
        (args (sexpr->args arg* env)))
    (make-application expr func args tail?)))

(define (sexpr->fixlet expr param* body arg* env tail?)
  (let ((local-env (adjoin-local-env param* env)))
    (let ((args (sexpr->args arg* env))
          (func-body (sexpr->sequence body local-env tail?)))
      (make-fixlet expr param* func-body args))))

(define (sexpr->args arg* env)
  (if (null? arg*)
      '()
      (cons (sexpr->ast (car arg*) env #f)
            (sexpr->args (cdr arg*) env))))

(define (sexpr->abstraction param* body env)
  (let ((local-env (adjoin-local-env param* env)))
    (make-abstraction param* (sexpr->sequence body local-env #t))))

(define (sexpr->sequence expr* env tail?)
  (define (convert-all exprs)
    (if (pair? exprs)
        (let ((node (sexpr->ast (car exprs)
                                env
                                (and tail? (null? (cdr exprs))))))
          (cons node (convert-all (cdr exprs))))
        '()))
  (if (= 1 (length expr*))
      (sexpr->ast (car expr*) env tail?)
      (make-sequence (convert-all expr*))))


(define (make-constant val)
  (define (print)
    `(CONSTANT ,val))
  (define (transform func)
    (func self (lambda () self)))
  (define (gen-rust)
    (display "Scm::from(")
    (write val)
    (display ")"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'CONSTANT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message CONSTANT" msg))))
  self)

(define (make-reference name var)
  (define (print)
    (list (variable-getter var) name))
  (define (transform func)
    (func self (lambda () self)))
  (define (gen-rust)
    (let ((getter (variable-getter var)))
      (cond ((eq? 'GLOBAL-REF getter)
             (display (rustify-identifier name))
             (display ".with(|value| value.get())"))
            (else (display (rustify-identifier name))))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'REFERENCE)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message REFERENCE" msg))))
  self)

(define (make-assignment expr name var val)
  (define (print)
    (list (variable-setter var) name (val 'print)))
  (define (transform func)
    (func self
          (lambda () (make-assignment (rustify-identifier name)
                                      var
                                      (val 'transform func)))))
  (define (gen-rust)
    (if expr
        (begin (newline)
               (display "//")
               (write expr)
               (newline)))
    (let ((setter (variable-setter var)))
      (cond ((eq? 'GLOBAL-SET setter)
             (display (rustify-identifier name))
             (display ".with(|value| value.set(")
             (val 'gen-rust)
             (display "))"))
            (else (error "set! not yet implemented for anything other than global vars")))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'ASSIGNMENT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ASSIGNMENT" msg))))
  self)

(define (make-application expr func args tail?)
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
  (define (gen-rust)
    (define (gen-args a*)
      (if (pair? a*)
          (begin ((car a*) 'gen-rust)
                 (display ", ")
                 (gen-args (cdr a*)))))
    (newline)
    (display "//")
    (write expr)
    (newline)
    (func 'gen-rust)
    (display ".invoke(&[")
    (gen-args args)
    (display "])"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'APPLICATION)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message APPLICATION" msg))))
  self)

(define (make-fixlet expr params body args)
  (define (print)
    (cons params
          (cons (print-list args)
                (body 'print))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-fixlet
                      expr params
                      (body 'transform fnc)
                      (transform-list args fnc)))))
  (define (gen-rust)
    (define (gen-args a*)
      (if (pair? a*)
          (begin ((car a*) 'gen-rust)
                 (display ", ")
                 (gen-args (cdr a*)))))
    (define (gen-params p*)
      (if (pair? p*)
          (begin (display (rustify-identifier (car p*)))
                 (display ", ")
                 (gen-params (cdr p*)))))
    (newline)
    (display "//")
    (write expr)
    (newline)

    (display "(|")
    (gen-params params)
    (display "| {")
    (body 'gen-rust)
    (display "})")
    (display "(")
    (gen-args args)
    (display ")"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'FIXLET)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message FIXLET" msg))))
  self)

(define (make-sequence nodes)
  (define (print)
    (cons 'SEQUENCE
          (print-list nodes)))
  (define (transform func)
    (func self
          (lambda ()
            (make-sequence (transform-list nodes func)))))
  (define (gen-rust)
    (define (gen-seq n*)
      (if (pair? n*)
          (begin ((car n*) 'gen-rust)
                 (if (pair? (cdr n*))
                     (begin (display ";")
                            (newline)))
                 (gen-seq (cdr n*)))))
    (display "{")
    (gen-seq nodes)
    (display "}"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'SEQUENCE)
          ((eq? 'gen-rust msg) (gen-rust))
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
  (define (gen-rust)
    (define (gen-params p*)
      (if (pair? p*)
          (begin (display (rustify-identifier (car p*)))
                 (display ", ")
                 (gen-params (cdr p*)))))
    (display "Scm::func(|args: &[Scm]| match args {&[")
    (gen-params params)
    (display "] => {")
    (body 'gen-rust)
    (display "} _ => panic!(\"invalid arity\")})"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'ABSTRACTION)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ABSTRACTION" msg))))
  self)

(define (make-program globals body)
  (define (print)
    (cons 'PROGRAM
          (cons globals
                (body 'print))))
  (define (transform func)
    (func self
          (lambda ()
            (make-program globals (body 'transform func)))))
  (define (gen-global-defs g)
    (if (null? g)
        (newline)
        (if (not (global-imported? (cdar g)))
            (begin (display "thread_local!{#[allow(non_upper_case_globals)] pub static ")
                   (display (rustify-identifier (caar g)))
                   (display ": Cell<Scm> = Cell::new(Scm::Nil)}")
                   (newline)
                   (gen-global-defs (cdr g))))))
  (define (gen-rust)
    (display "use scheme::write::*;")
    (gen-global-defs globals)
    (display "fn main() {")
    (newline)
    (body 'gen-rust)
    (display ";")
    (newline)
    (display "}")
    (newline))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'PROGRAM)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message PROGRAM" msg))))
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


(define (rustify-identifier name)
  (define (char-map ch)
    (cond ((eq? ch #\_) "__")
          ((eq? ch #\?) "_p")
          ((eq? ch #\!) "_i")
          ((eq? ch #\<) "_l_")
          ((eq? ch #\>) "_g_")
          ((eq? ch #\=) "_e_")
          ((eq? ch #\-) "_minus_")
          ((eq? ch #\+) "_plus_")
          ((eq? ch #\*) "_star_")
          ((eq? ch #\/) "_slash_")
          (else (list->string (list ch)))))
  (define (append-all strs)
    (if (null? strs)
        ""
        (string-append (car strs) (append-all (cdr strs)))))
  (append-all (map char-map (string->list (symbol->string name)))))

(define (make-global-env)
  (list 'GLOBAL-MARKER
        (new-import 'display)
        (new-import 'newline)
        (new-import '+)))

(define (ensure-var name env)
  (let ((var (lookup name env)))
    (if var
        var
        (adjoin-global name env))))

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

(define (adjoin-global name env)
  (let ((genv (find-globals env))
        (var (new-global name)))
    (set-cdr! genv (cons var (cdr genv)))
    (cdr var)))

(define (adjoin-local name env)
  (cons (new-local name) env))

(define (adjoin-local-env name* env)
  (cond ((null? name*) env)
        ((pair? name*) (adjoin-local-env (cdr name*)
                                         (adjoin-local (car name*) env)))
        (else (adjoin-local name* env))))


(define (new-import name)
  (cons name (variable 'GLOBAL-REF 'IMPORT-SET #f)))

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

(define (global-imported? var)
  (eq? 'IMPORT-SET
       (cadr var)))


(define (atom? x)
  (not (pair? x)))


(define (load-sexpr)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (load-sexpr)))))

(define program (load-sexpr))

(define ast (program->ast program))

(ast 'gen-rust)
