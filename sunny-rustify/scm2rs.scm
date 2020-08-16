(import (scheme base)
        (scheme cxr)
        (scheme read)
        (scheme write))

(define (program->ast exp*)
  (define global-env (make-global-env))
  (define ast (sexpr->sequence exp* global-env #f))
  (make-program (cdr global-env)
                (boxify ast)))

(define (sexpr->ast exp env tail?)
  (if (atom? exp)
      (if (symbol? exp)
          (sexpr->reference exp env)
          (sexpr->constant exp env))
      (cond ((eq? 'set! (car exp)) (sexpr->assignment (cadr exp)
                                                      (caddr exp)
                                                      env))
            ((eq? 'define (car exp)) (wrap-sexpr exp
                                       (sexpr->definition exp env)))
            ((eq? 'lambda (car exp)) (sexpr->abstraction (cadr exp)
                                                         (cddr exp)
                                                         env))
            ((eq? 'begin (car exp)) (sexpr->sequence (cdr exp)
                                                     env tail?))
            ((eq? 'let (car exp)) (wrap-sexpr exp
                                    (sexpr->scope-let (cadr exp)
                                                      (cddr exp)
                                                      env tail?)))
            ((eq? 'let* (car exp)) (wrap-sexpr exp
                                     (sexpr->scope-seq (cadr exp)
                                                       (cddr exp)
                                                       env tail?)))
            ((eq? 'letrec (car exp)) (wrap-sexpr exp
                                       (sexpr->scope-rec (cadr exp)
                                                         (cddr exp)
                                                         env tail?)))
            ((eq? 'if (car exp)) (sexpr-alternative (cadr exp)
                                                    (caddr exp)
                                                    (cadddr exp)
                                                    env tail?))
            (else (wrap-sexpr exp (sexpr->application (car exp)
                                                      (cdr exp)
                                                      env tail?))))))

(define (wrap-sexpr exp node)
  (make-comment exp node))

(define (sexpr->constant exp env)
  (make-constant exp))

(define (sexpr->reference name env)
  (let ((var (ensure-var name env)))
    (make-reference name var)))

(define (sexpr->assignment name exp env)
  (let ((val (sexpr->ast exp env #f))
        (var (ensure-var name env)))
    (variable-set-mutable! var)
    (make-assignment name var val)))

(define (sexpr->definition exp env)
  (let ((name (definition-variable exp))
        (value (definition-value exp)))
    (let ((val (sexpr->ast value env #f))
          (var (ensure-var name env)))
      (make-assignment name var val))))

(define (sexpr-alternative condition consequent alternative env tail?)
  (make-alternative (sexpr->ast condition env #f)
                    (sexpr->ast consequent env tail?)
                    (sexpr->ast alternative env tail?)))

(define (sexpr->application func arg* env tail?)
  (if (and (pair? func)
           (eq? (car func) 'lambda))
      (sexpr->fixlet (cadr func) (cddr func) arg* env tail?)
      (sexpr->regular-application func arg* env tail?)))

(define (sexpr->regular-application func arg* env tail?)
  (let ((func (sexpr->ast func env #f)))
    (let ((args (sexpr->args arg* env)))
      (make-application func args tail?))))

(define (sexpr->fixlet param* body arg* env tail?)
  (let ((local-env (adjoin-local-env param* env)))
    (let ((args      (sexpr->args arg* env))
          (func-body (sexpr->sequence body local-env tail?)))
      (make-fixlet param* func-body args))))

(define (sexpr->args arg* env)
  (if (null? arg*)
      (make-null-arg)
      (make-args (sexpr->ast (car arg*) env #f)
                 (sexpr->args (cdr arg*) env))))

(define (sexpr->scope-seq bindings body env tail?)
  (if (null? bindings)
      (sexpr->sequence body env tail?)
      (sexpr->scope-let (list (car bindings))
                        (list (cons 'let*
                                    (cons (cdr bindings)
                                          body)))
                        env
                        tail?)))


(define (sexpr->scope-rec bindings body env tail?)
  (let* ((params (map (lambda (b) (car b)) bindings))
         (body-env (adjoin-boxed-env params env))
         (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings)))
    (make-scope params
                (sexpr->sequence body body-env tail?)
                args)))

(define (sexpr->scope-let bindings body env tail?)
  (let* ((param* (map (lambda (b) (car b)) bindings))
         (arg* (map (lambda (b) (cadr b)) bindings)))
    (sexpr->fixlet param* body arg* env tail?)))

(define (sexpr->abstraction param* body env)
  (let ((local-env (adjoin-local-env param* env))
        (body (scan-out-defines body)))
    (make-abstraction param*
                      (map (lambda (p) (lookup p local-env)) param*)
                      (sexpr->sequence body local-env #t))))

(define (sexpr->sequence expr* env tail?)
  (if (null? expr*)
      (error "empty sequence"))

  (if (null? (cdr expr*))
      (sexpr->ast (car expr*) env tail?)
      (let ((first (sexpr->ast (car expr*) env #f)))
        (make-sequence first
                       (sexpr->sequence (cdr expr*) env tail?)))))

; ======================================================================
; Syntax

(define (scan-out-defines body)
  (define (initializations exp*)
    (cond ((null? exp*)
           '())
          ((definition? (car exp*))
           (cons (list (definition-variable (car exp*))
                       (definition-value (car exp*)))
                 (initializations (cdr exp*))))
          (else (initializations (cdr exp*)))))
  (define (transform exp*)
    (cond ((null? exp*)
           '())
          ((definition? (car exp*))
           (transform (cdr exp*)))
          (else (cons (car exp*)
                      (transform (cdr exp*))))))
  (list (cons 'letrec
              (cons (initializations body)
                    (transform body)))))


(define (definition? expr)
  (and (pair? expr)
       (eq? (car expr) 'define)))

(define (definition-variable expr)
  (if (pair? (cadr expr))
      (caadr expr)
      (cadr expr)))

(define (definition-value expr)
  (if (pair? (cadr expr))
      (cons 'lambda
            (cons (cdadr expr)
                  (cddr expr)))
      (caddr expr)))


; ======================================================================
; AST


(define (make-comment comment node)
  (define (print)
    (newline)
    (display "; ")
    (write comment)
    (newline)
    (node 'print))
  (define (transform func)
    (func self (lambda () (make-comment comment
                                        (node 'transform func)))))
  (define (free-vars)
    (node 'free-vars))
  (define (gen-rust)
    (newline)
    (display "// ")
    (write comment)
    (newline)
    (node 'gen-rust))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'COMMENT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message COMMENT" msg))))
  self)

(define (make-constant val)
  (define (print)
    `(CONSTANT ,val))
  (define (transform func)
    (func self (lambda () self)))
  (define (free-vars)
    (make-set))
  (define (gen-rust)
    (display "Scm::from(")
    (cond ((eq? val #t) (display "true"))
          ((eq? val #f) (display "false"))
          (else (write val)))
    (display ")"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'CONSTANT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message CONSTANT" msg))))
  self)

(define (make-reference name var)
  (define (global?)
    (or (eq? 'GLOBAL-REF (variable-getter var))
        (eq? 'IMPORT-REF (variable-getter var))))
  (define (print)
    (list (variable-getter var) name))
  (define (transform func)
    (func self (lambda () self)))
  (define (free-vars)
    (if (not (global?))
        (set-add (make-set)
                 name)
        (make-set)))
  (define (gen-rust)
    (let ((getter (variable-getter var)))
      (cond ((eq? 'GLOBAL-REF getter)
             (display "globals::")
             (display (rustify-identifier name))
             (display ".with(|value| value.get())"))
            ((eq? 'IMPORT-REF getter)
             (display (rustify-identifier name))
             (display ".with(|value| value.get())"))
            ((eq? 'BOXED-REF getter)
             (display (rustify-identifier name))
             (display ".get()"))
            (else
              (display (rustify-identifier name))
              (display ".clone()")))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'REFERENCE)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message REFERENCE" msg))))
  self)

(define (make-assignment name var val)
  (define (print)
    (list (variable-setter var) name (val 'print)))
  (define (transform func)
    (func self
          (lambda () (make-assignment name
                                      var
                                      (val 'transform func)))))
  (define (free-vars)
    (set-add (val 'free-vars)
             name))
  (define (gen-rust)
    (let ((setter (variable-setter var)))
      (cond ((eq? 'GLOBAL-SET setter)
             (display "globals::")
             (display (rustify-identifier name))
             (display ".with(|value| value.set(")
             (val 'gen-rust)
             (display "))"))
            ((eq? 'BOXED-SET setter)
             (display (rustify-identifier name))
             (display ".set(")
             (val 'gen-rust)
             (display ")"))
            (else (error "set! on unboxed variable")))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'ASSIGNMENT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ASSIGNMENT" msg))))
  self)

(define (make-alternative condition consequent alternative)
  (define (print)
    (list 'IF (condition 'print) (consequent 'print) (alternative 'print)))
  (define (transform func)
    (func self (lambda ()
                 (make-alternative (condition 'transform func)
                                   (consequent 'transform func)
                                   (alternative 'transform func)))))
  (define (free-vars)
    (set-union
      (set-union (condition 'free-vars)
                 (consequent 'free-vars))
      (alternative 'free-vars)))
  (define (gen-rust)
    (display "if (")
    (condition 'gen-rust)
    (display ").is_true() {")
    (consequent 'gen-rust)
    (display "} else {")
    (alternative 'gen-rust)
    (display "}"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'ALTERNATIVE)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ALTERNATIVE" msg))))
  self)

(define (make-application func args tail?)
  (define (print)
    (cons (if tail? 'APPLY-TC 'APPLY)
          (cons (func 'print)
                (args 'print))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-application
                      (func 'transform fnc)
                      (args 'transform fnc)
                      tail?))))
  (define (free-vars)
    (set-union (func 'free-vars)
               (args 'free-vars)))
  (define (gen-rust)
    (func 'gen-rust)
    (display ".invoke(&[")
    (args 'gen-rust)
    (display "])"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'APPLICATION)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message APPLICATION" msg))))
  self)

(define (make-null-arg)
  (define (print) (list 'NULL-ARG))
  (define (transform fnc) (fnc self (lambda () self)))
  (define (free-vars) (make-set))
  (define (gen-rust) (display ""))

  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'NULL-ARG)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message NULL-ARG" msg))))
  self)

(define (make-args arg next)
  (define (print)
    (cons 'ARG
          (cons arg
                next)))
  (define (transform fnc)
    (fnc self
         (lambda () (make-args (arg 'transform fnc)
                               (next 'transform fnc)))))
  (define (free-vars)
    (set-union (arg 'free-vars)
               (next 'free-vars)))
  (define (gen-rust)
    (arg 'gen-rust)
    (display ", ")
    (next 'gen-rust))

  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'ARG)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ARG" msg))))
  self)

(define (make-fixlet params body args)
  (define (print)
    (cons 'FIXLET
          (cons params
                (cons (args 'print)
                      (body 'print)))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-fixlet
                      params
                      (body 'transform fnc)
                      (args 'transform fnc)))))
  (define (free-vars)
    (set-union (set-remove* (body 'free-vars)
                            params)
               (args 'free-vars)))

  (define (gen-rust)
    (define (gen-params p*)
      (if (pair? p*)
          (begin (display (rustify-identifier (car p*)))
                 (display ", ")
                 (gen-params (cdr p*)))))
    (rust-block
      (lambda ()
        (display "let [")
        (gen-params params)
        (display "] = [")
        (args 'gen-rust)
        (display "];")
        (body 'gen-rust))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'FIXLET)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message FIXLET" msg))))
  self)

(define (make-scope params body args)
  (define (print)
    (cons 'SCOPE
          (cons params
                (cons (args 'print)
                      (body 'print)))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-scope
                      params
                      (body 'transform fnc)
                      (map (lambda (a) (a 'transform fnc)) args)))))
  (define (free-vars-args args)
    (if (null? args)
        (make-set)
        (set-union ((car args) 'free-vars)
                   (free-vars-args (cdr args)))))
  (define (free-vars)
    (set-union (set-remove* (body 'free-vars)
                            params)
               (free-vars-args args)))

  (define (gen-bindings bindings)
    (for-each (lambda (b) (display "let ")
                          (display (rustify-identifier (car b)))
                          (display " = Scm::uninitialized().into_boxed();")
                          (newline))
              bindings)
    (for-each (lambda (b) (display (rustify-identifier (car b)))
                          (display ".set(")
                          ((cdr b) 'gen-rust)
                          (display ");")
                          (newline))
              bindings))
  (define (gen-rust)
    (rust-block
      (lambda ()
        (for-each (lambda (p) (display "let ")
                              (display (rustify-identifier p))
                              (display " = Scm::uninitialized().into_boxed();")
                              (newline))
                  params)
        (for-each (lambda (p a) (display (rustify-identifier p))
                                (display ".set(")
                                (a 'gen-rust)
                                (display ");")
                                (newline))
                  params
                  args)
        (body 'gen-rust))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'SCOPE)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message SCOPE" msg))))
  self)

(define (make-sequence first next)
  (define (print)
    (list 'SEQUENCE (first 'print) (next 'print)))
  (define (transform func)
    (func self
          (lambda ()
            (make-sequence (first 'transform func)
                           (next 'transform func)))))
  (define (free-vars)
    (set-union (first 'free-vars)
               (next 'free-vars)))
  (define (gen-rust-inner)
    (first 'gen-rust)
    (display ";")
    (if (eq? 'SEQUENCE (next 'kind))
        (next 'gen-rust-inner)
        (next 'gen-rust)))
  (define (gen-rust)
    (display "{")
    (gen-rust-inner)
    (display "}"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'SEQUENCE)
          ((eq? 'gen-rust msg) (gen-rust))
          ((eq? 'gen-rust-inner msg) (gen-rust-inner))
          (else (error "Unknown message SEQUENCE" msg))))
  self)

(define (make-abstraction params vars body)
  (define (print)
    (cons 'ABSTRACTION
          (cons params
                (body 'print))))
  (define (transform func)
    (func self
          (lambda ()
            (make-abstraction params (body 'transform func)))))
  (define (free-vars)
    (set-remove* (body 'free-vars)
                 params))
  (define (prepare-closure free-vars)
    (if (pair? free-vars)
        (let ((name (car free-vars)))
          (display "let ")
          (display (rustify-identifier name))
          (display " = ")
          (display (rustify-identifier name))
          (display ".clone();")
          (prepare-closure (cdr free-vars)))))
  (define (gen-rust)
    (define (gen-params p* k)
      (if (pair? p*)
          (begin (display "let ")
                 (display (rustify-identifier (car p*)))
                 (display " = args[")
                 (display k)
                 (display "].clone();")
                 (gen-params (cdr p*) (+ k 1)))))
    (rust-block
      (lambda ()
        (prepare-closure (free-vars))
        (display "Scm::func(move |args: &[Scm]|")
        (rust-block
          (lambda ()
            (display "if args.len() != ")
            (display (length params))
            (display "{panic!(\"invalid arity\")}")
            (gen-params params 0)
            (body 'gen-rust)))
        (display ")"))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'ABSTRACTION)
          ((eq? 'gen-rust msg) (gen-rust))
          ((eq? 'get-params msg) params)
          ((eq? 'get-vars msg) vars)
          ((eq? 'get-body msg) body)
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
                   (display ": Mut<Scm> = Mut::new(Scm::Nil)}")
                   (newline)
                   (gen-global-defs (cdr g))))))
  (define (gen-rust)
    (display "use scheme::write::*;") (newline)
    (display "use scheme::sunny_helpers::*;") (newline)
    (newline)
    (display "mod globals")
    (rust-block (lambda ()
                  (display "use super::*;")
                  (gen-global-defs globals)))
    (newline)
    (newline)
    (display "fn main()")
    (rust-block (lambda ()
                  (newline)
                  (display "println!(\"built with\");")
                  (display "println!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
                  (newline)
                  (body 'gen-rust)
                  (display ";")
                  (newline)))
    (newline))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'kind msg) 'PROGRAM)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message PROGRAM" msg))))
  self)

(define (make-boxify name body)
  (define (print)
    (cons 'BOXIFY (cons name (body 'print))))
  (define (transform func)
    (func self (lambda () (make-boxify name (body 'transform func)))))
  (define (free-vars)
    (body 'free-vars))
  (define (gen-rust)
    (rust-block
      (lambda ()
        (display "let ")
        (display (rustify-identifier name))
        (display " = ")
        (display (rustify-identifier name))
        (display ".into_boxed();")
        (body 'gen-rust))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars))
          ((eq? 'kind msg) 'BOXIFY)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message BOXIFY" msg))))
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

(define (list-find-free-vars seq local-env)
  (if (pair? seq)
      (append ((car seq) 'free-vars local-env)
              (list-find-free-vars (cdr seq) local-env))
      '()))


(define (rust-block code)
  (display "{")
  (code)
  (display "}"))


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
        (new-import 'assert-eq)
        (new-import '=)
        (new-import '+)
        (new-import '-)))

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

(define (adjoin-boxed name env)
  (cons (new-boxed name) env))

(define (adjoin-boxed-env name* env)
  (cond ((null? name*) env)
        ((pair? name*) (adjoin-boxed-env (cdr name*)
                                         (adjoin-boxed (car name*) env)))
        (else (adjoin-boxed name* env))))


(define (new-import name)
  (cons name (variable 'IMPORT-REF 'IMPORT-SET #f)))

(define (new-global name)
  (cons name (variable 'GLOBAL-REF 'GLOBAL-SET #f)))

(define (new-local name)
  (cons name (variable 'LOCAL-REF 'LOCAL-SET #f)))

(define (new-boxed name)
  (cons name (variable 'BOXED-REF 'BOXED-SET #f)))

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
  (eq? 'IMPORT-SET
       (cadr var)))


(define (atom? x)
  (not (pair? x)))


;--------------------------------------------------------------
; AST transformations

(define (boxify node)
  (define (transform node ignore)
    (cond ((eq? (node 'kind) 'ABSTRACTION)
           (boxify-abstraction (node 'get-params)
                               (node 'get-vars)
                               (node 'get-params)
                               (node 'get-vars)
                               (node 'get-body)))
          (else (ignore))))
  (node 'transform transform))


(define (boxify-abstraction params vars param* var* body)
  (if (null? var*)
      (make-abstraction params vars body)
      (if (variable-mut? (car var*))
          (begin (variable-set-setter! (car var*) 'BOXED-SET)
                 (variable-set-getter! (car var*) 'BOXED-REF)
                 (boxify-abstraction params vars (cdr param*) (cdr var*)
                                     (make-boxify (car param*) body)))
          (boxify-abstraction params vars (cdr param*) (cdr var*) body))))

;------------------------------------------------------------
; quick and dirty implementation of sets as a unordered list

(define (make-set)
  '())

(define (set-add set item)
  (cond ((null? set)
         (cons item '()))
        ((eq? (car set) item)
         set)
        (else (cons (car set)
                    (set-add (cdr set) item)))))

(define (set-remove set item)
  (cond ((null? set)
         '())
        ((eq? (car set) item)
         (cdr set))
        (else (cons (car set)
                    (set-remove (cdr set) item)))))

(define (set-add* set item*)
  (set-do* set-add set item*))

(define (set-remove* set item*)
  (set-do* set-remove set item*))

(define (set-do* func set item*)
  (if (null? item*)
      set
      (set-do* func
               (func set (car item*))
               (cdr item*))))

(define (set-union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((set-add* set1 set2))))


;--------------------------------------------------

(define (load-sexpr)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (load-sexpr)))))

(define program (load-sexpr))

(define ast (program->ast program))

(ast 'gen-rust)
