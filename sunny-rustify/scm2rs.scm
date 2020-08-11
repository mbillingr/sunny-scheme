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
    (make-assignment name var val)))

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
    (let ((args (sexpr->args arg* env))
          (func-body (sexpr->sequence body local-env tail?)))
      (make-fixlet param* func-body args))))

(define (sexpr->args arg* env)
  (if (null? arg*)
      '()
      (cons (sexpr->ast (car arg*) env #f)
            (sexpr->args (cdr arg*) env))))

(define (sexpr->scope-seq bindings body env tail?)
  (define body-env '*UNINIT*)
  (define (bindings->ast bindings env)
    (if (pair? bindings)
        (let ((var (caar bindings))
              (val (cadar bindings)))
          (let ((local-env (adjoin-local-env `(,var) env)))
            (cons (cons var (sexpr->ast val env #f))
                  (bindings->ast (cdr bindings)
                                 local-env))))
        (begin (set! body-env env)
               '())))
  (let ((transformed-bindings (bindings->ast bindings env)))
    (make-scope 'seq transformed-bindings
                (sexpr->sequence body body-env tail?))))

(define (sexpr->scope-rec bindings body env tail?)
  (let* ((body-env
           (adjoin-boxed-env
             (map (lambda (b) (car b)) bindings)
             env))
         (transformed-bindings
           (map (lambda (b)
                  (cons (car b)
                        (sexpr->ast (cadr b) body-env #f)))
                bindings)))
    (make-scope 'rec transformed-bindings
                (sexpr->sequence body body-env tail?))))

(define (sexpr->scope-let bindings body env tail?)
  (let* ((param* (map (lambda (b) (car b)) bindings))
         (arg* (map (lambda (b) (cadr b)) bindings)))
    (sexpr->fixlet param* body arg* env tail?)))

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
  (define (free-vars local-env)
    (node 'free-vars local-env))
  (define (gen-rust)
    (newline)
    (display "// ")
    (write comment)
    (newline)
    (node 'gen-rust))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
          ((eq? 'kind msg) 'COMMENT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message COMMENT" msg))))
  self)

(define (make-constant val)
  (define (print)
    `(CONSTANT ,val))
  (define (transform func)
    (func self (lambda () self)))
  (define (free-vars local-env)
    '())
  (define (gen-rust)
    (display "Scm::from(")
    (cond ((eq? val #t) (display "true"))
          ((eq? val #f) (display "false"))
          (else (write val)))
    (display ")"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
          ((eq? 'kind msg) 'CONSTANT)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message CONSTANT" msg))))
  self)

(define (make-reference name var)
  (define (print)
    (list (variable-getter var) name))
  (define (transform func)
    (func self (lambda () self)))
  (define (free-vars local-env)
    (list (cons name var)))
  (define (gen-rust)
    (let ((getter (variable-getter var)))
      (cond ((eq? 'GLOBAL-REF getter)
             (display (rustify-identifier name))
             (display ".with(|value| value.get())"))
            ((eq? 'BOXED-REF getter)
             (display (rustify-identifier name))
             (display ".get()"))
            (else (display (rustify-identifier name))))))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
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
  (define (free-vars local-env)
    (if (memq name local-env)
        (val 'free-vars local-env)
        (cons (cons name var) (val 'free-vars local-env))))
  (define (gen-rust)
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
          ((eq? 'free-vars msg) (free-vars (car args)))
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
  (define (free-vars local-env)
    (append (condition 'free-vars local-env)
            (consequent 'free-vars local-env)
            (alternative 'free-vars local-env)))
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
          ((eq? 'free-vars msg) (free-vars (car args)))
          ((eq? 'kind msg) 'ALTERNATIVE)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message ALTERNATIVE" msg))))
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
  (define (free-vars local-env)
    (append (func 'free-vars local-env)
            (list-find-free-vars args local-env)))
  (define (gen-rust)
    (define (gen-args a*)
      (if (pair? a*)
          (begin ((car a*) 'gen-rust)
                 (display ".clone(), ")
                 (gen-args (cdr a*)))))
    (func 'gen-rust)
    (display ".invoke(&[")
    (gen-args args)
    (display "])"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
          ((eq? 'kind msg) 'APPLICATION)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message APPLICATION" msg))))
  self)

(define (make-fixlet params body args)
  (define (print)
    (cons params
          (cons (print-list args)
                (body 'print))))
  (define (transform fnc)
    (fnc self
         (lambda () (make-fixlet
                      params
                      (body 'transform fnc)
                      (transform-list args fnc)))))
  (define (free-vars local-env)
    (append (list-find-free-vars args local-env)
            (body 'free-vars (append local-env params))))
  (define (gen-rust)
    (define (gen-args a*)
      (if (pair? a*)
          (begin ((car a*) 'gen-rust)
                 (display ", ")
                 (gen-args (cdr a*)))))
    (define (gen-params p*)
      (if (pair? p*)
          (begin (display (rustify-identifier (car p*)))
                 (display ": Scm, ")
                 (gen-params (cdr p*)))))
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
          ((eq? 'free-vars msg) (free-vars (car args)))
          ((eq? 'kind msg) 'FIXLET)
          ((eq? 'gen-rust msg) (gen-rust))
          (else (error "Unknown message FIXLET" msg))))
  self)

(define (make-scope kind bindings body)
  (define (print-bindings bindings)
    (map (lambda (b) (list (car b)
                           ((cdr b) 'print)))
         bindings))
  (define (print)
    (cons 'SCOPE
          (cons kind
                (cons (print-bindings bindings)
                      (body 'print)))))
  (define (transform-bindings bindings fnc)
    (map (lambda (b) (cons (car b)
                           ((cdr b) 'transform fnc)))
         bindings))
  (define (transform fnc)
    (fnc self
         (lambda () (make-scope
                      kind
                      (transform-bindings bindings fnc)
                      (body 'transform fnc)))))
  (define (free-vars-seq bindings local-env)
    (if (null? bindings)
        (body 'free-vars local-env)
        (append (free-vars-seq (cdr bindings) (cons (caar bindings) local-env))
                ((cdar bindings) 'free-vars local-env))))
  (define (free-vars-rec bindings local-env)
    (if (null? bindings)
        (body 'free-vars local-env)
        (append ((cdar bindings) 'free-vars local-env)
                (free-vars-rec (cdr bindings) local-env))))
  (define (free-vars local-env)
    (if (eq? kind 'rec)
        (free-vars-rec bindings (append local-env (map car bindings)))
        (free-vars-seq bindings local-env)))

  (define (gen-bindings-seq bindings)
    (for-each (lambda (b) (display "let ")
                          (display (rustify-identifier (car b)))
                          (display " = ")
                          ((cdr b) 'gen-rust)
                          (display ";")
                          (newline))
              bindings))
  (define (gen-bindings-rec bindings)
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
    (display "{")
    (if (eq? kind 'rec)
        (gen-bindings-rec bindings)
        (gen-bindings-seq bindings))
    (body 'gen-rust)
    (display "}"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
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
  (define (free-vars nodes local-env)
    (if (null? nodes)
        '()
        (append ((car nodes) 'free-vars local-env)
                (free-vars (cdr nodes) local-env))))
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
          ((eq? 'free-vars msg) (free-vars nodes (car args)))
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
  (define (free-vars local-env)
    (body 'free-vars (append local-env params)))
  (define (gen-rust)
    (define (gen-params p*)
      (if (pair? p*)
          (begin (display (rustify-identifier (car p*)))
                 (display ", ")
                 (gen-params (cdr p*)))))
    (newline)
    (display "// free vars:")
    (display (free-vars '()))
    (newline)
    (display "Scm::func(move |args: &[Scm]| match &args {&[")
    (gen-params params)
    (display "] => {")
    (body 'gen-rust)
    (display "} _ => panic!(\"invalid arity\")})"))
  (define (self msg . args)
    (cond ((eq? 'print msg) (print))
          ((eq? 'transform msg) (transform (car args)))
          ((eq? 'free-vars msg) (free-vars (car args)))
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
                   (display ": Mut<Scm> = Mut::new(Scm::Nil)}")
                   (newline)
                   (gen-global-defs (cdr g))))))
  (define (gen-rust)
    (display "use scheme::write::*;")
    (display "use scheme::sunny_helpers::*;")
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

(define (list-find-free-vars seq local-env)
  (if (pair? seq)
      (append ((car seq) 'free-vars local-env)
              (list-find-free-vars (cdr seq) local-env))
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
  (cons name (variable 'GLOBAL-REF 'IMPORT-SET #f)))

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
