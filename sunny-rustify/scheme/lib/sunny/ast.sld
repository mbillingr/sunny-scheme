(define-library (sunny ast)
  (export ast-node?
          make-abstraction
          make-alternative
          make-application
          make-args
          make-assert
          make-assignment
          make-boxify
          make-closure
          make-comment
          make-constant
          make-definition
          make-export
          make-fixlet
          make-function-application
          make-import
          make-import-only
          make-library
          make-nop
          make-null-arg
          make-program
          make-reference
          make-sequence
          make-testcase
          make-testsuite
          make-vararg-abstraction
          procedure-node?)

  (import (scheme base)
          (sunny env)
          (sunny sets)
          (sunny rust codegen)
          (sunny rust module)
          (sunny rust rustify)
          (sunny table)
          (sunny utils)
          (sunny variable))

  (begin
    (define (ast-node? obj)
      (procedure? obj))

    (define (procedure-node? obj)
      (if (eq? (obj 'kind) 'ABSTRACTION)
          #t
          (eq? (obj 'kind) 'VARARG-ABSTRACTION)))

    (define (make-comment comment node)
      (define (repr)
        (list 'COMMENT comment (node 'repr)))
      (define (transform func)
        (func self (lambda () (make-comment comment
                                            (node 'transform func)))))
      (define (free-vars)
        (node 'free-vars))
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (println module)
            (print module "// ")
            (showln module comment)
            (node 'gen-rust module))))
      (define (gen-rust-inner module)
        (println module)
        (print module "// ")
        (showln module comment)
        (node 'gen-rust-inner module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'COMMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'gen-rust-inner msg) (gen-rust-inner (car args)))
              ((eq? 'inner msg) node)
              (else (error "Unknown message COMMENT" msg))))
      self)

    (define (make-nop)
      (define (repr) '(NOP))
      (define (transform func) (func self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust module) (print module "(/*NOP*/)"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'NOP)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message NOP" msg))))
      self)

    (define (make-constant val)
      (define (repr)
        (cons 'CONSTANT
              val))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (make-set))
      (define (gen-constant module val)
        (cond ((null? val) (print module "Scm::Nil"))
              ((eq? val #t) (print module "Scm::True"))
              ((eq? val #f) (print module "Scm::False"))
              ((symbol? val) (print module "Scm::symbol(\"" val "\")"))
              ((eq? val #\') (print module "Scm::char_apostrophe()"))
              ((char? val) (print module "Scm::char('" val "')"))
              ((pair? val) (print module "Scm::pair(")
                           (gen-constant module (car val))
                           (print module ", ")
                           (gen-constant module (cdr val))
                           (print module ")"))
              (else (print module "Scm::from(")
                    (show module val)
                    (print module ")"))))
      (define (gen-rust module)
        (gen-constant module val))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'CONSTANT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message CONSTANT" msg))))
      self)

    (define (make-reference var)
      (define (repr)
        (list 'GET (variable-name var)))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (if (bor (global-variable? var)
                 (global-function? var)
                 (import-variable? var))
            (make-set)
            (set-add (make-set)
                     (free-var-name (variable-name var)))))
      (define (gen-rust module)
        (let ((name (variable-name var)))
          (cond ((global-variable? var)
                 (print module
                        name
                        ".with(|value| value.get())"))
                ((global-function? var)
                 (print module
                        "Scm::func("
                        name
                        ")"))
                ((import-variable? var)
                 (print module
                        "Scm::func(imports::"
                        name
                        ")"))
                ((boxed-variable? var)
                 (print module name ".get()"))
                (else
                  (print module name ".clone()")))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'REFERENCE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-name msg) (variable-name var))
              ((eq? 'get-var msg) var)
              (else (error "Unknown message REFERENCE" msg))))
      self)

    (define (make-assignment var val)
      (define (repr)
        (list 'SET! (variable-name var) (val 'repr)))
      (define (transform func)
        (func self
              (lambda () (make-assignment var
                                          (val 'transform func)))))
      (define (free-vars)
        (if (bor (global-variable? var)
                 (global-function? var)
                 (import-variable? var))
            (make-set)
            (set-add (val 'free-vars)
                     (free-var-name (variable-name var)))))
      (define (gen-rust module)
        (let ((name (variable-name var)))
          (cond ((global-variable? var)
                 (print module
                        name
                        ".with(|value| value.set(")
                 (val 'gen-rust module)
                 (print module "));"))
                ((boxed-variable? var)
                 (print module name ".set(")
                 (val 'gen-rust module)
                 (print module ");"))
                (else (error "set! on unboxed variable" name var))))
        (print module "Scm::anything()"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ASSIGNMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-var msg) var)
              (else (error "Unknown message ASSIGNMENT" msg))))
      self)

    (define (make-definition var val)
      (define (repr)
        (list 'DEFINE (variable-name var) (val 'repr)))
      (define (transform func)
        (func self
              (lambda () (make-definition var
                                          (val 'transform func)))))
      (define (free-vars)
        (set-add (val 'free-vars)
                 (free-var-name (variable-name var))))
      (define (gen-rust module)
        (let ((name (variable-name var)))
          (cond ((global-variable? var)
                 (print module
                        name
                        ".with(|value| value.set(")
                 (val 'gen-rust module)
                 (print module "));"))
                ((global-function? var)
                 (print module
                        name
                        ".with(|value| value.set(")
                 (val 'gen-rust module)
                 (print module "));"))
                (else (error "definition! of non-global variable"))))
        (print module "Scm::anything()"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'DEFINITION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-name msg) (variable-name var))
              ((eq? 'get-var msg) var)
              ((eq? 'get-val msg) val)
              (else (error "Unknown message DEFINITION" msg))))
      self)

    (define (make-alternative condition consequent alternative)
      (define (repr)
        (list 'IF (condition 'repr) (consequent 'repr) (alternative 'repr)))
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
      (define (gen-rust module)
        (print module "if (")
        (condition 'gen-rust module)
        (print module ").is_true() {")
        (consequent 'gen-rust module)
        (print module "} else ")
        (if (eq? (alternative 'kind) 'ALTERNATIVE)
            (alternative 'gen-rust module)
            (begin
              (print module "{")
              (alternative 'gen-rust module)
              (print module "}"))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ALTERNATIVE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ALTERNATIVE" msg))))
      self)

    (define (make-sequence first next)
      (define (repr)
        (list 'SEQUENCE (first 'repr) (next 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (let* ((a (first 'transform func))
                       (b (next 'transform func)))
                  (make-sequence a b)))))
      (define (free-vars)
        (set-union (first 'free-vars)
                   (next 'free-vars)))
      (define (gen-rust-inner module)
        (first 'gen-rust module)
        (print module ";")
        (if (eq? 'SEQUENCE (next 'kind))
            (next 'gen-rust-inner module)
            (next 'gen-rust module)))
      (define (gen-rust module)
        (print module "{")
        (gen-rust-inner module)
        (print module "}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'SEQUENCE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'gen-rust-inner msg) (gen-rust-inner (car args)))
              (else (error "Unknown message SEQUENCE" msg))))
      self)

    (define (make-application func args tail?)
      (define (repr)
        (cons (if tail? 'APPLY-TC 'APPLY)
              (cons (func 'repr)
                    (args 'repr))))
      (define (transform fnc)
        (fnc self
             (lambda () (make-application
                          (func 'transform fnc)
                          (args 'transform fnc)
                          tail?))))
      (define (free-vars)
        (set-union (func 'free-vars)
                   (args 'free-vars)))
      (define (gen-rust module)
        (func 'gen-rust module)
        (print module ".invoke(&[")
        (args 'gen-rust module)
        (print module "])"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'APPLICATION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message APPLICATION" msg))))
      self)

    (define (make-function-application var args tail?)
      (define (repr)
        (list (if tail? 'FN-APPLY-TC 'FN-APPLY)
              (variable-name var)
              (args 'repr)))
      (define (transform fnc)
        (fnc self
             (lambda () (make-function-application
                          var
                          (args 'transform fnc)
                          tail?))))
      (define (free-vars)
        (args 'free-vars))
      (define (gen-rust module)
        (cond ((global-function? var)
               (print module ""))
              ((import-variable? var)
               (print module "imports::"))
              (else
                (error "invalid function application" var)))
        (print module
               (variable-name var)
               "(&[")
        (args 'gen-rust module)
        (print module "])"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'FN-APPLICATION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-var msg) var)
              (else (error "Unknown message FN-APPLICATION" msg))))
      self)

    (define (make-null-arg)
      (define (repr) (list 'NULL-ARG))
      (define (transform fnc) (fnc self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust module) (print module ""))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'NULL-ARG)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message NULL-ARG" msg))))
      self)

    (define (make-args arg next)
      (define (repr)
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
      (define (gen-rust module)
        (arg 'gen-rust module)
        (if (not (eq? 'NULL-ARG (next 'kind)))
            (begin
              (print module ",")
              (next 'gen-rust module))))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ARG)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ARG" msg))))
      self)

    (define (make-fixlet vars args body)
      (define (repr)
        (list 'FIXLET vars (args 'repr) (body 'repr)))
      (define (transform fnc)
        (fnc self
             (lambda () (make-fixlet
                          vars
                          (args 'transform fnc)
                          (body 'transform fnc)))))
      (define (free-vars)
        (set-union (set-remove* (body 'free-vars)
                                (map free-var-name
                                     (map variable-name vars)))
                   (args 'free-vars)))
      (define (gen-rust-inner module)
        (define (gen-params v*)
          (if (pair? v*)
              (begin (print module
                            (variable-name (car v*))
                            ", ")
                     (gen-params (cdr v*)))))
        (cond ((= 0 (length vars))
               'IGNORE)
              ((= 1 (length vars))
               (print module "let ")
               (print module (variable-name (car vars)))
               (print module " = ")
               (args 'gen-rust module)
               (print module ";"))
              (else
                (print module "let [")
                (gen-params vars)
                (print module "] = [")
                (args 'gen-rust module)
                (print module "];")))
        (if (eq? 'FIXLET (body 'kind))
            (body 'gen-rust-inner module)
            (if (and (eq? 'COMMENT (body 'kind))
                     (eq? 'FIXLET ((body 'inner) 'kind)))
                (body 'gen-rust-inner module)
                (body 'gen-rust module))))
      (define (gen-rust module)
        (rust-block module
          (lambda () (gen-rust-inner module))))
      (define (self msg . arg*)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car arg*)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'FIXLET)
              ((eq? 'get-params msg) (map variable-name vars))
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-args msg) args)
              ((eq? 'get-body msg) body)
              ((eq? 'gen-rust msg) (gen-rust (car arg*)))
              ((eq? 'gen-rust-inner msg) (gen-rust-inner (car arg*)))
              (else (error "Unknown message FIXLET" msg))))
      self)

    (define (make-closure function)
      (define (repr)
        (list 'CLOSURE function))
      (define (transform func)
        (func self
              (lambda ()
                (make-closure (function 'transform func)))))
      (define (free-vars)
        (function 'free-vars))
      (define (prepare-closure module free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (print module "let ")
              (print module name)
              (print module " = ")
              (print module name)
              (print module ".clone();")
              (prepare-closure module (cdr free-vars)))))
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (println module "// Closure")
            (prepare-closure module (free-vars))
            (print module "Scm::func(move |args: &[Scm]|")
            (function 'gen-rust module)
            (print module ")"))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'CLOSURE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'inner-function msg) function)
              (else (error "Unknown message CLOSURE" msg))))
      self)

    (define (make-abstraction vars body)
      (define (repr)
        (list 'ABSTRACTION vars (body 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (make-abstraction vars (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     (map free-var-name
                          (map variable-name vars))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let ")
                     (print module (variable-name (car p*)))
                     (print module " = args[")
                     (print module k)
                     (print module "].clone();")
                     (gen-params (cdr p*) (+ k 1)))))
        (rust-block module
          (lambda ()
            (print module "if args.len() != ")
            (print module (length vars))
            (print module "{panic!(\"invalid arity\")}")
            (gen-params vars 0)
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) (map variable-name vars))
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-body msg) body)
              (else (error "Unknown message ABSTRACTION" msg))))
      self)

    (define (make-vararg-abstraction vars varvar body)
      (define (repr)
        (list 'VARARG-ABSTRACTION vars varvar (body 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (make-vararg-abstraction vars varvar (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     (map free-var-name
                          (map variable-name (cons varvar vars)))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let "
                                   (variable-name (car p*))
                                   " = args["
                                   k
                                   "].clone();")
                     (gen-params (cdr p*) (+ k 1)))
              (begin (print module "let "
                                   (variable-name varvar)
                                   " = Scm::list(&args["
                                   k
                                   "..]);"))))
        (rust-block module
          (lambda ()
            (print module "if args.len() < "
                          (length vars)
                          "{panic!(\"not enough args\")}")
            (gen-params vars 0)
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'VARARG-ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) (map variable-name vars))
              ((eq? 'get-vararg msg) (variable-name varvar))
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-varvar msg) varvar)
              ((eq? 'get-body msg) body)
              (else (error "Unknown message VARARG-ABSTRACTION" msg))))
      self)

    (define (make-program globals imports init body libraries)
      (define (repr)
        (cons 'PROGRAM
              (cons globals
                    (cons imports
                          (body 'repr)))))
      (define (transform func)
        (func self
              (lambda ()
                (make-program globals imports init (body 'transform func) libraries))))
      (define (gen-imports module)
        (for-each (lambda (i)
                    (i 'gen-rust module))
                  imports))
      (define (gen-rust module)
        (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
        (print module "mod imports")
        (rust-block module
          (lambda ()
            (gen-imports module)))
        (println module)
        (println module)
        (rust-gen-global-defs module globals)
        (println module)
        (println module)
        (print module "pub fn main()")
        (rust-block module
          (lambda ()
            (println module)
            (println module "eprintln!(\"built with\");")
            (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
            (println module)
            (for-each (lambda (lib)
                        (print module "crate::")
                        (for-each (lambda (l)
                                    (print module (rustify-libname l))
                                    (print module "::"))
                                  lib)
                        (print module "initialize();")
                        (println module))
                      init)
            (body 'gen-rust module)
            (println module ";")))
        (println module)
        (rust-gen-modules module libraries))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'PROGRAM)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message PROGRAM" msg))))
      self)

    (define (make-library name globals init body imports exports)
      (let* ((tests (list 'dummy))
             (new-body (body
                        'transform
                        (lambda (node ignore)
                          (if (eq? (node 'kind) 'TESTSUITE)
                              (begin
                                (set-cdr! tests (cons node (cdr tests)))
                                (make-constant '*UNSPECIFIED*))
                              (ignore))))))
        (_make-library name globals init new-body imports exports (cdr tests))))

    (define (_make-library name globals init body imports exports testsuite)
      (define (repr)
        (append 'LIBRARY name exports imports globals (body 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (_make-library name
                               globals
                               init
                               (body 'transform func)
                               (map (lambda (i) (i 'transform func)) imports)
                               (map (lambda (e) (e 'transform func)) exports)
                               (map (lambda (t) (t 'transform func))
                                    testsuite)))))
      (define (gen-exports module exports)
        (for-each (lambda (expo) (expo 'gen-rust module))
                  exports))
      (define (gen-rust module)
        (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
        (print module "mod imports")
        (rust-block module
          (lambda ()
            (for-each (lambda (i) (i 'gen-rust module))
                      imports)))
        (println module)
        (println module)
        (print module "pub mod exports")
        (rust-block module
          (lambda ()
            (gen-exports module exports)))
        (println module)
        (println module)
        (rust-gen-global-defs module globals)
        (println module)
        (println module)
        (if (eq? 'NOP (body 'kind))
            (println module "pub fn initialize() {")
            (begin
              (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
              (println module)
              (println module "pub fn initialize() {")
              (println module "if INITIALIZED.with(|x| x.get()) { return }")
              (println module "INITIALIZED.with(|x| x.set(true));")
              (println module)))
        (for-each (lambda (lib)
                    (print module "crate::")
                    (for-each (lambda (l) (print module (rustify-libname l) "::"))
                              lib)
                    (println module "initialize();"))
                  init)
        (body 'gen-rust module)
        (println module ";}")

        (for-each (lambda (test) (test 'gen-rust module))
                  testsuite))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'LIBRARY)
              ((eq? 'libname msg) name)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message LIBRARY" msg))))
      self)

    (define (make-boxify var body)
      (define (repr)
        (list 'BOXIFY var (body 'repr)))
      (define (transform func)
        (func self (lambda () (make-boxify var (body 'transform func)))))
      (define (free-vars)
        (body 'free-vars))
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (print module "let ")
            (print module (variable-name var))
            (print module " = ")
            (print module (variable-name var))
            (print module ".into_boxed();")
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'BOXIFY)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message BOXIFY" msg))))
      self)

    (define (make-export var exname)
      (define (repr)
        (list 'EXPORT var 'AS exname))
      (define (transform func)
        (func self (lambda () self)))
      (define (gen-rust module)
        (print module "pub use super::")
        (let* ((name (variable-name var)))
          (cond ((not var)
                 (error "undefined export" name))
                ((global-variable? var)
                 (print module ""))
                ((global-function? var)
                 (print module ""))
                ((import-variable? var)
                 (print module "imports::"))
                (else (error "invalid export variable" var name)))
          (println module
            name
            " as "
            (rustify-identifier exname)
            ";")))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'EXPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-var msg) var)
              (else (error "Unknown message EXPORT" msg))))
      self)

    (define (make-import lib)
      (define (repr)
        (cons 'IMPORT lib))
      (define (transform func)
        (func self (lambda () (make-import lib))))
      (define (free-vars)
        (make-set))
      (define (gen-libname module lib)
        (if (null? lib)
            (print module "")
            (begin (print module (rustify-libname (car lib)))
                   (if (null? (cdr lib))
                       (print module "")
                       (print module "::"))
                   (gen-libname module (cdr lib)))))
      (define (gen-rust module)
        (print module "pub use crate::")
        (gen-libname module lib)
        (print module "::exports::*;")
        (println module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'IMPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message IMPORT" msg))))
      self)

    (define (make-import-only lib names)
      (define (repr)
        (cons 'IMPORT-ONLY (cons lib names)))
      (define (transform func)
        (func self (lambda () (make-import-only lib names))))
      (define (free-vars)
        (make-set))
      (define (gen-libname module lib)
        (if (null? lib)
            (print module "")
            (begin (print module (rustify-libname (car lib)))
                   (if (null? (cdr lib))
                       (print module "")
                       (print module "::"))
                   (gen-libname module (cdr lib)))))
      (define (gen-imports module names)
        (if (null? names)
            'DONE
            (begin (print module (rustify-identifier (car names)))
                   (print module ", ")
                   (gen-imports module (cdr names)))))
      (define (gen-rust module)
        (print module "pub use crate::")
        (gen-libname module lib)
        (print module "::exports::{")
        (gen-imports module names)
        (print module "};")
        (println module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'IMPORT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message IMPORT" msg))))
      self)

    (define (make-testcase description body)
      (define (repr)
        (list 'TESTCASE description body))
      (define (transform func)
        (func self (lambda () (make-testcase description
                                             (body 'transform func)))))
      (define (free-vars) (make-set))
      (define (gen-rust module)
        (println module "#[test]")
        (println module "fn " (rustify-testname description) "() {")
        (println module "super::initialize();")
        (body 'gen-rust module)
        (println module "}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'TESTCASE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message TESTCASE" msg))))
      self)

    (define (make-testsuite name cases)
      (define (repr)
        (list 'TESTSUITE name cases))
      (define (transform func)
        (func self
              (lambda ()
               (make-testsuite name
                               (map (lambda (c) (c 'transform func))
                                    cases)))))
      (define (free-vars) (make-set))
      (define (gen-rust module)
         (println module "#[cfg(test)]")
         (println module "mod tests {")
         (println module "use super::*;")
         (for-each (lambda (c)
                     (c 'gen-rust module))
                   cases)
         (println module "}"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'TESTSUITE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message TESTSUITE" msg))))
      self)

    (define (make-assert condition)
      (define (repr)
        (list 'ASSERT condition))
      (define (transform func)
        (func self (lambda () (make-assert (condition 'transform func)))))
      (define (free-vars)
        (condition 'free-vars))
      (define (gen-rust module)
        (print module "assert!(")
        (condition 'gen-rust module)
        (println module ".is_true());"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (repr))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ASSERT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ASSERT" msg))))
      self)

    (define (free-var-name name)
      (cond ((symbol? name)
             name)
            ((string? name)
             (string->symbol name))
            (else (error "Invalid variable name" name))))))
