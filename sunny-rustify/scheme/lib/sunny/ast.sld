(define-library (sunny ast)
  (export make-abstraction
          make-alternative
          make-application
          make-args
          make-assignment
          make-boxify
          make-comment
          make-constant
          make-fixlet
          make-library
          make-nop
          make-null-arg
          make-program
          make-reference
          make-sequence
          make-scope
          make-vararg-abstraction)

  (import (scheme base)
          (sunny sets)
          (sunny rust codegen)
          (sunny rust module)
          (sunny rust rustify)
          (sunny utils)
          (sunny variable))

  (begin
    (define (make-comment comment node)
      (define (repr)
        (cons 'COMMENT
              (cons comment
                    (node 'repr))))
      (define (transform func)
        (func self (lambda () (make-comment comment
                                            (node 'transform func)))))
      (define (free-vars)
        (node 'free-vars))
      (define (gen-rust module)
        (println module)
        (print module "// ")
        (showln module comment)
        (node 'gen-rust module))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'COMMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message COMMENT" msg))))
      self)

    (define (make-nop)
      (define (repr) '(NOP))
      (define (transform func) (func self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust module) (print module "(/*NOP*/)"))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
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
              ((eq? val #\') (print module "Scm::char('\\'')"))
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
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'CONSTANT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message CONSTANT" msg))))
      self)

    (define (make-reference name var)
      (define (global?)
        (if (eq? 'GLOBAL-REF (variable-getter var))
            #t
            (eq? 'IMPORT-REF (variable-getter var))))
      (define (repr)
        (list (variable-getter var) name))
      (define (transform func)
        (func self (lambda () self)))
      (define (free-vars)
        (if (global?)
            (make-set)
            (set-add (make-set)
                     name)))
      (define (gen-rust module)
        (let ((getter (variable-getter var)))
          (cond ((eq? 'GLOBAL-REF getter)
                 (print module
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'IMPORT-REF getter)
                 (print module
                        "imports::"
                        (rustify-identifier name)
                        ".with(|value| value.get())"))
                ((eq? 'BOXED-REF getter)
                 (print module (rustify-identifier name) ".get()"))
                (else
                  (print module (rustify-identifier name) ".clone()")))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'REFERENCE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message REFERENCE" msg))))
      self)

    (define (make-assignment name var val)
      (define (repr)
        (list (variable-setter var) name (val 'repr)))
      (define (transform func)
        (func self
              (lambda () (make-assignment name
                                          var
                                          (val 'transform func)))))
      (define (free-vars)
        (set-add (val 'free-vars)
                 name))
      (define (gen-rust module)
        (let ((setter (variable-setter var)))
          (cond ((eq? 'GLOBAL-SET setter)
                 (print module
                        "globals::"
                        (rustify-identifier name)
                        ".with(|value| value.set(")
                 (val 'gen-rust module)
                 (print module "))"))
                ((eq? 'BOXED-SET setter)
                 (print module (rustify-identifier name) ".set(")
                 (val 'gen-rust module)
                 (print module ")"))
                (else (error "set! on unboxed variable")))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ASSIGNMENT)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ASSIGNMENT" msg))))
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
        (cond ((eq? 'repr msg) (print))
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
                (make-sequence (first 'transform func)
                               (next 'transform func)))))
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
        (cond ((eq? 'repr msg) (print))
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
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'APPLICATION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message APPLICATION" msg))))
      self)

    (define (make-null-arg)
      (define (repr) (list 'NULL-ARG))
      (define (transform fnc) (fnc self (lambda () self)))
      (define (free-vars) (make-set))
      (define (gen-rust module) (print module ""))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
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
        (print module ",")
        (next 'gen-rust module))

      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ARG)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message ARG" msg))))
      self)

    (define (make-fixlet params body args)
      (define (repr)
        (cons 'FIXLET
              (cons params
                    (cons (args 'repr)
                          (body 'repr)))))
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

      (define (gen-rust module)
        (define (gen-params p*)
          (if (pair? p*)
              (begin (print module (rustify-identifier (car p*)) ", ")
                     (gen-params (cdr p*)))))
        (rust-block module
          (lambda ()
            (print module "let [")
            (gen-params params)
            (print module "] = [")
            (args 'gen-rust module)
            (print module "];")
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'FIXLET)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message FIXLET" msg))))
      self)

    (define (make-scope params body args)
      (define (repr)
        (cons 'SCOPE
              (cons params
                    (cons (args 'repr)
                          (body 'repr)))))
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
        (set-remove* (set-union (body 'free-vars)
                                (free-vars-args args))
                     params))
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (for-each (lambda (p) (println module
                                    "let "
                                    (rustify-identifier p)
                                    " = Scm::uninitialized().into_boxed();"))
                      params)
            (for-each (lambda (p a) (print module
                                           (rustify-identifier p)
                                           ".set(")
                                    (a 'gen-rust module)
                                    (println module ");"))
                      params
                      args)
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'SCOPE)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message SCOPE" msg))))
      self)

    (define (make-abstraction params vars body)
      (define (repr)
        (cons 'ABSTRACTION
              (cons params
                    (body 'repr))))
      (define (transform func)
        (func self
              (lambda ()
                (make-abstraction params vars (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     params))
      (define (prepare-closure module free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (print module "let ")
              (print module (rustify-identifier name))
              (print module " = ")
              (print module (rustify-identifier name))
              (print module ".clone();")
              (prepare-closure module (cdr free-vars)))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let ")
                     (print module (rustify-identifier (car p*)))
                     (print module " = args[")
                     (print module k)
                     (print module "].clone();")
                     (gen-params (cdr p*) (+ k 1)))))
        (rust-block module
          (lambda ()
            (prepare-closure module (free-vars))
            (print module "Scm::func(move |args: &[Scm]|")
            (rust-block module
              (lambda ()
                (print module "if args.len() != ")
                (print module (length params))
                (print module "{panic!(\"invalid arity\")}")
                (gen-params params 0)
                (body 'gen-rust module)))
            (print module ")"))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) params)
              ((eq? 'get-vars msg) vars)
              ((eq? 'get-body msg) body)
              (else (error "Unknown message ABSTRACTION" msg))))
      self)

    (define (make-vararg-abstraction params vararg vars varvar body)
      (define (repr)
        (cons 'VARARG-ABSTRACTION
              (cons params
                    (body 'repr))))
      (define (transform func)
        (func self
              (lambda ()
                (make-vararg-abstraction params vararg vars varvar (body 'transform func)))))
      (define (free-vars)
        (set-remove* (body 'free-vars)
                     (cons vararg params)))
      (define (prepare-closure module free-vars)
        (if (pair? free-vars)
            (let ((name (car free-vars)))
              (print module "let ")
              (print module (rustify-identifier name))
              (print module " = ")
              (print module (rustify-identifier name))
              (print module ".clone();")
              (prepare-closure module (cdr free-vars)))))
      (define (gen-rust module)
        (define (gen-params p* k)
          (if (pair? p*)
              (begin (print module "let "
                                   (rustify-identifier (car p*))
                                   " = args["
                                   k
                                   "].clone();")
                     (gen-params (cdr p*) (+ k 1)))
              (begin (print module "let "
                                   (rustify-identifier vararg)
                                   " = Scm::list(&args["
                                   k
                                   "..]);"))))
        (rust-block module
          (lambda ()
            (prepare-closure module (free-vars))
            (print module "Scm::func(move |args: &[Scm]|")
            (rust-block module
              (lambda ()
                (print module "if args.len() < "
                              (length params)
                              "{panic!(\"not enough args\")}")
                (gen-params params 0)
                (body 'gen-rust module)))
            (print module ")"))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'VARARG-ABSTRACTION)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              ((eq? 'get-params msg) params)
              ((eq? 'get-vararg msg) vararg)
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
                (make-program globals imports init (body 'transform func)))))
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
        (print module "mod globals")
        (rust-block module
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println module "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs module globals)))
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
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'PROGRAM)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message PROGRAM" msg))))
      self)

    (define (make-library name globals init body imports exports)
      (define (repr)
        (append 'LIBRARY name exports imports globals (body 'repr)))
      (define (transform func)
        (func self
              (lambda ()
                (make-library globals
                              init
                              (body 'transform func)
                              imports
                              exports))))
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
        (print module "mod globals")
        (rust-block module
          (lambda ()
            (if (any (lambda (g) (global-regular? (cdr g)))
                     globals)
                (println module "use sunny_core::{Mut, Scm};"))
            (rust-gen-global-defs module globals)))
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
        (let ((tests (list 'dummy)))
          ((body 'transform (lambda (node ignore)
                              (if (eq? (node 'kind) 'TESTSUITE)
                                  (begin
                                    (set-cdr! tests (cons node (cdr tests)))
                                    (make-constant '*UNSPECIFIED*))
                                  (ignore))))
           'gen-rust module)
          (println module ";}")

          (for-each (lambda (test) (test 'gen-rust module))
                    (cdr tests))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'kind msg) 'LIBRARY)
              ((eq? 'libname msg) name)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message LIBRARY" msg))))
      self)

    (define (make-boxify name body)
      (define (repr)
        (cons 'BOXIFY (cons name (body 'repr))))
      (define (transform func)
        (func self (lambda () (make-boxify name (body 'transform func)))))
      (define (free-vars)
        (body 'free-vars))
      (define (gen-rust module)
        (rust-block module
          (lambda ()
            (print module "let ")
            (print module (rustify-identifier name))
            (print module " = ")
            (print module (rustify-identifier name))
            (print module ".into_boxed();")
            (body 'gen-rust module))))
      (define (self msg . args)
        (cond ((eq? 'repr msg) (print))
              ((eq? 'transform msg) (transform (car args)))
              ((eq? 'free-vars msg) (free-vars))
              ((eq? 'kind msg) 'BOXIFY)
              ((eq? 'gen-rust msg) (gen-rust (car args)))
              (else (error "Unknown message BOXIFY" msg))))
      self)))
