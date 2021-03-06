(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme read)
        (scheme write)
        (only (scheme process-context) command-line)
        (sunny ast-transforms boxify)
        (sunny ast-transforms close-procedures)
        (sunny ast-transforms extract-definitions)
        (sunny ast-transforms rename-vars)
        (sunny astify-toplevel)
        (sunny rust codegen)
        (sunny rust rustify)
        (sunny table)
        (sunny variable)
        (testsuite))

(define UNIQUE-COUNTS '())
(define (unique-name name)
  (let* ((entry (assoc name UNIQUE-COUNTS))
         (nr (cond (entry
                    (set-cdr! entry (+ 1 (cdr entry)))
                    (cdr entry))
                   (else
                    (set! UNIQUE-COUNTS (cons (cons name 0) UNIQUE-COUNTS))
                    0))))
    (string-append name "_" (number->string nr))))

(define (rust-pipeline scheme-ast)
  (extract-definitions
    (boxify
      (rename-vars (lambda (name var)
                     (let* ((str-name (if (string? name) name (symbol->string name)))
                            (rust-name (rustify-identifier str-name)))
                       (if (local-variable? var)
                           (unique-name rust-name)
                           rust-name)))
        (close-procedures
          scheme-ast)))))

(define args (command-line))

(define input-file-name (cadr args))
(define output-module-name (caddr args))

(define output-dir (if (pair? (cdddr args))
                       (cadddr args)
                       "."))

(newline)
(display input-file-name)
(display " --> ")
(display output-dir)
(display "/")
(display output-module-name)
(newline)
(newline)

(define input-file (open-input-file input-file-name))

(define (load-sexpr)
  (let ((expr (read input-file)))
    (if (eof-object? expr)
        '()
        (cons expr (load-sexpr)))))

(define program (load-sexpr))

(define ast (astify-toplevel program rust-pipeline))

(rust-gen-in-module output-module-name output-dir
  (lambda (module)
    (ast 'gen-rust module)))
