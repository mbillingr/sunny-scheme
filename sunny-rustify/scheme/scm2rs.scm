(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme read)
        (scheme write)
        (only (scheme process-context) command-line)
        (sunny translate)
        (testsuite))

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

(define ast (scm->ast program))

(rust-gen-in-module output-module-name output-dir
  (lambda (module)
    (ast 'gen-rust module)))
