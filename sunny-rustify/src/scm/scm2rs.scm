(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme read)
        (scheme write)
        (only (scheme process-context) command-line)
        (sunny translate))

(define input-file-name (cadr (command-line)))
(define output-file-name (caddr (command-line)))

(newline)
(display input-file-name)
(display " --> ")
(display output-file-name)
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

(define output-file (open-output-file output-file-name))

(ast 'gen-rust output-file)

(close-port output-file)
