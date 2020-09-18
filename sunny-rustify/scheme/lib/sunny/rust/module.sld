(define-library (sunny rust module)
  (export close-module
          module?
          module-path
          module-port
          open-module
          open-submodule
          print println
          rust-block
          show showln)

  (import (scheme base)
          (scheme cxr)
          (scheme file)
          (scheme write)
          (chibi filesystem)
          (sunny rust rustify))

  (begin
    (define (module? obj)
      (and (pair? obj)
           (eq? 'module (car obj))))

    (define (open-module name base-path)
      (let ((path (string-append
                    base-path "/" (rustify-libname name))))
        (create-directory* path)
        (list 'module
              (open-output-file (string-append path "/mod.rs"))
              path)))

    (define (open-submodule name module)
      (open-module name (module-path module)))


    (define (close-module module)
      (close-port (module-port module)))

    (define (module-port module)
      (cadr module))

    (define (module-path module)
      (caddr module))

    (define (rust-block module code)
      (print module "{")
      (code)
      (print module "}"))

    (define (println f . args)
      (for-each (lambda (a) (display a (as-port f)))
                args)
      (newline (as-port f)))

    (define (print f . args)
      (for-each (lambda (a) (display a (as-port f)))
                args))

    (define (showln f . args)
      (for-each (lambda (a) (write a (as-port f)))
                args)
      (newline (as-port f)))

    (define (show f . args)
      (for-each (lambda (a) (write a (as-port f)))
                args))

    (define (as-port port-or-module)
      (if (module? port-or-module)
          (module-port port-or-module)
          port-or-module))))
