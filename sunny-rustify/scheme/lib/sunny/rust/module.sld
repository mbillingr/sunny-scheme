(define-library (sunny rust module)
  (export close-module
          module?
          module-path
          module-port
          open-module
          open-submodule)

  (import (scheme base)
          (scheme cxr)
          (scheme file)
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
      (caddr module))))
