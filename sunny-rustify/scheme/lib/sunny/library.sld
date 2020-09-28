(define-library (sunny library)
  (export check-imports
          get-lib
          library-decls
          library-exports
          library-name)

  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (sunny env)
          (sunny utils)
          (sunny variable))

  (begin
    (define (library-name expr)
      (cadr expr))

    (define (library-decls expr)
      (cddr expr))

    (define (library-exports lib-decl*)
      (cond ((null? lib-decl*) '())
            ((eq? 'export (caar lib-decl*))
             (append (cdar lib-decl*)
                     (library-exports (cdr lib-decl*))))
            (else (library-exports (cdr lib-decl*)))))

    (define (get-lib lib)
      (let ((full-path (find-library
                         '("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")
                         (library-path lib)
                         '(".sld" ".slx"))))
        (if full-path
            (read (open-input-file full-path))
            (error "Unknown library" lib))))


    (define (find-library base-path* relative-path extension*)
      (if (null? base-path*)
          #f
          (let* ((path (string-append (car base-path*) relative-path))
                 (full-path (find-library-ext path extension*)))
            (if full-path
                full-path
                (find-library (cdr base-path*) relative-path extension*)))))

    (define (find-library-ext path extension*)
      (if (null? extension*)
          #f
          (let ((full-path (string-append path (car extension*))))
            (if (file-exists? full-path)
                full-path
                (find-library-ext path (cdr extension*))))))

    (define (library-path lib)
      (reduce (lambda (left right)
                (string-append left
                               (string-append "/" right)))
              ""
              (map symbol->string lib)))

    (define (check-imports imports exports lib)
      (if (null? imports)
          #t
          (if (memq (car imports) exports)
              (check-imports (cdr imports) exports lib)
              (error "Invalid import" (car imports) lib))))))
