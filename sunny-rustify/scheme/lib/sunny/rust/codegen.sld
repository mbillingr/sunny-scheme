(define-library (sunny rust codegen)
  (export rust-gen-global-defs
          rust-gen-in-module
          rust-gen-in-submodule
          rust-gen-module-tree
          rust-gen-module-tree-list
          rust-gen-modules)

  (import (scheme base)
          (sunny utils)
          (sunny rust module)
          (sunny rust module-tree)
          (sunny rust rustify)
          (sunny variable))

  (begin
    (define (rust-gen-global-defs module g)
      (if (null? g)
          (println module)
          (if (import-variable? (cdar g))
              (rust-gen-global-defs module (cdr g))
              (begin (println module
                       "thread_local!{#[allow(non_upper_case_globals)] pub static "
                       (rustify-identifier (caar g))
                       ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL "
                       (caar g)
                       "\"))}")
                     (rust-gen-global-defs module (cdr g))))))


    (define (rust-gen-modules module libs)
      (let ((module-tree (make-module-tree-node 'root)))
        (for-each (lambda (lib)
                    (module-tree-insert! module-tree (car lib) (cdr lib)))
                  libs)

        (rust-gen-module-tree-list module (module-tree-children module-tree))))

    (define (rust-gen-module-tree module node)
      (println module
        "pub mod " (rustify-libname (module-tree-name node)) ";")
      (if (module-tree-leaf? node)
          (rust-gen-in-submodule (module-tree-name node) module
            (lambda (submod)
              ((module-tree-libobj node) 'gen-rust submod)))
          (rust-gen-in-submodule (module-tree-name node) module
            (lambda (submod)
              (rust-gen-module-tree-list submod (module-tree-children node))))))

    (define (rust-gen-module-tree-list module nodes)
      (for-each (lambda (child)
                  (rust-gen-module-tree module child))
                nodes))


    (define (rust-gen-in-module name base-path body)
      (let ((module (open-module name base-path)))
        (body module)
        (close-module module)))

    (define (rust-gen-in-submodule name parent body)
      (let ((module (open-submodule name parent)))
        (body module)
        (close-module module)))))
