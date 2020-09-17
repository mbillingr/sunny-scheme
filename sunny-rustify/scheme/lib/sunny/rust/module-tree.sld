(define-library (sunny rust module-tree)
  (export make-module-tree-leaf
          make-module-tree-node
          module-tree-append-child!
          module-tree-children
          module-tree-find-child
          module-tree-insert!
          module-tree-leaf?
          module-tree-libobj
          module-tree-name
          module-tree-set-children!)

  (import (scheme base))

  (begin
    (define (make-module-tree-node name)
      (cons name '()))

    (define (make-module-tree-leaf name lib)
      (cons name lib))

    (define (module-tree-leaf? node)
      (and (pair? node)
           (symbol? (car node))
           (not (null? (cdr node)))
           (not (pair? (cdr node)))))

    (define (module-tree-name node)
      (car node))

    (define (module-tree-children node)
      (cdr node))

    (define (module-tree-libobj node)
      (cdr node))

    (define (module-tree-set-children! node children)
      (set-cdr! node children))

    (define (module-tree-find-child node name)
      (if (module-tree-leaf? node)
          (error "called (module-tree-find-child) on leaf node" name node))
      (assq name (module-tree-children node)))

    (define (module-tree-append-child! node child)
      (module-tree-set-children!
        node
        (cons child (module-tree-children node))))

    (define (module-tree-insert! tree libname libobj)
      (if (null? libname)
          (error "invalid insert"))
      (let ((child (module-tree-find-child tree (car libname))))
        (if child
            (module-tree-insert! child (cdr libname) libobj)
            (if (null? (cdr libname))
                (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
                (let ((new-node (make-module-tree-node (car libname))))
                  (module-tree-insert! new-node (cdr libname) libobj)
                  (module-tree-append-child! tree new-node))))))))
