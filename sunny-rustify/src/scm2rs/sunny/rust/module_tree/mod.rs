#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::make_minus_module_minus_tree_minus_leaf;
    pub use super::make_minus_module_minus_tree_minus_node;
    pub use super::module_minus_tree_minus_append_minus_child_i;
    pub use super::module_minus_tree_minus_children;
    pub use super::module_minus_tree_minus_find_minus_child;
    pub use super::module_minus_tree_minus_insert_i;
    pub use super::module_minus_tree_minus_leaf_p;
    pub use super::module_minus_tree_minus_libobj;
    pub use super::module_minus_tree_minus_name;
    pub use super::module_minus_tree_minus_set_minus_children_i;
}

pub fn make_minus_module_minus_tree_minus_leaf(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        let lib = args[1].clone();
        {
            // (cons name lib)
            imports::cons(&[name.clone(), lib.clone()])
        }
    }
    .into()
}
pub fn make_minus_module_minus_tree_minus_node(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        {
            // (cons name (quote ()))
            imports::cons(&[name.clone(), Scm::Nil])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_append_minus_child_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        let child = args[1].clone();
        {
            // (module-tree-set-children! node (cons child (module-tree-children node)))
            module_minus_tree_minus_set_minus_children_i(&[node.clone(), {
                // (cons child (module-tree-children node))
                imports::cons(&[child.clone(), {
                    // (module-tree-children node)
                    module_minus_tree_minus_children(&[node.clone()])
                }])
            }])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_children(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        {
            // (cdr node)
            imports::cdr(&[node.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_find_minus_child(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        let name = args[1].clone();
        {
            if ({
                // (module-tree-leaf? node)
                module_minus_tree_minus_leaf_p(&[node.clone()])
            })
            .is_true()
            {
                {
                    // (error "called (module-tree-find-child) on leaf node" name node)
                    imports::error(&[
                        Scm::from("called (module-tree-find-child) on leaf node"),
                        name.clone(),
                        node.clone(),
                    ])
                }
            } else {
                Scm::symbol("*UNSPECIFIED*")
            };
            {
                // (assq name (module-tree-children node))
                imports::assq(&[name.clone(), {
                    // (module-tree-children node)
                    module_minus_tree_minus_children(&[node.clone()])
                }])
            }
        }
    }
    .into()
}
pub fn module_minus_tree_minus_insert_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let tree = args[0].clone();
        let libname = args[1].clone();
        let libobj = args[2].clone();
        {
            if ({
                // (null? libname)
                imports::null_p(&[libname.clone()])
            })
            .is_true()
            {
                {
                    // (error "invalid insert - empty libname")
                    imports::error(&[Scm::from("invalid insert - empty libname")])
                }
            } else {
                Scm::symbol("*UNSPECIFIED*")
            };
            {
                // (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node)))))
                {
                    let child = {
                        // (module-tree-find-child tree (car libname))
                        module_minus_tree_minus_find_minus_child(&[tree.clone(), {
                            // (car libname)
                            imports::car(&[libname.clone()])
                        }])
                    };
                    if (child.clone()).is_true() {
                        {
                            // (module-tree-insert! child (cdr libname) libobj)
                            Scm::func(module_minus_tree_minus_insert_i).invoke(&[
                                child.clone(),
                                {
                                    // (cdr libname)
                                    imports::cdr(&[libname.clone()])
                                },
                                libobj.clone(),
                            ])
                        }
                    } else if ({
                        // (null? (cdr libname))
                        imports::null_p(&[{
                            // (cdr libname)
                            imports::cdr(&[libname.clone()])
                        }])
                    })
                    .is_true()
                    {
                        {
                            // (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
                            module_minus_tree_minus_append_minus_child_i(&[tree.clone(), {
                                // (make-module-tree-leaf (car libname) libobj)
                                make_minus_module_minus_tree_minus_leaf(&[
                                    {
                                        // (car libname)
                                        imports::car(&[libname.clone()])
                                    },
                                    libobj.clone(),
                                ])
                            }])
                        }
                    } else {
                        {
                            // (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))
                            {
                                let new_minus_node = {
                                    // (make-module-tree-node (car libname))
                                    make_minus_module_minus_tree_minus_node(&[{
                                        // (car libname)
                                        imports::car(&[libname.clone()])
                                    }])
                                };
                                {
                                    {
                                        // (module-tree-insert! new-node (cdr libname) libobj)
                                        Scm::func(module_minus_tree_minus_insert_i).invoke(&[
                                            new_minus_node.clone(),
                                            {
                                                // (cdr libname)
                                                imports::cdr(&[libname.clone()])
                                            },
                                            libobj.clone(),
                                        ])
                                    };
                                    {
                                        // (module-tree-append-child! tree new-node)
                                        module_minus_tree_minus_append_minus_child_i(&[
                                            tree.clone(),
                                            new_minus_node.clone(),
                                        ])
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn module_minus_tree_minus_leaf_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        {
            // (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node))))
            if ({
                // (pair? node)
                imports::pair_p(&[node.clone()])
            })
            .is_true()
            {
                if ({
                    // (symbol? (car node))
                    imports::symbol_p(&[{
                        // (car node)
                        imports::car(&[node.clone()])
                    }])
                })
                .is_true()
                {
                    if ({
                        // (not (null? (cdr node)))
                        imports::not(&[{
                            // (null? (cdr node))
                            imports::null_p(&[{
                                // (cdr node)
                                imports::cdr(&[node.clone()])
                            }])
                        }])
                    })
                    .is_true()
                    {
                        {
                            // (not (pair? (cdr node)))
                            imports::not(&[{
                                // (pair? (cdr node))
                                imports::pair_p(&[{
                                    // (cdr node)
                                    imports::cdr(&[node.clone()])
                                }])
                            }])
                        }
                    } else {
                        Scm::False
                    }
                } else {
                    Scm::False
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn module_minus_tree_minus_libobj(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        {
            // (cdr node)
            imports::cdr(&[node.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        {
            // (car node)
            imports::car(&[node.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_set_minus_children_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node = args[0].clone();
        let children = args[1].clone();
        {
            // (set-cdr! node children)
            imports::set_minus_cdr_i(&[node.clone(), children.clone()])
        }
    }
    .into()
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    {
        (/*NOP*/);
        {
            // (define (make-module-tree-node name) ...)
            (/*NOP*/)
        };
        {
            // (define (make-module-tree-leaf name lib) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-leaf? node) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-name node) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-children node) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-libobj node) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-set-children! node children) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-find-child node name) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-append-child! node child) ...)
            (/*NOP*/)
        };
        {
            // (define (module-tree-insert! tree libname libobj) ...)
            (/*NOP*/)
        }
    };
}
