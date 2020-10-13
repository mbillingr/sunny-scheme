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
        let name__298 = args[0].clone();
        let lib__299 = args[1].clone();
        {
            // (cons name lib)
            imports::cons(&[name__298.clone(), lib__299.clone()])
        }
    }
    .into()
}
pub fn make_minus_module_minus_tree_minus_node(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__297 = args[0].clone();
        {
            // (cons name (quote ()))
            imports::cons(&[name__297.clone(), Scm::Nil])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_append_minus_child_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node__308 = args[0].clone();
        let child__309 = args[1].clone();
        {
            // (module-tree-set-children! node (cons child (module-tree-children node)))
            module_minus_tree_minus_set_minus_children_i(&[node__308.clone(), {
                // (cons child (module-tree-children node))
                imports::cons(&[child__309.clone(), {
                    // (module-tree-children node)
                    module_minus_tree_minus_children(&[node__308.clone()])
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
        let node__302 = args[0].clone();
        {
            // (cdr node)
            imports::cdr(&[node__302.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_find_minus_child(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node__306 = args[0].clone();
        let name__307 = args[1].clone();
        {
            if ({
                // (module-tree-leaf? node)
                module_minus_tree_minus_leaf_p(&[node__306.clone()])
            })
            .is_true()
            {
                {
                    // (error "called (module-tree-find-child) on leaf node" name node)
                    imports::error(&[
                        Scm::from("called (module-tree-find-child) on leaf node"),
                        name__307.clone(),
                        node__306.clone(),
                    ])
                }
            } else {
                Scm::symbol("*UNSPECIFIED*")
            };
            {
                // (assq name (module-tree-children node))
                imports::assq(&[name__307.clone(), {
                    // (module-tree-children node)
                    module_minus_tree_minus_children(&[node__306.clone()])
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
        let tree__311 = args[0].clone();
        let libname__310 = args[1].clone();
        let libobj__313 = args[2].clone();
        {
            if ({
                // (null? libname)
                imports::null_p(&[libname__310.clone()])
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
                    let child__312 = {
                        // (module-tree-find-child tree (car libname))
                        module_minus_tree_minus_find_minus_child(&[tree__311.clone(), {
                            // (car libname)
                            imports::car(&[libname__310.clone()])
                        }])
                    };
                    if (child__312.clone()).is_true() {
                        {
                            // (module-tree-insert! child (cdr libname) libobj)
                            Scm::func(module_minus_tree_minus_insert_i).invoke(&[
                                child__312.clone(),
                                {
                                    // (cdr libname)
                                    imports::cdr(&[libname__310.clone()])
                                },
                                libobj__313.clone(),
                            ])
                        }
                    } else if ({
                        // (null? (cdr libname))
                        imports::null_p(&[{
                            // (cdr libname)
                            imports::cdr(&[libname__310.clone()])
                        }])
                    })
                    .is_true()
                    {
                        {
                            // (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
                            module_minus_tree_minus_append_minus_child_i(&[tree__311.clone(), {
                                // (make-module-tree-leaf (car libname) libobj)
                                make_minus_module_minus_tree_minus_leaf(&[
                                    {
                                        // (car libname)
                                        imports::car(&[libname__310.clone()])
                                    },
                                    libobj__313.clone(),
                                ])
                            }])
                        }
                    } else {
                        {
                            // (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))
                            {
                                let new_minus_node__314 = {
                                    // (make-module-tree-node (car libname))
                                    make_minus_module_minus_tree_minus_node(&[{
                                        // (car libname)
                                        imports::car(&[libname__310.clone()])
                                    }])
                                };
                                {
                                    {
                                        // (module-tree-insert! new-node (cdr libname) libobj)
                                        Scm::func(module_minus_tree_minus_insert_i).invoke(&[
                                            new_minus_node__314.clone(),
                                            {
                                                // (cdr libname)
                                                imports::cdr(&[libname__310.clone()])
                                            },
                                            libobj__313.clone(),
                                        ])
                                    };
                                    {
                                        // (module-tree-append-child! tree new-node)
                                        module_minus_tree_minus_append_minus_child_i(&[
                                            tree__311.clone(),
                                            new_minus_node__314.clone(),
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
        let node__300 = args[0].clone();
        {
            // (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node))))
            if ({
                // (pair? node)
                imports::pair_p(&[node__300.clone()])
            })
            .is_true()
            {
                if ({
                    // (symbol? (car node))
                    imports::symbol_p(&[{
                        // (car node)
                        imports::car(&[node__300.clone()])
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
                                imports::cdr(&[node__300.clone()])
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
                                    imports::cdr(&[node__300.clone()])
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
        let node__303 = args[0].clone();
        {
            // (cdr node)
            imports::cdr(&[node__303.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node__301 = args[0].clone();
        {
            // (car node)
            imports::car(&[node__301.clone()])
        }
    }
    .into()
}
pub fn module_minus_tree_minus_set_minus_children_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let node__304 = args[0].clone();
        let children__305 = args[1].clone();
        {
            // (set-cdr! node children)
            imports::set_minus_cdr_i(&[node__304.clone(), children__305.clone()])
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
