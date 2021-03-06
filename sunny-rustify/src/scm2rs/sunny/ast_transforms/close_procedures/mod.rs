#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::close_minus_procedures;
}

pub fn close_minus_procedures(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node_12 = args[0].clone();
        {
            // (letrec ((transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote CLOSURE)) node) (else (transform-children)))))) (node (quote transform) transform))
            {
                // (let ((transform (quote *uninitialized*))) (begin (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote CLOSURE)) node) (else (transform-children))))) (node (quote transform) transform)))
                {
                    let transform_26 = Scm::symbol("*uninitialized*");
                    {
                        let transform_26 = transform_26.into_boxed();
                        {
                            transform_26.set({
                                // Closure
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let node_13 = args[0].clone();
                                    let transform_minus_children_0 = args[1].clone();
                                    {
                                        // (cond ...)
                                        if ({
                                            // (eq? (node (quote kind)) (quote ABSTRACTION))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node_13.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (make-closure (transform-children))
                                                imports::make_minus_closure(&[{
                                                    // (transform-children)
                                                    transform_minus_children_0.clone().invoke(&[])
                                                }])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node_13.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("VARARG-ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (make-closure (transform-children))
                                                imports::make_minus_closure(&[{
                                                    // (transform-children)
                                                    transform_minus_children_0.clone().invoke(&[])
                                                }])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote CLOSURE))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node_13.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("CLOSURE"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            node_13.clone()
                                        } else {
                                            {
                                                // (transform-children)
                                                transform_minus_children_0.clone().invoke(&[])
                                            }
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (node (quote transform) transform)
                                node_12
                                    .clone()
                                    .invoke(&[Scm::symbol("transform"), transform_26.get()])
                            }
                        }
                    }
                }
            }
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
    crate::sunny::ast::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (close-procedures node) ...)
            (/*NOP*/)
        }
    };
}
