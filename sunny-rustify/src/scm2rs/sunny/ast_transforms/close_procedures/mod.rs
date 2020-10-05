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
        let node = args[0].clone();
        {
            // (letrec ((transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote CLOSURE)) node) (else (transform-children)))))) (node (quote transform) transform))
            {
                // (let ((transform (quote *uninitialized*))) (begin (set! transform (lambda (node transform-children) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (make-closure (transform-children))) ((eq? (node (quote kind)) (quote CLOSURE)) node) (else (transform-children))))) (node (quote transform) transform)))
                {
                    let transform = Scm::symbol("*uninitialized*");
                    {
                        let transform = transform.into_boxed();
                        {
                            transform.set({
                                // Closure
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let node = args[0].clone();
                                    let transform_minus_children = args[1].clone();
                                    {
                                        // (cond ...)
                                        if ({
                                            // (eq? (node (quote kind)) (quote ABSTRACTION))
                                            Scm::func(imports::eq_p).invoke(&[
                                                {
                                                    // (node (quote kind))
                                                    node.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (make-closure (transform-children))
                                                Scm::func(imports::make_minus_closure).invoke(&[{
                                                    // (transform-children)
                                                    transform_minus_children.clone().invoke(&[])
                                                }])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                            Scm::func(imports::eq_p).invoke(&[
                                                {
                                                    // (node (quote kind))
                                                    node.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("VARARG-ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (make-closure (transform-children))
                                                Scm::func(imports::make_minus_closure).invoke(&[{
                                                    // (transform-children)
                                                    transform_minus_children.clone().invoke(&[])
                                                }])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote CLOSURE))
                                            Scm::func(imports::eq_p).invoke(&[
                                                {
                                                    // (node (quote kind))
                                                    node.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("CLOSURE"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            node.clone()
                                        } else {
                                            {
                                                // (transform-children)
                                                transform_minus_children.clone().invoke(&[])
                                            }
                                        }
                                    }
                                })
                            });
                            {
                                // (node (quote transform) transform)
                                node.clone()
                                    .invoke(&[Scm::symbol("transform"), transform.get()])
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
