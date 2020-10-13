#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::boxify;
}

pub fn boxify(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let node__681 = args[0].clone();
        {
            // (letrec ((transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-vars)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore)))))) (node (quote transform) transform))
            {
                // (let ((transform (quote *uninitialized*))) (begin (set! transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-vars)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore))))) (node (quote transform) transform)))
                {
                    let transform__678 = Scm::symbol("*uninitialized*");
                    {
                        let transform__678 = transform__678.into_boxed();
                        {
                            transform__678.set({
                                // Closure
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let node__679 = args[0].clone();
                                    let ignore__680 = args[1].clone();
                                    {
                                        // (cond ...)
                                        if ({
                                            // (eq? (node (quote kind)) (quote ABSTRACTION))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node__679.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (boxify-abstraction (node (quote get-vars)) (node (quote get-vars)) (node (quote get-body)))
                                                Scm::func(boxify_minus_abstraction).invoke(&[
                                                    {
                                                        // (node (quote get-vars))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-vars))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-body")])
                                                    },
                                                ])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node__679.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("VARARG-ABSTRACTION"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (boxify-vararg-abstraction (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))
                                                Scm::func(boxify_minus_vararg_minus_abstraction)
                                                    .invoke(&[
                                                        {
                                                            // (node (quote get-vars))
                                                            node__679
                                                                .clone()
                                                                .invoke(&[Scm::symbol("get-vars")])
                                                        },
                                                        {
                                                            // (node (quote get-varvar))
                                                            node__679.clone().invoke(&[
                                                                Scm::symbol("get-varvar"),
                                                            ])
                                                        },
                                                        {
                                                            // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                            imports::cons(&[
                                                                {
                                                                    // (node (quote get-varvar))
                                                                    node__679.clone().invoke(&[
                                                                        Scm::symbol("get-varvar"),
                                                                    ])
                                                                },
                                                                {
                                                                    // (node (quote get-vars))
                                                                    node__679.clone().invoke(&[
                                                                        Scm::symbol("get-vars"),
                                                                    ])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (node (quote get-body))
                                                            node__679
                                                                .clone()
                                                                .invoke(&[Scm::symbol("get-body")])
                                                        },
                                                    ])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote FIXLET))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node__679.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("FIXLET"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (boxify-fixlet (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))
                                                Scm::func(boxify_minus_fixlet).invoke(&[
                                                    {
                                                        // (node (quote get-vars))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-args))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-args")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node__679
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-body")])
                                                    },
                                                ])
                                            }
                                        } else {
                                            {
                                                // (ignore)
                                                ignore__680.clone().invoke(&[])
                                            }
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (node (quote transform) transform)
                                node__681
                                    .clone()
                                    .invoke(&[Scm::symbol("transform"), transform__678.get()])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn boxify_minus_abstraction(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let vars__683 = args[0].clone();
        let var_star___682 = args[1].clone();
        let body__684 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? var*)
                imports::null_p(&[var_star___682.clone()])
            })
            .is_true()
            {
                {
                    // (make-abstraction vars (boxify body))
                    imports::make_minus_abstraction(&[vars__683.clone(), {
                        // (boxify body)
                        boxify(&[body__684.clone()])
                    }])
                }
            } else if ({
                // (variable-mutable? (car var*))
                imports::variable_minus_mutable_p(&[{
                    // (car var*)
                    imports::car(&[var_star___682.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        imports::local_minus_boxify_i(&[{
                            // (car var*)
                            imports::car(&[var_star___682.clone()])
                        }])
                    };
                    {
                        // (boxify-abstraction vars (cdr var*) (make-boxify (car var*) body))
                        Scm::func(boxify_minus_abstraction).invoke(&[
                            vars__683.clone(),
                            {
                                // (cdr var*)
                                imports::cdr(&[var_star___682.clone()])
                            },
                            {
                                // (make-boxify (car var*) body)
                                imports::make_minus_boxify(&[
                                    {
                                        // (car var*)
                                        imports::car(&[var_star___682.clone()])
                                    },
                                    body__684.clone(),
                                ])
                            },
                        ])
                    }
                }
            } else {
                {
                    // (boxify-abstraction vars (cdr var*) body)
                    Scm::func(boxify_minus_abstraction).invoke(&[
                        vars__683.clone(),
                        {
                            // (cdr var*)
                            imports::cdr(&[var_star___682.clone()])
                        },
                        body__684.clone(),
                    ])
                }
            }
        }
    }
    .into()
}
pub fn boxify_minus_fixlet(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let vars__689 = args[0].clone();
        let args__690 = args[1].clone();
        let body__691 = args[2].clone();
        {
            // (make-fixlet vars args (boxify-vars! vars (boxify body)))
            imports::make_minus_fixlet(&[vars__689.clone(), args__690.clone(), {
                // (boxify-vars! vars (boxify body))
                Scm::func(boxify_minus_vars_i).invoke(&[vars__689.clone(), {
                    // (boxify body)
                    boxify(&[body__691.clone()])
                }])
            }])
        }
    }
    .into()
}
pub fn boxify_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {
        if args.len() != 4 {
            panic!("invalid arity")
        }
        let vars__686 = args[0].clone();
        let varvar__687 = args[1].clone();
        let var_star___685 = args[2].clone();
        let body__688 = args[3].clone();
        if ({
            // (null? var*)
            imports::null_p(&[var_star___685.clone()])
        })
        .is_true()
        {
            {
                // (make-vararg-abstraction vars varvar (boxify body))
                imports::make_minus_vararg_minus_abstraction(&[
                    vars__686.clone(),
                    varvar__687.clone(),
                    {
                        // (boxify body)
                        boxify(&[body__688.clone()])
                    },
                ])
            }
        } else if ({
            // (variable-mutable? (car var*))
            imports::variable_minus_mutable_p(&[{
                // (car var*)
                imports::car(&[var_star___685.clone()])
            }])
        })
        .is_true()
        {
            {
                {
                    // (local-boxify! (car var*))
                    imports::local_minus_boxify_i(&[{
                        // (car var*)
                        imports::car(&[var_star___685.clone()])
                    }])
                };
                {
                    // (boxify-vararg-abstraction vars varvar (cdr var*) (make-boxify (car var*) body))
                    Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                        vars__686.clone(),
                        varvar__687.clone(),
                        {
                            // (cdr var*)
                            imports::cdr(&[var_star___685.clone()])
                        },
                        {
                            // (make-boxify (car var*) body)
                            imports::make_minus_boxify(&[
                                {
                                    // (car var*)
                                    imports::car(&[var_star___685.clone()])
                                },
                                body__688.clone(),
                            ])
                        },
                    ])
                }
            }
        } else {
            {
                // (boxify-vararg-abstraction vars varvar (cdr var*) body)
                Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                    vars__686.clone(),
                    varvar__687.clone(),
                    {
                        // (cdr var*)
                        imports::cdr(&[var_star___685.clone()])
                    },
                    body__688.clone(),
                ])
            }
        }
    }
    .into()
}
pub fn boxify_minus_vars_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var_star___692 = args[0].clone();
        let body__693 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? var*)
                imports::null_p(&[var_star___692.clone()])
            })
            .is_true()
            {
                body__693.clone()
            } else if ({
                // (variable-mutable? (car var*))
                imports::variable_minus_mutable_p(&[{
                    // (car var*)
                    imports::car(&[var_star___692.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        imports::local_minus_boxify_i(&[{
                            // (car var*)
                            imports::car(&[var_star___692.clone()])
                        }])
                    };
                    {
                        // (boxify-vars! (cdr var*) (make-boxify (car var*) body))
                        Scm::func(boxify_minus_vars_i).invoke(&[
                            {
                                // (cdr var*)
                                imports::cdr(&[var_star___692.clone()])
                            },
                            {
                                // (make-boxify (car var*) body)
                                imports::make_minus_boxify(&[
                                    {
                                        // (car var*)
                                        imports::car(&[var_star___692.clone()])
                                    },
                                    body__693.clone(),
                                ])
                            },
                        ])
                    }
                }
            } else {
                {
                    // (boxify-vars! (cdr var*) body)
                    Scm::func(boxify_minus_vars_i).invoke(&[
                        {
                            // (cdr var*)
                            imports::cdr(&[var_star___692.clone()])
                        },
                        body__693.clone(),
                    ])
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
            // (define (boxify node) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-abstraction vars var* body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-vararg-abstraction vars varvar var* body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-fixlet vars args body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-vars! var* body) ...)
            (/*NOP*/)
        }
    };
}
