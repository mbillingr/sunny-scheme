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
        let node__11 = args[0].clone();
        {
            // (letrec ((transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-vars)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore)))))) (node (quote transform) transform))
            {
                // (let ((transform (quote *uninitialized*))) (begin (set! transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-vars)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore))))) (node (quote transform) transform)))
                {
                    let transform__25 = Scm::symbol("*uninitialized*");
                    {
                        let transform__25 = transform__25.into_boxed();
                        {
                            transform__25.set({
                                // Closure
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let node__10 = args[0].clone();
                                    let ignore__1 = args[1].clone();
                                    {
                                        // (cond ...)
                                        if ({
                                            // (eq? (node (quote kind)) (quote ABSTRACTION))
                                            imports::eq_p(&[
                                                {
                                                    // (node (quote kind))
                                                    node__10.clone().invoke(&[Scm::symbol("kind")])
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
                                                        node__10
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-vars))
                                                        node__10
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node__10
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
                                                    node__10.clone().invoke(&[Scm::symbol("kind")])
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
                                                            node__10
                                                                .clone()
                                                                .invoke(&[Scm::symbol("get-vars")])
                                                        },
                                                        {
                                                            // (node (quote get-varvar))
                                                            node__10.clone().invoke(&[Scm::symbol(
                                                                "get-varvar",
                                                            )])
                                                        },
                                                        {
                                                            // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                            imports::cons(&[
                                                                {
                                                                    // (node (quote get-varvar))
                                                                    node__10.clone().invoke(&[
                                                                        Scm::symbol("get-varvar"),
                                                                    ])
                                                                },
                                                                {
                                                                    // (node (quote get-vars))
                                                                    node__10.clone().invoke(&[
                                                                        Scm::symbol("get-vars"),
                                                                    ])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (node (quote get-body))
                                                            node__10
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
                                                    node__10.clone().invoke(&[Scm::symbol("kind")])
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
                                                        node__10
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-args))
                                                        node__10
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-args")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node__10
                                                            .clone()
                                                            .invoke(&[Scm::symbol("get-body")])
                                                    },
                                                ])
                                            }
                                        } else {
                                            {
                                                // (ignore)
                                                ignore__1.clone().invoke(&[])
                                            }
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (node (quote transform) transform)
                                node__11
                                    .clone()
                                    .invoke(&[Scm::symbol("transform"), transform__25.get()])
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
        let vars__3 = args[0].clone();
        let var_star___0 = args[1].clone();
        let body__10 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? var*)
                imports::null_p(&[var_star___0.clone()])
            })
            .is_true()
            {
                {
                    // (make-abstraction vars (boxify body))
                    imports::make_minus_abstraction(&[vars__3.clone(), {
                        // (boxify body)
                        boxify(&[body__10.clone()])
                    }])
                }
            } else if ({
                // (variable-mutable? (car var*))
                imports::variable_minus_mutable_p(&[{
                    // (car var*)
                    imports::car(&[var_star___0.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        imports::local_minus_boxify_i(&[{
                            // (car var*)
                            imports::car(&[var_star___0.clone()])
                        }])
                    };
                    {
                        // (boxify-abstraction vars (cdr var*) (make-boxify (car var*) body))
                        Scm::func(boxify_minus_abstraction).invoke(&[
                            vars__3.clone(),
                            {
                                // (cdr var*)
                                imports::cdr(&[var_star___0.clone()])
                            },
                            {
                                // (make-boxify (car var*) body)
                                imports::make_minus_boxify(&[
                                    {
                                        // (car var*)
                                        imports::car(&[var_star___0.clone()])
                                    },
                                    body__10.clone(),
                                ])
                            },
                        ])
                    }
                }
            } else {
                {
                    // (boxify-abstraction vars (cdr var*) body)
                    Scm::func(boxify_minus_abstraction).invoke(&[
                        vars__3.clone(),
                        {
                            // (cdr var*)
                            imports::cdr(&[var_star___0.clone()])
                        },
                        body__10.clone(),
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
        let vars__5 = args[0].clone();
        let args__36 = args[1].clone();
        let body__12 = args[2].clone();
        {
            // (make-fixlet vars args (boxify-vars! vars (boxify body)))
            imports::make_minus_fixlet(&[vars__5.clone(), args__36.clone(), {
                // (boxify-vars! vars (boxify body))
                Scm::func(boxify_minus_vars_i).invoke(&[vars__5.clone(), {
                    // (boxify body)
                    boxify(&[body__12.clone()])
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
        let vars__4 = args[0].clone();
        let varvar__1 = args[1].clone();
        let var_star___1 = args[2].clone();
        let body__11 = args[3].clone();
        if ({
            // (null? var*)
            imports::null_p(&[var_star___1.clone()])
        })
        .is_true()
        {
            {
                // (make-vararg-abstraction vars varvar (boxify body))
                imports::make_minus_vararg_minus_abstraction(&[
                    vars__4.clone(),
                    varvar__1.clone(),
                    {
                        // (boxify body)
                        boxify(&[body__11.clone()])
                    },
                ])
            }
        } else if ({
            // (variable-mutable? (car var*))
            imports::variable_minus_mutable_p(&[{
                // (car var*)
                imports::car(&[var_star___1.clone()])
            }])
        })
        .is_true()
        {
            {
                {
                    // (local-boxify! (car var*))
                    imports::local_minus_boxify_i(&[{
                        // (car var*)
                        imports::car(&[var_star___1.clone()])
                    }])
                };
                {
                    // (boxify-vararg-abstraction vars varvar (cdr var*) (make-boxify (car var*) body))
                    Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                        vars__4.clone(),
                        varvar__1.clone(),
                        {
                            // (cdr var*)
                            imports::cdr(&[var_star___1.clone()])
                        },
                        {
                            // (make-boxify (car var*) body)
                            imports::make_minus_boxify(&[
                                {
                                    // (car var*)
                                    imports::car(&[var_star___1.clone()])
                                },
                                body__11.clone(),
                            ])
                        },
                    ])
                }
            }
        } else {
            {
                // (boxify-vararg-abstraction vars varvar (cdr var*) body)
                Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                    vars__4.clone(),
                    varvar__1.clone(),
                    {
                        // (cdr var*)
                        imports::cdr(&[var_star___1.clone()])
                    },
                    body__11.clone(),
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
        let var_star___2 = args[0].clone();
        let body__13 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? var*)
                imports::null_p(&[var_star___2.clone()])
            })
            .is_true()
            {
                body__13.clone()
            } else if ({
                // (variable-mutable? (car var*))
                imports::variable_minus_mutable_p(&[{
                    // (car var*)
                    imports::car(&[var_star___2.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        imports::local_minus_boxify_i(&[{
                            // (car var*)
                            imports::car(&[var_star___2.clone()])
                        }])
                    };
                    {
                        // (boxify-vars! (cdr var*) (make-boxify (car var*) body))
                        Scm::func(boxify_minus_vars_i).invoke(&[
                            {
                                // (cdr var*)
                                imports::cdr(&[var_star___2.clone()])
                            },
                            {
                                // (make-boxify (car var*) body)
                                imports::make_minus_boxify(&[
                                    {
                                        // (car var*)
                                        imports::car(&[var_star___2.clone()])
                                    },
                                    body__13.clone(),
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
                            imports::cdr(&[var_star___2.clone()])
                        },
                        body__13.clone(),
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
