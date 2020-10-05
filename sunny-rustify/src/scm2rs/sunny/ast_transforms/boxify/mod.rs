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
        let node = args[0].clone();
        {
            // (letrec ((transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-params)) (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore)))))) (node (quote transform) transform))
            {
                // (let ((transform (quote *uninitialized*))) (begin (set! transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) ((eq? (node (quote kind)) (quote FIXLET)) (boxify-fixlet (node (quote get-params)) (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))) (else (ignore))))) (node (quote transform) transform)))
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
                                    let ignore = args[1].clone();
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
                                                // (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))
                                                Scm::func(boxify_minus_abstraction).invoke(&[
                                                    {
                                                        // (node (quote get-params))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-params")])
                                                    },
                                                    {
                                                        // (node (quote get-vars))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-params))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-params")])
                                                    },
                                                    {
                                                        // (node (quote get-vars))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-body")])
                                                    },
                                                ])
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
                                                // (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))
                                                Scm::func(boxify_minus_vararg_minus_abstraction)
                                                    .invoke(&[
                                                        {
                                                            // (node (quote get-params))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-params",
                                                            )])
                                                        },
                                                        {
                                                            // (node (quote get-vararg))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-vararg",
                                                            )])
                                                        },
                                                        {
                                                            // (node (quote get-vars))
                                                            node.clone()
                                                                .invoke(&[Scm::symbol("get-vars")])
                                                        },
                                                        {
                                                            // (node (quote get-varvar))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-varvar",
                                                            )])
                                                        },
                                                        {
                                                            // (cons (node (quote get-vararg)) (node (quote get-params)))
                                                            Scm::func(imports::cons).invoke(&[
                                                                {
                                                                    // (node (quote get-vararg))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-vararg"),
                                                                    ])
                                                                },
                                                                {
                                                                    // (node (quote get-params))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-params"),
                                                                    ])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                            Scm::func(imports::cons).invoke(&[
                                                                {
                                                                    // (node (quote get-varvar))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-varvar"),
                                                                    ])
                                                                },
                                                                {
                                                                    // (node (quote get-vars))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-vars"),
                                                                    ])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (node (quote get-body))
                                                            node.clone()
                                                                .invoke(&[Scm::symbol("get-body")])
                                                        },
                                                    ])
                                            }
                                        } else if ({
                                            // (eq? (node (quote kind)) (quote FIXLET))
                                            Scm::func(imports::eq_p).invoke(&[
                                                {
                                                    // (node (quote kind))
                                                    node.clone().invoke(&[Scm::symbol("kind")])
                                                },
                                                Scm::symbol("FIXLET"),
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (boxify-fixlet (node (quote get-params)) (node (quote get-vars)) (node (quote get-args)) (node (quote get-body)))
                                                Scm::func(boxify_minus_fixlet).invoke(&[
                                                    {
                                                        // (node (quote get-params))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-params")])
                                                    },
                                                    {
                                                        // (node (quote get-vars))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-vars")])
                                                    },
                                                    {
                                                        // (node (quote get-args))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-args")])
                                                    },
                                                    {
                                                        // (node (quote get-body))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-body")])
                                                    },
                                                ])
                                            }
                                        } else {
                                            {
                                                // (ignore)
                                                ignore.clone().invoke(&[])
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
pub fn boxify_minus_abstraction(args: &[Scm]) -> Scm {
    {
        if args.len() != 5 {
            panic!("invalid arity")
        }
        let params = args[0].clone();
        let vars = args[1].clone();
        let param_star_ = args[2].clone();
        let var_star_ = args[3].clone();
        let body = args[4].clone();
        {
            // (cond ...)
            if ({
                // (and (null? param*) (null? var*))
                if ({
                    // (null? param*)
                    Scm::func(imports::null_p).invoke(&[param_star_.clone()])
                })
                .is_true()
                {
                    {
                        // (null? var*)
                        Scm::func(imports::null_p).invoke(&[var_star_.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (make-abstraction params vars (boxify body))
                    Scm::func(imports::make_minus_abstraction).invoke(&[
                        params.clone(),
                        vars.clone(),
                        {
                            // (boxify body)
                            Scm::func(boxify).invoke(&[body.clone()])
                        },
                    ])
                }
            } else if ({
                // (null? param*)
                Scm::func(imports::null_p).invoke(&[param_star_.clone()])
            })
            .is_true()
            {
                {
                    // (error "mismatch params/vars")
                    Scm::func(imports::error).invoke(&[Scm::from("mismatch params/vars")])
                }
            } else if ({
                // (null? var*)
                Scm::func(imports::null_p).invoke(&[var_star_.clone()])
            })
            .is_true()
            {
                {
                    // (error "mismatch params/vars")
                    Scm::func(imports::error).invoke(&[Scm::from("mismatch params/vars")])
                }
            } else if ({
                // (variable-mutable? (car var*))
                Scm::func(imports::variable_minus_mutable_p).invoke(&[{
                    // (car var*)
                    Scm::func(imports::car).invoke(&[var_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        Scm::func(imports::local_minus_boxify_i).invoke(&[{
                            // (car var*)
                            Scm::func(imports::car).invoke(&[var_star_.clone()])
                        }])
                    };
                    {
                        // (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))
                        Scm::func(boxify_minus_abstraction).invoke(&[
                            params.clone(),
                            vars.clone(),
                            {
                                // (cdr param*)
                                Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                            },
                            {
                                // (cdr var*)
                                Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                            },
                            {
                                // (make-boxify (car param*) body)
                                Scm::func(imports::make_minus_boxify).invoke(&[
                                    {
                                        // (car param*)
                                        Scm::func(imports::car).invoke(&[param_star_.clone()])
                                    },
                                    body.clone(),
                                ])
                            },
                        ])
                    }
                }
            } else {
                {
                    // (boxify-abstraction params vars (cdr param*) (cdr var*) body)
                    Scm::func(boxify_minus_abstraction).invoke(&[
                        params.clone(),
                        vars.clone(),
                        {
                            // (cdr param*)
                            Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                        },
                        {
                            // (cdr var*)
                            Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                        },
                        body.clone(),
                    ])
                }
            }
        }
    }
    .into()
}
pub fn boxify_minus_fixlet(args: &[Scm]) -> Scm {
    {
        if args.len() != 4 {
            panic!("invalid arity")
        }
        let params = args[0].clone();
        let vars = args[1].clone();
        let args_ = args[2].clone();
        let body = args[3].clone();
        {
            // (make-fixlet params vars args (boxify-vars! params vars (boxify body)))
            Scm::func(imports::make_minus_fixlet).invoke(&[
                params.clone(),
                vars.clone(),
                args_.clone(),
                {
                    // (boxify-vars! params vars (boxify body))
                    Scm::func(boxify_minus_vars_i).invoke(&[params.clone(), vars.clone(), {
                        // (boxify body)
                        Scm::func(boxify).invoke(&[body.clone()])
                    }])
                },
            ])
        }
    }
    .into()
}
pub fn boxify_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {
        if args.len() != 7 {
            panic!("invalid arity")
        }
        let params = args[0].clone();
        let vararg = args[1].clone();
        let vars = args[2].clone();
        let varvar = args[3].clone();
        let param_star_ = args[4].clone();
        let var_star_ = args[5].clone();
        let body = args[6].clone();
        if ({
            // (null? var*)
            Scm::func(imports::null_p).invoke(&[var_star_.clone()])
        })
        .is_true()
        {
            {
                // (make-vararg-abstraction params vararg vars varvar (boxify body))
                Scm::func(imports::make_minus_vararg_minus_abstraction).invoke(&[
                    params.clone(),
                    vararg.clone(),
                    vars.clone(),
                    varvar.clone(),
                    {
                        // (boxify body)
                        Scm::func(boxify).invoke(&[body.clone()])
                    },
                ])
            }
        } else if ({
            // (variable-mutable? (car var*))
            Scm::func(imports::variable_minus_mutable_p).invoke(&[{
                // (car var*)
                Scm::func(imports::car).invoke(&[var_star_.clone()])
            }])
        })
        .is_true()
        {
            {
                {
                    // (local-boxify! (car var*))
                    Scm::func(imports::local_minus_boxify_i).invoke(&[{
                        // (car var*)
                        Scm::func(imports::car).invoke(&[var_star_.clone()])
                    }])
                };
                {
                    // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))
                    Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                        params.clone(),
                        vararg.clone(),
                        vars.clone(),
                        varvar.clone(),
                        {
                            // (cdr param*)
                            Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                        },
                        {
                            // (cdr var*)
                            Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                        },
                        {
                            // (make-boxify (car param*) body)
                            Scm::func(imports::make_minus_boxify).invoke(&[
                                {
                                    // (car param*)
                                    Scm::func(imports::car).invoke(&[param_star_.clone()])
                                },
                                body.clone(),
                            ])
                        },
                    ])
                }
            }
        } else {
            {
                // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body)
                Scm::func(boxify_minus_vararg_minus_abstraction).invoke(&[
                    params.clone(),
                    vararg.clone(),
                    vars.clone(),
                    varvar.clone(),
                    {
                        // (cdr param*)
                        Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                    },
                    {
                        // (cdr var*)
                        Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                    },
                    body.clone(),
                ])
            }
        }
    }
    .into()
}
pub fn boxify_minus_vars_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let param_star_ = args[0].clone();
        let var_star_ = args[1].clone();
        let body = args[2].clone();
        {
            // (cond ...)
            if ({
                // (and (null? param*) (null? var*))
                if ({
                    // (null? param*)
                    Scm::func(imports::null_p).invoke(&[param_star_.clone()])
                })
                .is_true()
                {
                    {
                        // (null? var*)
                        Scm::func(imports::null_p).invoke(&[var_star_.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                body.clone()
            } else if ({
                // (null? param*)
                Scm::func(imports::null_p).invoke(&[param_star_.clone()])
            })
            .is_true()
            {
                {
                    // (error "mismatch params/vars")
                    Scm::func(imports::error).invoke(&[Scm::from("mismatch params/vars")])
                }
            } else if ({
                // (null? var*)
                Scm::func(imports::null_p).invoke(&[var_star_.clone()])
            })
            .is_true()
            {
                {
                    // (error "mismatch params/vars")
                    Scm::func(imports::error).invoke(&[Scm::from("mismatch params/vars")])
                }
            } else if ({
                // (variable-mutable? (car var*))
                Scm::func(imports::variable_minus_mutable_p).invoke(&[{
                    // (car var*)
                    Scm::func(imports::car).invoke(&[var_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    {
                        // (local-boxify! (car var*))
                        Scm::func(imports::local_minus_boxify_i).invoke(&[{
                            // (car var*)
                            Scm::func(imports::car).invoke(&[var_star_.clone()])
                        }])
                    };
                    {
                        // (boxify-vars! (cdr param*) (cdr var*) (make-boxify (car param*) body))
                        Scm::func(boxify_minus_vars_i).invoke(&[
                            {
                                // (cdr param*)
                                Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                            },
                            {
                                // (cdr var*)
                                Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                            },
                            {
                                // (make-boxify (car param*) body)
                                Scm::func(imports::make_minus_boxify).invoke(&[
                                    {
                                        // (car param*)
                                        Scm::func(imports::car).invoke(&[param_star_.clone()])
                                    },
                                    body.clone(),
                                ])
                            },
                        ])
                    }
                }
            } else {
                {
                    // (boxify-vars! (cdr param*) (cdr var*) body)
                    Scm::func(boxify_minus_vars_i).invoke(&[
                        {
                            // (cdr param*)
                            Scm::func(imports::cdr).invoke(&[param_star_.clone()])
                        },
                        {
                            // (cdr var*)
                            Scm::func(imports::cdr).invoke(&[var_star_.clone()])
                        },
                        body.clone(),
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
            // (define (boxify-abstraction params vars param* var* body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-vararg-abstraction params vararg vars varvar param* var* body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-fixlet params vars args body) ...)
            (/*NOP*/)
        };
        {
            // (define (boxify-vars! param* var* body) ...)
            (/*NOP*/)
        }
    };
}
