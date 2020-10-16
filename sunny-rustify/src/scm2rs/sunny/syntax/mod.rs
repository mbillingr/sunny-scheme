#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::astify::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::make_minus_core_minus_env;
}

pub fn expand_minus_and(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_4 = args[0].clone();
        let env_36 = args[1].clone();
        let tail_p_10 = args[2].clone();
        {
            // (astify-comment exp (astify-and (and-args exp) env tail?))
            imports::astify_minus_comment(&[exp_4.clone(), {
                // (astify-and (and-args exp) env tail?)
                imports::astify_minus_and(&[
                    {
                        // (and-args exp)
                        imports::and_minus_args(&[exp_4.clone()])
                    },
                    env_36.clone(),
                    tail_p_10.clone(),
                ])
            }])
        }
    }
    .into()
}
pub fn expand_minus_assert(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_5 = args[0].clone();
        let env_37 = args[1].clone();
        let tail_p_11 = args[2].clone();
        {
            // (astify-assert (assert-condition exp) env)
            imports::astify_minus_assert(&[
                {
                    // (assert-condition exp)
                    imports::assert_minus_condition(&[exp_5.clone()])
                },
                env_37.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_begin(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_6 = args[0].clone();
        let env_38 = args[1].clone();
        let tail_p_12 = args[2].clone();
        {
            // (astify-sequence (begin-statements exp) env tail?)
            imports::astify_minus_sequence(&[
                {
                    // (begin-statements exp)
                    imports::begin_minus_statements(&[exp_6.clone()])
                },
                env_38.clone(),
                tail_p_12.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_cond(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_7 = args[0].clone();
        let env_39 = args[1].clone();
        let tail_p_13 = args[2].clone();
        {
            // (astify-comment (quote (cond ...)) (astify-cond (cond-clauses exp) env tail?))
            imports::astify_minus_comment(&[
                Scm::pair(Scm::symbol("cond"), Scm::pair(Scm::symbol("..."), Scm::Nil)),
                {
                    // (astify-cond (cond-clauses exp) env tail?)
                    imports::astify_minus_cond(&[
                        {
                            // (cond-clauses exp)
                            imports::cond_minus_clauses(&[exp_7.clone()])
                        },
                        env_39.clone(),
                        tail_p_13.clone(),
                    ])
                },
            ])
        }
    }
    .into()
}
pub fn expand_minus_define(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_8 = args[0].clone();
        let env_40 = args[1].clone();
        let tail_p_14 = args[2].clone();
        {
            // (astify-comment (cons (quote define) (definition-signature exp)) (astify-definition (definition-variable exp) (definition-value exp) env))
            imports::astify_minus_comment(&[
                {
                    // (cons (quote define) (definition-signature exp))
                    imports::cons(&[Scm::symbol("define"), {
                        // (definition-signature exp)
                        imports::definition_minus_signature(&[exp_8.clone()])
                    }])
                },
                {
                    // (astify-definition (definition-variable exp) (definition-value exp) env)
                    imports::astify_minus_definition(&[
                        {
                            // (definition-variable exp)
                            imports::definition_minus_variable(&[exp_8.clone()])
                        },
                        {
                            // (definition-value exp)
                            imports::definition_minus_value(&[exp_8.clone()])
                        },
                        env_40.clone(),
                    ])
                },
            ])
        }
    }
    .into()
}
pub fn expand_minus_if(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_9 = args[0].clone();
        let env_41 = args[1].clone();
        let tail_p_15 = args[2].clone();
        {
            // (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
            imports::astify_minus_alternative(&[
                {
                    // (if-condition exp)
                    imports::if_minus_condition(&[exp_9.clone()])
                },
                {
                    // (if-consequence exp)
                    imports::if_minus_consequence(&[exp_9.clone()])
                },
                {
                    // (if-alternative exp)
                    imports::if_minus_alternative(&[exp_9.clone()])
                },
                env_41.clone(),
                tail_p_15.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_lambda(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_10 = args[0].clone();
        let env_42 = args[1].clone();
        let tail_p_16 = args[2].clone();
        {
            // (astify-abstraction (lambda-params exp) (lambda-body exp) env)
            imports::astify_minus_abstraction(&[
                {
                    // (lambda-params exp)
                    imports::lambda_minus_params(&[exp_10.clone()])
                },
                {
                    // (lambda-body exp)
                    imports::lambda_minus_body(&[exp_10.clone()])
                },
                env_42.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_let(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_11 = args[0].clone();
        let env_43 = args[1].clone();
        let tail_p_17 = args[2].clone();
        {
            // (astify-comment exp (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?))
            imports::astify_minus_comment(&[exp_11.clone(), {
                // (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?)
                imports::astify_minus_application(&[
                    {
                        // (astify-abstraction (let-vars exp) (let-body exp) env)
                        imports::astify_minus_abstraction(&[
                            {
                                // (let-vars exp)
                                imports::let_minus_vars(&[exp_11.clone()])
                            },
                            {
                                // (let-body exp)
                                imports::let_minus_body(&[exp_11.clone()])
                            },
                            env_43.clone(),
                        ])
                    },
                    {
                        // (let-args exp)
                        imports::let_minus_args(&[exp_11.clone()])
                    },
                    env_43.clone(),
                    tail_p_17.clone(),
                ])
            }])
        }
    }
    .into()
}
pub fn expand_minus_let_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_12 = args[0].clone();
        let env_44 = args[1].clone();
        let tail_p_18 = args[2].clone();
        {
            // (letrec ((loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings))))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings)))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?))))
                {
                    let loop__2 = Scm::symbol("*uninitialized*");
                    {
                        let loop__2 = loop__2.into_boxed();
                        {
                            loop__2.set({
                                // Closure
                                let exp_12 = exp_12.clone();
                                let loop__2 = loop__2.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let bindings_0 = args[0].clone();
                                    if ({
                                        // (null? bindings)
                                        imports::null_p(&[bindings_0.clone()])
                                    })
                                    .is_true()
                                    {
                                        {
                                            // (cons (quote begin) (let-body exp))
                                            imports::cons(&[Scm::symbol("begin"), {
                                                // (let-body exp)
                                                imports::let_minus_body(&[exp_12.clone()])
                                            }])
                                        }
                                    } else {
                                        {
                                            // (list (quote let) (list (car bindings)) (loop (cdr bindings)))
                                            imports::list(&[
                                                Scm::symbol("let"),
                                                {
                                                    // (list (car bindings))
                                                    imports::list(&[{
                                                        // (car bindings)
                                                        imports::car(&[bindings_0.clone()])
                                                    }])
                                                },
                                                {
                                                    // (loop (cdr bindings))
                                                    loop__2.get().invoke(&[{
                                                        // (cdr bindings)
                                                        imports::cdr(&[bindings_0.clone()])
                                                    }])
                                                },
                                            ])
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?))
                                imports::astify_minus_comment(&[exp_12.clone(), {
                                    // (expand-let (loop (let*-bindings exp)) env tail?)
                                    expand_minus_let(&[
                                        {
                                            // (loop (let*-bindings exp))
                                            loop__2.get().invoke(&[{
                                                // (let*-bindings exp)
                                                imports::let_star__minus_bindings(&[exp_12.clone()])
                                            }])
                                        },
                                        env_44.clone(),
                                        tail_p_18.clone(),
                                    ])
                                }])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn expand_minus_letrec(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_13 = args[0].clone();
        let env_45 = args[1].clone();
        let tail_p_19 = args[2].clone();
        {
            // (astify-comment exp (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?))
            imports::astify_minus_comment(&[exp_13.clone(), {
                // (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?)
                expand_minus_let(&[
                    {
                        // (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))))
                        imports::list(&[
                            Scm::symbol("let"),
                            {
                                // (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp))
                                imports::map(&[
                                    {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let name_42 = args[0].clone();
                                            {
                                                // (list name (quote (quote *uninitialized*)))
                                                imports::list(&[
                                                    name_42.clone(),
                                                    Scm::pair(
                                                        Scm::symbol("quote"),
                                                        Scm::pair(
                                                            Scm::symbol("*uninitialized*"),
                                                            Scm::Nil,
                                                        ),
                                                    ),
                                                ])
                                            }
                                        })
                                    },
                                    {
                                        // (let-vars exp)
                                        imports::let_minus_vars(&[exp_13.clone()])
                                    },
                                ])
                            },
                            {
                                // (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))
                                imports::cons(&[Scm::symbol("begin"), {
                                    // (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))
                                    imports::append(&[
                                        {
                                            // (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp))
                                            imports::map(&[
                                                {
                                                    // Closure
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 2 {
                                                            panic!("invalid arity")
                                                        }
                                                        let name_43 = args[0].clone();
                                                        let val_9 = args[1].clone();
                                                        {
                                                            // (list (quote set!) name val)
                                                            imports::list(&[
                                                                Scm::symbol("set!"),
                                                                name_43.clone(),
                                                                val_9.clone(),
                                                            ])
                                                        }
                                                    })
                                                },
                                                {
                                                    // (let-vars exp)
                                                    imports::let_minus_vars(&[exp_13.clone()])
                                                },
                                                {
                                                    // (let-args exp)
                                                    imports::let_minus_args(&[exp_13.clone()])
                                                },
                                            ])
                                        },
                                        {
                                            // (let-body exp)
                                            imports::let_minus_body(&[exp_13.clone()])
                                        },
                                    ])
                                }])
                            },
                        ])
                    },
                    env_45.clone(),
                    tail_p_19.clone(),
                ])
            }])
        }
    }
    .into()
}
pub fn expand_minus_or(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_14 = args[0].clone();
        let env_46 = args[1].clone();
        let tail_p_20 = args[2].clone();
        {
            // (let ((x1 (make-syntactic-closure env (quote ()) (cadr exp))) (x2 (make-syntactic-closure env (quote ()) (caddr exp)))) (astify (make-syntactic-closure (make-core-env) (quote ()) (list (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2)) x1)) env tail?))
            {
                let [x1_0, x2_0] = [
                    {
                        // (make-syntactic-closure env (quote ()) (cadr exp))
                        imports::make_minus_syntactic_minus_closure(&[env_46.clone(), Scm::Nil, {
                            // (cadr exp)
                            imports::cadr(&[exp_14.clone()])
                        }])
                    },
                    {
                        // (make-syntactic-closure env (quote ()) (caddr exp))
                        imports::make_minus_syntactic_minus_closure(&[env_46.clone(), Scm::Nil, {
                            // (caddr exp)
                            imports::caddr(&[exp_14.clone()])
                        }])
                    },
                ];
                {
                    // (astify (make-syntactic-closure (make-core-env) (quote ()) (list (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2)) x1)) env tail?)
                    imports::astify(&[
                        {
                            // (make-syntactic-closure (make-core-env) (quote ()) (list (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2)) x1))
                            imports::make_minus_syntactic_minus_closure(&[
                                {
                                    // (make-core-env)
                                    make_minus_core_minus_env(&[])
                                },
                                Scm::Nil,
                                {
                                    // (list (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2)) x1)
                                    imports::list(&[
                                        {
                                            // (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2))
                                            imports::list(&[
                                                Scm::symbol("lambda"),
                                                {
                                                    // (list (quote tmp))
                                                    imports::list(&[Scm::symbol("tmp")])
                                                },
                                                {
                                                    // (list (quote if) (quote tmp) (quote tmp) x2)
                                                    imports::list(&[
                                                        Scm::symbol("if"),
                                                        Scm::symbol("tmp"),
                                                        Scm::symbol("tmp"),
                                                        x2_0.clone(),
                                                    ])
                                                },
                                            ])
                                        },
                                        x1_0.clone(),
                                    ])
                                },
                            ])
                        },
                        env_46.clone(),
                        tail_p_20.clone(),
                    ])
                }
            }
        }
    }
    .into()
}
pub fn expand_minus_quote(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_15 = args[0].clone();
        let env_47 = args[1].clone();
        let tail_p_21 = args[2].clone();
        {
            // (astify-constant (cadr exp) env)
            imports::astify_minus_constant(&[
                {
                    // (cadr exp)
                    imports::cadr(&[exp_15.clone()])
                },
                env_47.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_set_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_16 = args[0].clone();
        let env_48 = args[1].clone();
        let tail_p_22 = args[2].clone();
        {
            // (astify-assignment (set!-variable exp) (set!-value exp) env)
            imports::astify_minus_assignment(&[
                {
                    // (set!-variable exp)
                    imports::set_i_minus_variable(&[exp_16.clone()])
                },
                {
                    // (set!-value exp)
                    imports::set_i_minus_value(&[exp_16.clone()])
                },
                env_48.clone(),
            ])
        }
    }
    .into()
}
pub fn expand_minus_testsuite(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp_17 = args[0].clone();
        let env_49 = args[1].clone();
        let tail_p_23 = args[2].clone();
        {
            // (astify-testsuite (testsuite-name exp) (testsuite-cases exp) env)
            imports::astify_minus_testsuite(&[
                {
                    // (testsuite-name exp)
                    imports::testsuite_minus_name(&[exp_17.clone()])
                },
                {
                    // (testsuite-cases exp)
                    imports::testsuite_minus_cases(&[exp_17.clone()])
                },
                env_49.clone(),
            ])
        }
    }
    .into()
}
pub fn make_minus_core_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (list (quote GLOBAL-MARKER) (new-keyword (quote and) expand-and) (new-keyword (quote assert) expand-assert) (new-keyword (quote begin) expand-begin) (new-keyword (quote cond) expand-cond) (new-keyword (quote define) expand-define) (new-keyword (quote if) expand-if) (new-keyword (quote lambda) expand-lambda) (new-keyword (quote let) expand-let) (new-keyword (quote let*) expand-let*) (new-keyword (quote letrec) expand-letrec) (new-keyword (quote or) expand-or) (new-keyword (quote quote) expand-quote) (new-keyword (quote set!) expand-set!) (new-keyword (quote testsuite) expand-testsuite))
            imports::list(&[
                Scm::symbol("GLOBAL-MARKER"),
                {
                    // (new-keyword (quote and) expand-and)
                    imports::new_minus_keyword(&[Scm::symbol("and"), Scm::func(expand_minus_and)])
                },
                {
                    // (new-keyword (quote assert) expand-assert)
                    imports::new_minus_keyword(&[
                        Scm::symbol("assert"),
                        Scm::func(expand_minus_assert),
                    ])
                },
                {
                    // (new-keyword (quote begin) expand-begin)
                    imports::new_minus_keyword(&[
                        Scm::symbol("begin"),
                        Scm::func(expand_minus_begin),
                    ])
                },
                {
                    // (new-keyword (quote cond) expand-cond)
                    imports::new_minus_keyword(&[Scm::symbol("cond"), Scm::func(expand_minus_cond)])
                },
                {
                    // (new-keyword (quote define) expand-define)
                    imports::new_minus_keyword(&[
                        Scm::symbol("define"),
                        Scm::func(expand_minus_define),
                    ])
                },
                {
                    // (new-keyword (quote if) expand-if)
                    imports::new_minus_keyword(&[Scm::symbol("if"), Scm::func(expand_minus_if)])
                },
                {
                    // (new-keyword (quote lambda) expand-lambda)
                    imports::new_minus_keyword(&[
                        Scm::symbol("lambda"),
                        Scm::func(expand_minus_lambda),
                    ])
                },
                {
                    // (new-keyword (quote let) expand-let)
                    imports::new_minus_keyword(&[Scm::symbol("let"), Scm::func(expand_minus_let)])
                },
                {
                    // (new-keyword (quote let*) expand-let*)
                    imports::new_minus_keyword(&[
                        Scm::symbol("let*"),
                        Scm::func(expand_minus_let_star_),
                    ])
                },
                {
                    // (new-keyword (quote letrec) expand-letrec)
                    imports::new_minus_keyword(&[
                        Scm::symbol("letrec"),
                        Scm::func(expand_minus_letrec),
                    ])
                },
                {
                    // (new-keyword (quote or) expand-or)
                    imports::new_minus_keyword(&[Scm::symbol("or"), Scm::func(expand_minus_or)])
                },
                {
                    // (new-keyword (quote quote) expand-quote)
                    imports::new_minus_keyword(&[
                        Scm::symbol("quote"),
                        Scm::func(expand_minus_quote),
                    ])
                },
                {
                    // (new-keyword (quote set!) expand-set!)
                    imports::new_minus_keyword(&[
                        Scm::symbol("set!"),
                        Scm::func(expand_minus_set_i),
                    ])
                },
                {
                    // (new-keyword (quote testsuite) expand-testsuite)
                    imports::new_minus_keyword(&[
                        Scm::symbol("testsuite"),
                        Scm::func(expand_minus_testsuite),
                    ])
                },
            ])
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
    crate::scheme::cxr::initialize();
    crate::sunny::astify::initialize();
    crate::sunny::env::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (make-core-env) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-and exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-assert exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-begin exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-cond exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-define exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-if exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-lambda exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-let exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-let* exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-letrec exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-or exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-quote exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-set! exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (expand-testsuite exp env tail?) ...)
            (/*NOP*/)
        }
    };
}
