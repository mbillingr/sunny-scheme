#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment exp (astify-and (and-args exp) env tail?))
            Scm::func(imports::astify_minus_comment).invoke(&[exp.clone(), {
                // (astify-and (and-args exp) env tail?)
                Scm::func(imports::astify_minus_and).invoke(&[
                    {
                        // (and-args exp)
                        Scm::func(imports::and_minus_args).invoke(&[exp.clone()])
                    },
                    env.clone(),
                    tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-assert (assert-condition exp) env)
            Scm::func(imports::astify_minus_assert).invoke(&[
                {
                    // (assert-condition exp)
                    Scm::func(imports::assert_minus_condition).invoke(&[exp.clone()])
                },
                env.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-sequence (begin-statements exp) env tail?)
            Scm::func(imports::astify_minus_sequence).invoke(&[
                {
                    // (begin-statements exp)
                    Scm::func(imports::begin_minus_statements).invoke(&[exp.clone()])
                },
                env.clone(),
                tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment (quote (cond ...)) (astify-cond (cond-clauses exp) env tail?))
            Scm::func(imports::astify_minus_comment).invoke(&[
                Scm::pair(Scm::symbol("cond"), Scm::pair(Scm::symbol("..."), Scm::Nil)),
                {
                    // (astify-cond (cond-clauses exp) env tail?)
                    Scm::func(imports::astify_minus_cond).invoke(&[
                        {
                            // (cond-clauses exp)
                            Scm::func(imports::cond_minus_clauses).invoke(&[exp.clone()])
                        },
                        env.clone(),
                        tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment (cons (quote define) (definition-signature exp)) (astify-definition (definition-variable exp) (definition-value exp) env))
            Scm::func(imports::astify_minus_comment).invoke(&[
                {
                    // (cons (quote define) (definition-signature exp))
                    Scm::func(imports::cons).invoke(&[Scm::symbol("define"), {
                        // (definition-signature exp)
                        Scm::func(imports::definition_minus_signature).invoke(&[exp.clone()])
                    }])
                },
                {
                    // (astify-definition (definition-variable exp) (definition-value exp) env)
                    Scm::func(imports::astify_minus_definition).invoke(&[
                        {
                            // (definition-variable exp)
                            Scm::func(imports::definition_minus_variable).invoke(&[exp.clone()])
                        },
                        {
                            // (definition-value exp)
                            Scm::func(imports::definition_minus_value).invoke(&[exp.clone()])
                        },
                        env.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
            Scm::func(imports::astify_minus_alternative).invoke(&[
                {
                    // (if-condition exp)
                    Scm::func(imports::if_minus_condition).invoke(&[exp.clone()])
                },
                {
                    // (if-consequence exp)
                    Scm::func(imports::if_minus_consequence).invoke(&[exp.clone()])
                },
                {
                    // (if-alternative exp)
                    Scm::func(imports::if_minus_alternative).invoke(&[exp.clone()])
                },
                env.clone(),
                tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-abstraction (lambda-params exp) (lambda-body exp) env)
            Scm::func(imports::astify_minus_abstraction).invoke(&[
                {
                    // (lambda-params exp)
                    Scm::func(imports::lambda_minus_params).invoke(&[exp.clone()])
                },
                {
                    // (lambda-body exp)
                    Scm::func(imports::lambda_minus_body).invoke(&[exp.clone()])
                },
                env.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment exp (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?))
            Scm::func(imports::astify_minus_comment).invoke(&[exp.clone(), {
                // (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?)
                Scm::func(imports::astify_minus_application).invoke(&[
                    {
                        // (astify-abstraction (let-vars exp) (let-body exp) env)
                        Scm::func(imports::astify_minus_abstraction).invoke(&[
                            {
                                // (let-vars exp)
                                Scm::func(imports::let_minus_vars).invoke(&[exp.clone()])
                            },
                            {
                                // (let-body exp)
                                Scm::func(imports::let_minus_body).invoke(&[exp.clone()])
                            },
                            env.clone(),
                        ])
                    },
                    {
                        // (let-args exp)
                        Scm::func(imports::let_minus_args).invoke(&[exp.clone()])
                    },
                    env.clone(),
                    tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (letrec ((loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings))))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings)))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?))))
                {
                    let loop_ = Scm::symbol("*uninitialized*");
                    {
                        let loop_ = loop_.into_boxed();
                        {
                            loop_.set({
                                // Closure
                                let exp = exp.clone();
                                let loop_ = loop_.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let bindings = args[0].clone();
                                    if ({
                                        // (null? bindings)
                                        Scm::func(imports::null_p).invoke(&[bindings.clone()])
                                    })
                                    .is_true()
                                    {
                                        {
                                            // (cons (quote begin) (let-body exp))
                                            Scm::func(imports::cons).invoke(&[
                                                Scm::symbol("begin"),
                                                {
                                                    // (let-body exp)
                                                    Scm::func(imports::let_minus_body)
                                                        .invoke(&[exp.clone()])
                                                },
                                            ])
                                        }
                                    } else {
                                        {
                                            // (list (quote let) (list (car bindings)) (loop (cdr bindings)))
                                            Scm::func(imports::list).invoke(&[
                                                Scm::symbol("let"),
                                                {
                                                    // (list (car bindings))
                                                    Scm::func(imports::list).invoke(&[{
                                                        // (car bindings)
                                                        Scm::func(imports::car)
                                                            .invoke(&[bindings.clone()])
                                                    }])
                                                },
                                                {
                                                    // (loop (cdr bindings))
                                                    loop_.get().invoke(&[{
                                                        // (cdr bindings)
                                                        Scm::func(imports::cdr)
                                                            .invoke(&[bindings.clone()])
                                                    }])
                                                },
                                            ])
                                        }
                                    }
                                })
                            });
                            {
                                // (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?))
                                Scm::func(imports::astify_minus_comment).invoke(&[exp.clone(), {
                                    // (expand-let (loop (let*-bindings exp)) env tail?)
                                    Scm::func(expand_minus_let).invoke(&[
                                        {
                                            // (loop (let*-bindings exp))
                                            loop_.get().invoke(&[{
                                                // (let*-bindings exp)
                                                Scm::func(imports::let_star__minus_bindings)
                                                    .invoke(&[exp.clone()])
                                            }])
                                        },
                                        env.clone(),
                                        tail_p.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment exp (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?))
            Scm::func(imports::astify_minus_comment).invoke(&[exp.clone(), {
                // (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?)
                Scm::func(expand_minus_let).invoke(&[
                    {
                        // (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))))
                        Scm::func(imports::list).invoke(&[
                            Scm::symbol("let"),
                            {
                                // (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp))
                                Scm::func(imports::map).invoke(&[
                                    {
                                        // Closure
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let name = args[0].clone();
                                            {
                                                // (list name (quote (quote *uninitialized*)))
                                                Scm::func(imports::list).invoke(&[
                                                    name.clone(),
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
                                        Scm::func(imports::let_minus_vars).invoke(&[exp.clone()])
                                    },
                                ])
                            },
                            {
                                // (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))
                                Scm::func(imports::cons).invoke(&[Scm::symbol("begin"), {
                                    // (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))
                                    Scm::func(imports::append).invoke(&[
                                        {
                                            // (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp))
                                            Scm::func(imports::map).invoke(&[
                                                {
                                                    // Closure
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 2 {
                                                            panic!("invalid arity")
                                                        }
                                                        let name = args[0].clone();
                                                        let val = args[1].clone();
                                                        {
                                                            // (list (quote set!) name val)
                                                            Scm::func(imports::list).invoke(&[
                                                                Scm::symbol("set!"),
                                                                name.clone(),
                                                                val.clone(),
                                                            ])
                                                        }
                                                    })
                                                },
                                                {
                                                    // (let-vars exp)
                                                    Scm::func(imports::let_minus_vars)
                                                        .invoke(&[exp.clone()])
                                                },
                                                {
                                                    // (let-args exp)
                                                    Scm::func(imports::let_minus_args)
                                                        .invoke(&[exp.clone()])
                                                },
                                            ])
                                        },
                                        {
                                            // (let-body exp)
                                            Scm::func(imports::let_minus_body)
                                                .invoke(&[exp.clone()])
                                        },
                                    ])
                                }])
                            },
                        ])
                    },
                    env.clone(),
                    tail_p.clone(),
                ])
            }])
        }
    }
    .into()
}
pub fn expand_minus_quote(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-constant (cadr exp) env)
            Scm::func(imports::astify_minus_constant).invoke(&[
                {
                    // (cadr exp)
                    Scm::func(imports::cadr).invoke(&[exp.clone()])
                },
                env.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-assignment (set!-variable exp) (set!-value exp) env)
            Scm::func(imports::astify_minus_assignment).invoke(&[
                {
                    // (set!-variable exp)
                    Scm::func(imports::set_i_minus_variable).invoke(&[exp.clone()])
                },
                {
                    // (set!-value exp)
                    Scm::func(imports::set_i_minus_value).invoke(&[exp.clone()])
                },
                env.clone(),
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
        let exp = args[0].clone();
        let env = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-testsuite (testsuite-name exp) (testsuite-cases exp) env)
            Scm::func(imports::astify_minus_testsuite).invoke(&[
                {
                    // (testsuite-name exp)
                    Scm::func(imports::testsuite_minus_name).invoke(&[exp.clone()])
                },
                {
                    // (testsuite-cases exp)
                    Scm::func(imports::testsuite_minus_cases).invoke(&[exp.clone()])
                },
                env.clone(),
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
            // (list (quote GLOBAL-MARKER) (new-keyword (quote and) expand-and) (new-keyword (quote assert) expand-assert) (new-keyword (quote begin) expand-begin) (new-keyword (quote cond) expand-cond) (new-keyword (quote define) expand-define) (new-keyword (quote if) expand-if) (new-keyword (quote lambda) expand-lambda) (new-keyword (quote let) expand-let) (new-keyword (quote let*) expand-let*) (new-keyword (quote letrec) expand-letrec) (new-keyword (quote quote) expand-quote) (new-keyword (quote set!) expand-set!) (new-keyword (quote testsuite) expand-testsuite))
            Scm::func(imports::list).invoke(&[
                Scm::symbol("GLOBAL-MARKER"),
                {
                    // (new-keyword (quote and) expand-and)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("and"), Scm::func(expand_minus_and)])
                },
                {
                    // (new-keyword (quote assert) expand-assert)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("assert"), Scm::func(expand_minus_assert)])
                },
                {
                    // (new-keyword (quote begin) expand-begin)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("begin"), Scm::func(expand_minus_begin)])
                },
                {
                    // (new-keyword (quote cond) expand-cond)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("cond"), Scm::func(expand_minus_cond)])
                },
                {
                    // (new-keyword (quote define) expand-define)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("define"), Scm::func(expand_minus_define)])
                },
                {
                    // (new-keyword (quote if) expand-if)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("if"), Scm::func(expand_minus_if)])
                },
                {
                    // (new-keyword (quote lambda) expand-lambda)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("lambda"), Scm::func(expand_minus_lambda)])
                },
                {
                    // (new-keyword (quote let) expand-let)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("let"), Scm::func(expand_minus_let)])
                },
                {
                    // (new-keyword (quote let*) expand-let*)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("let*"), Scm::func(expand_minus_let_star_)])
                },
                {
                    // (new-keyword (quote letrec) expand-letrec)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("letrec"), Scm::func(expand_minus_letrec)])
                },
                {
                    // (new-keyword (quote quote) expand-quote)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("quote"), Scm::func(expand_minus_quote)])
                },
                {
                    // (new-keyword (quote set!) expand-set!)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("set!"), Scm::func(expand_minus_set_i)])
                },
                {
                    // (new-keyword (quote testsuite) expand-testsuite)
                    Scm::func(imports::new_minus_keyword)
                        .invoke(&[Scm::symbol("testsuite"), Scm::func(expand_minus_testsuite)])
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
