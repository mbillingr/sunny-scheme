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
    pub use super::globals::make_minus_core_minus_env;
}

mod globals {
    use sunny_core::{Mut, Scm};
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
                imports::astify_minus_comment
                    .with(|value| value.get())
                    .invoke(&[exp.clone(), {
                        // (astify-and (and-args exp) env tail?)
                        imports::astify_minus_and
                            .with(|value| value.get())
                            .invoke(&[
                                {
                                    // (and-args exp)
                                    imports::and_minus_args
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()])
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
                imports::astify_minus_assert
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (assert-condition exp)
                            imports::assert_minus_condition
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_sequence
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (begin-statements exp)
                            imports::begin_minus_statements
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_comment
                    .with(|value| value.get())
                    .invoke(&[
                        Scm::pair(Scm::symbol("cond"), Scm::pair(Scm::symbol("..."), Scm::Nil)),
                        {
                            // (astify-cond (cond-clauses exp) env tail?)
                            imports::astify_minus_cond
                                .with(|value| value.get())
                                .invoke(&[
                                    {
                                        // (cond-clauses exp)
                                        imports::cond_minus_clauses
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()])
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
                imports::astify_minus_comment
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (cons (quote define) (definition-signature exp))
                            imports::cons.with(|value| value.get()).invoke(&[
                                Scm::symbol("define"),
                                {
                                    // (definition-signature exp)
                                    imports::definition_minus_signature
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()])
                                },
                            ])
                        },
                        {
                            // (astify-definition (definition-variable exp) (definition-value exp) env)
                            imports::astify_minus_definition
                                .with(|value| value.get())
                                .invoke(&[
                                    {
                                        // (definition-variable exp)
                                        imports::definition_minus_variable
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()])
                                    },
                                    {
                                        // (definition-value exp)
                                        imports::definition_minus_value
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()])
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
                imports::astify_minus_alternative
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (if-condition exp)
                            imports::if_minus_condition
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        },
                        {
                            // (if-consequence exp)
                            imports::if_minus_consequence
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        },
                        {
                            // (if-alternative exp)
                            imports::if_minus_alternative
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_abstraction
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (lambda-params exp)
                            imports::lambda_minus_params
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        },
                        {
                            // (lambda-body exp)
                            imports::lambda_minus_body
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_comment
                    .with(|value| value.get())
                    .invoke(&[exp.clone(), {
                        // (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?)
                        imports::astify_minus_application
                            .with(|value| value.get())
                            .invoke(&[
                                {
                                    // (astify-abstraction (let-vars exp) (let-body exp) env)
                                    imports::astify_minus_abstraction
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (let-vars exp)
                                                imports::let_minus_vars
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()])
                                            },
                                            {
                                                // (let-body exp)
                                                imports::let_minus_body
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()])
                                            },
                                            env.clone(),
                                        ])
                                },
                                {
                                    // (let-args exp)
                                    imports::let_minus_args
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()])
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
                                            imports::null_p
                                                .with(|value| value.get())
                                                .invoke(&[bindings.clone()])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (cons (quote begin) (let-body exp))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    Scm::symbol("begin"),
                                                    {
                                                        // (let-body exp)
                                                        imports::let_minus_body
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone()])
                                                    },
                                                ])
                                            }
                                        } else {
                                            {
                                                // (list (quote let) (list (car bindings)) (loop (cdr bindings)))
                                                imports::list.with(|value| value.get()).invoke(&[
                                                    Scm::symbol("let"),
                                                    {
                                                        // (list (car bindings))
                                                        imports::list
                                                            .with(|value| value.get())
                                                            .invoke(&[{
                                                                // (car bindings)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[bindings.clone()])
                                                            }])
                                                    },
                                                    {
                                                        // (loop (cdr bindings))
                                                        loop_.get().invoke(&[{
                                                            // (cdr bindings)
                                                            imports::cdr
                                                                .with(|value| value.get())
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
                                    imports::astify_minus_comment
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone(), {
                                            // (expand-let (loop (let*-bindings exp)) env tail?)
                                            Scm::func(globals::expand_minus_let).invoke(&[
                                                {
                                                    // (loop (let*-bindings exp))
                                                    loop_.get().invoke(&[{
                                                        // (let*-bindings exp)
                                                        imports::let_star__minus_bindings
                                                            .with(|value| value.get())
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
                imports::astify_minus_comment
                    .with(|value| value.get())
                    .invoke(&[exp.clone(), {
                        // (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?)
                        Scm::func(globals::expand_minus_let).invoke(&[
                            {
                                // (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))))
                                imports::list.with(|value| value.get()).invoke(&[
                                    Scm::symbol("let"),
                                    {
                                        // (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp))
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let name = args[0].clone();
                                                    {
                                                        // (list name (quote (quote *uninitialized*)))
                                                        imports::list
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                name.clone(),
                                                                Scm::pair(
                                                                    Scm::symbol("quote"),
                                                                    Scm::pair(
                                                                        Scm::symbol(
                                                                            "*uninitialized*",
                                                                        ),
                                                                        Scm::Nil,
                                                                    ),
                                                                ),
                                                            ])
                                                    }
                                                })
                                            },
                                            {
                                                // (let-vars exp)
                                                imports::let_minus_vars
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()])
                                            },
                                        ])
                                    },
                                    {
                                        // (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            Scm::symbol("begin"),
                                            {
                                                // (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp))
                                                imports::append.with(|value| value.get()).invoke(&[
                                                    {
                                                        // (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp))
                                                        imports::map
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                {
                                                                    // Closure
                                                                    Scm::func(
                                                                        move |args: &[Scm]| {
                                                                            if args.len() != 2 {
                                                                                panic!(
                                                                                    "invalid arity"
                                                                                )
                                                                            }
                                                                            let name =
                                                                                args[0].clone();
                                                                            let val =
                                                                                args[1].clone();
                                                                            {
                                                                                // (list (quote set!) name val)
                                                                                imports::list
                                                                                    .with(|value| {
                                                                                        value.get()
                                                                                    })
                                                                                    .invoke(&[
                                                                                        Scm::symbol(
                                                                                            "set!",
                                                                                        ),
                                                                                        name.clone(
                                                                                        ),
                                                                                        val.clone(),
                                                                                    ])
                                                                            }
                                                                        },
                                                                    )
                                                                },
                                                                {
                                                                    // (let-vars exp)
                                                                    imports::let_minus_vars
                                                                        .with(|value| value.get())
                                                                        .invoke(&[exp.clone()])
                                                                },
                                                                {
                                                                    // (let-args exp)
                                                                    imports::let_minus_args
                                                                        .with(|value| value.get())
                                                                        .invoke(&[exp.clone()])
                                                                },
                                                            ])
                                                    },
                                                    {
                                                        // (let-body exp)
                                                        imports::let_minus_body
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone()])
                                                    },
                                                ])
                                            },
                                        ])
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
                imports::astify_minus_constant
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (cadr exp)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_assignment
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (set!-variable exp)
                            imports::set_i_minus_variable
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        },
                        {
                            // (set!-value exp)
                            imports::set_i_minus_value
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::astify_minus_testsuite
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // (testsuite-name exp)
                            imports::testsuite_minus_name
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        },
                        {
                            // (testsuite-cases exp)
                            imports::testsuite_minus_cases
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
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
                imports::list.with(|value| value.get()).invoke(&[
                    Scm::symbol("GLOBAL-MARKER"),
                    {
                        // (new-keyword (quote and) expand-and)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("and"), Scm::func(globals::expand_minus_and)])
                    },
                    {
                        // (new-keyword (quote assert) expand-assert)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("assert"),
                                Scm::func(globals::expand_minus_assert),
                            ])
                    },
                    {
                        // (new-keyword (quote begin) expand-begin)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("begin"), Scm::func(globals::expand_minus_begin)])
                    },
                    {
                        // (new-keyword (quote cond) expand-cond)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("cond"), Scm::func(globals::expand_minus_cond)])
                    },
                    {
                        // (new-keyword (quote define) expand-define)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("define"),
                                Scm::func(globals::expand_minus_define),
                            ])
                    },
                    {
                        // (new-keyword (quote if) expand-if)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("if"), Scm::func(globals::expand_minus_if)])
                    },
                    {
                        // (new-keyword (quote lambda) expand-lambda)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("lambda"),
                                Scm::func(globals::expand_minus_lambda),
                            ])
                    },
                    {
                        // (new-keyword (quote let) expand-let)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("let"), Scm::func(globals::expand_minus_let)])
                    },
                    {
                        // (new-keyword (quote let*) expand-let*)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("let*"),
                                Scm::func(globals::expand_minus_let_star_),
                            ])
                    },
                    {
                        // (new-keyword (quote letrec) expand-letrec)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("letrec"),
                                Scm::func(globals::expand_minus_letrec),
                            ])
                    },
                    {
                        // (new-keyword (quote quote) expand-quote)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("quote"), Scm::func(globals::expand_minus_quote)])
                    },
                    {
                        // (new-keyword (quote set!) expand-set!)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("set!"), Scm::func(globals::expand_minus_set_i)])
                    },
                    {
                        // (new-keyword (quote testsuite) expand-testsuite)
                        imports::new_minus_keyword
                            .with(|value| value.get())
                            .invoke(&[
                                Scm::symbol("testsuite"),
                                Scm::func(globals::expand_minus_testsuite),
                            ])
                    },
                ])
            }
        }
        .into()
    }
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
