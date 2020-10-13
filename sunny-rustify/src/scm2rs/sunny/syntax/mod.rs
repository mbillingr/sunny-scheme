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
        let exp__876 = args[0].clone();
        let env__877 = args[1].clone();
        let tail_p__878 = args[2].clone();
        {
            // (astify-comment exp (astify-and (and-args exp) env tail?))
            imports::astify_minus_comment(&[exp__876.clone(), {
                // (astify-and (and-args exp) env tail?)
                imports::astify_minus_and(&[
                    {
                        // (and-args exp)
                        imports::and_minus_args(&[exp__876.clone()])
                    },
                    env__877.clone(),
                    tail_p__878.clone(),
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
        let exp__879 = args[0].clone();
        let env__880 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-assert (assert-condition exp) env)
            imports::astify_minus_assert(&[
                {
                    // (assert-condition exp)
                    imports::assert_minus_condition(&[exp__879.clone()])
                },
                env__880.clone(),
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
        let exp__881 = args[0].clone();
        let env__882 = args[1].clone();
        let tail_p__883 = args[2].clone();
        {
            // (astify-sequence (begin-statements exp) env tail?)
            imports::astify_minus_sequence(&[
                {
                    // (begin-statements exp)
                    imports::begin_minus_statements(&[exp__881.clone()])
                },
                env__882.clone(),
                tail_p__883.clone(),
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
        let exp__884 = args[0].clone();
        let env__885 = args[1].clone();
        let tail_p__886 = args[2].clone();
        {
            // (astify-comment (quote (cond ...)) (astify-cond (cond-clauses exp) env tail?))
            imports::astify_minus_comment(&[
                Scm::pair(Scm::symbol("cond"), Scm::pair(Scm::symbol("..."), Scm::Nil)),
                {
                    // (astify-cond (cond-clauses exp) env tail?)
                    imports::astify_minus_cond(&[
                        {
                            // (cond-clauses exp)
                            imports::cond_minus_clauses(&[exp__884.clone()])
                        },
                        env__885.clone(),
                        tail_p__886.clone(),
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
        let exp__887 = args[0].clone();
        let env__888 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-comment (cons (quote define) (definition-signature exp)) (astify-definition (definition-variable exp) (definition-value exp) env))
            imports::astify_minus_comment(&[
                {
                    // (cons (quote define) (definition-signature exp))
                    imports::cons(&[Scm::symbol("define"), {
                        // (definition-signature exp)
                        imports::definition_minus_signature(&[exp__887.clone()])
                    }])
                },
                {
                    // (astify-definition (definition-variable exp) (definition-value exp) env)
                    imports::astify_minus_definition(&[
                        {
                            // (definition-variable exp)
                            imports::definition_minus_variable(&[exp__887.clone()])
                        },
                        {
                            // (definition-value exp)
                            imports::definition_minus_value(&[exp__887.clone()])
                        },
                        env__888.clone(),
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
        let exp__889 = args[0].clone();
        let env__890 = args[1].clone();
        let tail_p__891 = args[2].clone();
        {
            // (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
            imports::astify_minus_alternative(&[
                {
                    // (if-condition exp)
                    imports::if_minus_condition(&[exp__889.clone()])
                },
                {
                    // (if-consequence exp)
                    imports::if_minus_consequence(&[exp__889.clone()])
                },
                {
                    // (if-alternative exp)
                    imports::if_minus_alternative(&[exp__889.clone()])
                },
                env__890.clone(),
                tail_p__891.clone(),
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
        let exp__892 = args[0].clone();
        let env__893 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-abstraction (lambda-params exp) (lambda-body exp) env)
            imports::astify_minus_abstraction(&[
                {
                    // (lambda-params exp)
                    imports::lambda_minus_params(&[exp__892.clone()])
                },
                {
                    // (lambda-body exp)
                    imports::lambda_minus_body(&[exp__892.clone()])
                },
                env__893.clone(),
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
        let exp__894 = args[0].clone();
        let env__895 = args[1].clone();
        let tail_p__896 = args[2].clone();
        {
            // (astify-comment exp (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?))
            imports::astify_minus_comment(&[exp__894.clone(), {
                // (astify-application (astify-abstraction (let-vars exp) (let-body exp) env) (let-args exp) env tail?)
                imports::astify_minus_application(&[
                    {
                        // (astify-abstraction (let-vars exp) (let-body exp) env)
                        imports::astify_minus_abstraction(&[
                            {
                                // (let-vars exp)
                                imports::let_minus_vars(&[exp__894.clone()])
                            },
                            {
                                // (let-body exp)
                                imports::let_minus_body(&[exp__894.clone()])
                            },
                            env__895.clone(),
                        ])
                    },
                    {
                        // (let-args exp)
                        imports::let_minus_args(&[exp__894.clone()])
                    },
                    env__895.clone(),
                    tail_p__896.clone(),
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
        let exp__899 = args[0].clone();
        let env__900 = args[1].clone();
        let tail_p__901 = args[2].clone();
        {
            // (letrec ((loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings))))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (bindings) (if (null? bindings) (cons (quote begin) (let-body exp)) (list (quote let) (list (car bindings)) (loop (cdr bindings)))))) (astify-comment exp (expand-let (loop (let*-bindings exp)) env tail?))))
                {
                    let loop__897 = Scm::symbol("*uninitialized*");
                    {
                        let loop__897 = loop__897.into_boxed();
                        {
                            loop__897.set({
                                // Closure
                                let exp__899 = exp__899.clone();
                                let loop__897 = loop__897.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let bindings__898 = args[0].clone();
                                    if ({
                                        // (null? bindings)
                                        imports::null_p(&[bindings__898.clone()])
                                    })
                                    .is_true()
                                    {
                                        {
                                            // (cons (quote begin) (let-body exp))
                                            imports::cons(&[Scm::symbol("begin"), {
                                                // (let-body exp)
                                                imports::let_minus_body(&[exp__899.clone()])
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
                                                        imports::car(&[bindings__898.clone()])
                                                    }])
                                                },
                                                {
                                                    // (loop (cdr bindings))
                                                    loop__897.get().invoke(&[{
                                                        // (cdr bindings)
                                                        imports::cdr(&[bindings__898.clone()])
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
                                imports::astify_minus_comment(&[exp__899.clone(), {
                                    // (expand-let (loop (let*-bindings exp)) env tail?)
                                    expand_minus_let(&[
                                        {
                                            // (loop (let*-bindings exp))
                                            loop__897.get().invoke(&[{
                                                // (let*-bindings exp)
                                                imports::let_star__minus_bindings(&[
                                                    exp__899.clone()
                                                ])
                                            }])
                                        },
                                        env__900.clone(),
                                        tail_p__901.clone(),
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
        let exp__902 = args[0].clone();
        let env__906 = args[1].clone();
        let tail_p__907 = args[2].clone();
        {
            // (astify-comment exp (expand-let (list (quote let) (map (lambda (name) (list name (quote (quote *uninitialized*)))) (let-vars exp)) (cons (quote begin) (append (map (lambda (name val) (list (quote set!) name val)) (let-vars exp) (let-args exp)) (let-body exp)))) env tail?))
            imports::astify_minus_comment(&[exp__902.clone(), {
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
                                            let name__903 = args[0].clone();
                                            {
                                                // (list name (quote (quote *uninitialized*)))
                                                imports::list(&[
                                                    name__903.clone(),
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
                                        imports::let_minus_vars(&[exp__902.clone()])
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
                                                        let name__904 = args[0].clone();
                                                        let val__905 = args[1].clone();
                                                        {
                                                            // (list (quote set!) name val)
                                                            imports::list(&[
                                                                Scm::symbol("set!"),
                                                                name__904.clone(),
                                                                val__905.clone(),
                                                            ])
                                                        }
                                                    })
                                                },
                                                {
                                                    // (let-vars exp)
                                                    imports::let_minus_vars(&[exp__902.clone()])
                                                },
                                                {
                                                    // (let-args exp)
                                                    imports::let_minus_args(&[exp__902.clone()])
                                                },
                                            ])
                                        },
                                        {
                                            // (let-body exp)
                                            imports::let_minus_body(&[exp__902.clone()])
                                        },
                                    ])
                                }])
                            },
                        ])
                    },
                    env__906.clone(),
                    tail_p__907.clone(),
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
        let exp__909 = args[0].clone();
        let env__908 = args[1].clone();
        let tail_p__912 = args[2].clone();
        {
            // (let ((x1 (make-syntactic-closure env (quote ()) (cadr exp))) (x2 (make-syntactic-closure env (quote ()) (caddr exp)))) (astify (make-syntactic-closure (make-core-env) (quote ()) (list (list (quote lambda) (list (quote tmp)) (list (quote if) (quote tmp) (quote tmp) x2)) x1)) env tail?))
            {
                let [x1__911, x2__910] = [
                    {
                        // (make-syntactic-closure env (quote ()) (cadr exp))
                        imports::make_minus_syntactic_minus_closure(&[
                            env__908.clone(),
                            Scm::Nil,
                            {
                                // (cadr exp)
                                imports::cadr(&[exp__909.clone()])
                            },
                        ])
                    },
                    {
                        // (make-syntactic-closure env (quote ()) (caddr exp))
                        imports::make_minus_syntactic_minus_closure(&[
                            env__908.clone(),
                            Scm::Nil,
                            {
                                // (caddr exp)
                                imports::caddr(&[exp__909.clone()])
                            },
                        ])
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
                                                        x2__910.clone(),
                                                    ])
                                                },
                                            ])
                                        },
                                        x1__911.clone(),
                                    ])
                                },
                            ])
                        },
                        env__908.clone(),
                        tail_p__912.clone(),
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
        let exp__913 = args[0].clone();
        let env__914 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-constant (cadr exp) env)
            imports::astify_minus_constant(&[
                {
                    // (cadr exp)
                    imports::cadr(&[exp__913.clone()])
                },
                env__914.clone(),
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
        let exp__915 = args[0].clone();
        let env__916 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-assignment (set!-variable exp) (set!-value exp) env)
            imports::astify_minus_assignment(&[
                {
                    // (set!-variable exp)
                    imports::set_i_minus_variable(&[exp__915.clone()])
                },
                {
                    // (set!-value exp)
                    imports::set_i_minus_value(&[exp__915.clone()])
                },
                env__916.clone(),
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
        let exp__917 = args[0].clone();
        let env__918 = args[1].clone();
        let tail_p = args[2].clone();
        {
            // (astify-testsuite (testsuite-name exp) (testsuite-cases exp) env)
            imports::astify_minus_testsuite(&[
                {
                    // (testsuite-name exp)
                    imports::testsuite_minus_name(&[exp__917.clone()])
                },
                {
                    // (testsuite-cases exp)
                    imports::testsuite_minus_cases(&[exp__917.clone()])
                },
                env__918.clone(),
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
