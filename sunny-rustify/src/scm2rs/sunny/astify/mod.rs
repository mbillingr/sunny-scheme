#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::sexpr_ast::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::astify;
    pub use super::globals::astify_minus_abstraction;
    pub use super::globals::astify_minus_alternative;
    pub use super::globals::astify_minus_and;
    pub use super::globals::astify_minus_application;
    pub use super::globals::astify_minus_assert;
    pub use super::globals::astify_minus_assignment;
    pub use super::globals::astify_minus_comment;
    pub use super::globals::astify_minus_cond;
    pub use super::globals::astify_minus_constant;
    pub use super::globals::astify_minus_definition;
    pub use super::globals::astify_minus_sequence;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_definition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-definition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_cond: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-cond"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_unspecified: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-unspecified"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_comment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-comment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-assignment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_and: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-and"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::env::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::sexpr_ast::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define astify sexpr->ast)
            globals::astify
                .with(|value| value.set(imports::sexpr_minus__g_ast.with(|value| value.get())))
        };
        {
            // (define (astify-abstraction param* body env) ...)
            globals::astify_minus_abstraction.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let param_star_ = args[0].clone();
                        let body = args[1].clone();
                        let env = args[2].clone();
                        {
                            // (let* ((local-env (adjoin-local-env param* env)) (body-sexpr (scan-out-defines body)) (body-ast (astify-sequence body-sexpr local-env #t))) (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast)))
                            {
                                // (let ((local-env (adjoin-local-env param* env))) (let ((body-sexpr (scan-out-defines body))) (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast))))))
                                {
                                    let local_minus_env = {
                                        // (adjoin-local-env param* env)
                                        imports::adjoin_minus_local_minus_env
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone(), env.clone()])
                                    };
                                    // (let ((body-sexpr (scan-out-defines body))) (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast)))))
                                    let body_minus_sexpr = {
                                        // (scan-out-defines body)
                                        imports::scan_minus_out_minus_defines
                                            .with(|value| value.get())
                                            .invoke(&[body.clone()])
                                    };
                                    // (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast))))
                                    let body_minus_ast = {
                                        // (astify-sequence body-sexpr local-env #t)
                                        globals::astify_minus_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                body_minus_sexpr.clone(),
                                                local_minus_env.clone(),
                                                Scm::True,
                                            ])
                                    };
                                    if ({
                                        // (dotted-list? param*)
                                        imports::dotted_minus_list_p
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()])
                                    })
                                    .is_true()
                                    {
                                        {
                                            // (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast))
                                            {
                                                let [fix_minus_param, var_minus_param] = [
                                                    {
                                                        // (proper-list-part param*)
                                                        imports::proper_minus_list_minus_part
                                                            .with(|value| value.get())
                                                            .invoke(&[param_star_.clone()])
                                                    },
                                                    {
                                                        // (last-cdr param*)
                                                        imports::last_minus_cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[param_star_.clone()])
                                                    },
                                                ];
                                                {
                                                    // (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)
                                                    imports::make_minus_vararg_minus_abstraction
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            fix_minus_param.clone(),
                                                            var_minus_param.clone(),
                                                            {
                                                                // (lookup* fix-param local-env)
                                                                imports::lookup_star_
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        fix_minus_param.clone(),
                                                                        local_minus_env.clone(),
                                                                    ])
                                                            },
                                                            {
                                                                // (lookup var-param local-env)
                                                                imports::lookup
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        var_minus_param.clone(),
                                                                        local_minus_env.clone(),
                                                                    ])
                                                            },
                                                            body_minus_ast.clone(),
                                                        ])
                                                }
                                            }
                                        }
                                    } else {
                                        {
                                            // (make-abstraction param* (lookup* param* local-env) body-ast)
                                            imports::make_minus_abstraction
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    param_star_.clone(),
                                                    {
                                                        // (lookup* param* local-env)
                                                        imports::lookup_star_
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                param_star_.clone(),
                                                                local_minus_env.clone(),
                                                            ])
                                                    },
                                                    body_minus_ast.clone(),
                                                ])
                                        }
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-alternative condition consequent alternative env tail?) ...)
            globals::astify_minus_alternative.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 5 {
                            panic!("invalid arity")
                        }
                        let condition = args[0].clone();
                        let consequent = args[1].clone();
                        let alternative = args[2].clone();
                        let env = args[3].clone();
                        let tail_p = args[4].clone();
                        {
                            // (make-alternative (astify condition env #f) (astify consequent env tail?) (astify alternative env tail?))
                            imports::make_minus_alternative
                                .with(|value| value.get())
                                .invoke(&[
                                    {
                                        // (astify condition env #f)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            condition.clone(),
                                            env.clone(),
                                            Scm::False,
                                        ])
                                    },
                                    {
                                        // (astify consequent env tail?)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            consequent.clone(),
                                            env.clone(),
                                            tail_p.clone(),
                                        ])
                                    },
                                    {
                                        // (astify alternative env tail?)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            alternative.clone(),
                                            env.clone(),
                                            tail_p.clone(),
                                        ])
                                    },
                                ])
                        }
                    })
                })
            })
        };
        {
            // (define (astify-and arg* env tail?) ...)
            globals::astify_minus_and.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let arg_star_ = args[0].clone();
                        let env = args[1].clone();
                        let tail_p = args[2].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? arg*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[arg_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (make-constant #t)
                                    imports::make_minus_constant
                                        .with(|value| value.get())
                                        .invoke(&[Scm::True])
                                }
                            } else if ({
                                // (null? (cdr arg*))
                                imports::null_p.with(|value| value.get()).invoke(&[{
                                    // (cdr arg*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[arg_star_.clone()])
                                }])
                            })
                            .is_true()
                            {
                                {
                                    // (astify (car arg*) env tail?)
                                    globals::astify.with(|value| value.get()).invoke(&[
                                        {
                                            // (car arg*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[arg_star_.clone()])
                                        },
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                                }
                            } else {
                                {
                                    // (make-alternative (astify (car arg*) env #f) (astify-and (cdr arg*) env tail?) (astify-constant #f env))
                                    imports::make_minus_alternative
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (astify (car arg*) env #f)
                                                globals::astify.with(|value| value.get()).invoke(&[
                                                    {
                                                        // (car arg*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[arg_star_.clone()])
                                                    },
                                                    env.clone(),
                                                    Scm::False,
                                                ])
                                            },
                                            {
                                                // (astify-and (cdr arg*) env tail?)
                                                globals::astify_minus_and
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        {
                                                            // (cdr arg*)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[arg_star_.clone()])
                                                        },
                                                        env.clone(),
                                                        tail_p.clone(),
                                                    ])
                                            },
                                            {
                                                // (astify-constant #f env)
                                                globals::astify_minus_constant
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::False, env.clone()])
                                            },
                                        ])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-application proc arg* env tail?) ...)
            globals::astify_minus_application.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 4 {
                            panic!("invalid arity")
                        }
                        let proc = args[0].clone();
                        let arg_star_ = args[1].clone();
                        let env = args[2].clone();
                        let tail_p = args[3].clone();
                        if ({
                            // (eq? (quote ABSTRACTION) (proc (quote kind)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("ABSTRACTION"),
                                {
                                    // (proc (quote kind))
                                    proc.clone().invoke(&[Scm::symbol("kind")])
                                },
                            ])
                        })
                        .is_true()
                        {
                            {
                                // (make-fixlet (proc (quote get-params)) (proc (quote get-vars)) (astify-args arg* env) (proc (quote get-body)))
                                imports::make_minus_fixlet
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (proc (quote get-params))
                                            proc.clone().invoke(&[Scm::symbol("get-params")])
                                        },
                                        {
                                            // (proc (quote get-vars))
                                            proc.clone().invoke(&[Scm::symbol("get-vars")])
                                        },
                                        {
                                            // (astify-args arg* env)
                                            globals::astify_minus_args
                                                .with(|value| value.get())
                                                .invoke(&[arg_star_.clone(), env.clone()])
                                        },
                                        {
                                            // (proc (quote get-body))
                                            proc.clone().invoke(&[Scm::symbol("get-body")])
                                        },
                                    ])
                            }
                        } else {
                            {
                                // (make-application proc (astify-args arg* env) tail?)
                                imports::make_minus_application
                                    .with(|value| value.get())
                                    .invoke(&[
                                        proc.clone(),
                                        {
                                            // (astify-args arg* env)
                                            globals::astify_minus_args
                                                .with(|value| value.get())
                                                .invoke(&[arg_star_.clone(), env.clone()])
                                        },
                                        tail_p.clone(),
                                    ])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-args arg* env) ...)
            globals::astify_minus_args.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let arg_star_ = args[0].clone();
                        let env = args[1].clone();
                        if ({
                            // (null? arg*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[arg_star_.clone()])
                        })
                        .is_true()
                        {
                            {
                                // (make-null-arg)
                                imports::make_minus_null_minus_arg
                                    .with(|value| value.get())
                                    .invoke(&[])
                            }
                        } else {
                            {
                                // (make-args (astify (car arg*) env #f) (astify-args (cdr arg*) env))
                                imports::make_minus_args.with(|value| value.get()).invoke(&[
                                    {
                                        // (astify (car arg*) env #f)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            {
                                                // (car arg*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[arg_star_.clone()])
                                            },
                                            env.clone(),
                                            Scm::False,
                                        ])
                                    },
                                    {
                                        // (astify-args (cdr arg*) env)
                                        globals::astify_minus_args.with(|value| value.get()).invoke(
                                            &[
                                                {
                                                    // (cdr arg*)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[arg_star_.clone()])
                                                },
                                                env.clone(),
                                            ],
                                        )
                                    },
                                ])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-assert cond env) ...)
            globals::astify_minus_assert.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let cond = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (make-assert (astify cond env #f))
                            imports::make_minus_assert
                                .with(|value| value.get())
                                .invoke(&[{
                                    // (astify cond env #f)
                                    globals::astify.with(|value| value.get()).invoke(&[
                                        cond.clone(),
                                        env.clone(),
                                        Scm::False,
                                    ])
                                }])
                        }
                    })
                })
            })
        };
        {
            // (define (astify-assignment var-name value env) ...)
            globals::astify_minus_assignment.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let var_minus_name = args[0].clone();
                        let value = args[1].clone();
                        let env = args[2].clone();
                        {
                            // (let ((var (ensure-var! var-name env)) (val (astify value env #f))) (variable-set-mutable! var) (make-assignment var-name var val))
                            {
                                let [var, val] = [
                                    {
                                        // (ensure-var! var-name env)
                                        imports::ensure_minus_var_i
                                            .with(|value| value.get())
                                            .invoke(&[var_minus_name.clone(), env.clone()])
                                    },
                                    {
                                        // (astify value env #f)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            value.clone(),
                                            env.clone(),
                                            Scm::False,
                                        ])
                                    },
                                ];
                                {
                                    {
                                        // (variable-set-mutable! var)
                                        imports::variable_minus_set_minus_mutable_i
                                            .with(|value| value.get())
                                            .invoke(&[var.clone()])
                                    };
                                    {
                                        // (make-assignment var-name var val)
                                        imports::make_minus_assignment
                                            .with(|value| value.get())
                                            .invoke(&[
                                                var_minus_name.clone(),
                                                var.clone(),
                                                val.clone(),
                                            ])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define astify-comment make-comment)
            globals::astify_minus_comment
                .with(|value| value.set(imports::make_minus_comment.with(|value| value.get())))
        };
        {
            // (define (astify-cond clause* env tail?) ...)
            globals::astify_minus_cond.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let clause_star_ = args[0].clone();let env = args[1].clone();let tail_p = args[2].clone();{
// (cond ...)
if ({
// (null? clause*)
imports::null_p.with(|value| value.get()).invoke(&[clause_star_.clone()])}).is_true() {{
// (astify-unspecified)
globals::astify_minus_unspecified.with(|value| value.get()).invoke(&[])}} else if ({
// (cond-else-clause? (car clause*))
imports::cond_minus_else_minus_clause_p.with(|value| value.get()).invoke(&[{
// (car clause*)
imports::car.with(|value| value.get()).invoke(&[clause_star_.clone()])}])}).is_true() {{
// (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
globals::astify_minus_sequence.with(|value| value.get()).invoke(&[{
// (cond-clause-sequence (car clause*))
imports::cond_minus_clause_minus_sequence.with(|value| value.get()).invoke(&[{
// (car clause*)
imports::car.with(|value| value.get()).invoke(&[clause_star_.clone()])}])},env.clone(),tail_p.clone()])}} else {{
// (let* ((i (astify (cond-clause-condition (car clause*)) env #f)) (t (astify-sequence (cond-clause-sequence (car clause*)) env tail?)) (e (astify-cond (cdr clause*) env tail?))) (make-alternative i t e))
{
// (let ((i (astify (cond-clause-condition (car clause*)) env #f))) (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))))
{let i = {
// (astify (cond-clause-condition (car clause*)) env #f)
globals::astify.with(|value| value.get()).invoke(&[{
// (cond-clause-condition (car clause*))
imports::cond_minus_clause_minus_condition.with(|value| value.get()).invoke(&[{
// (car clause*)
imports::car.with(|value| value.get()).invoke(&[clause_star_.clone()])}])},env.clone(),Scm::False])};
// (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e))))
let t = {
// (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
globals::astify_minus_sequence.with(|value| value.get()).invoke(&[{
// (cond-clause-sequence (car clause*))
imports::cond_minus_clause_minus_sequence.with(|value| value.get()).invoke(&[{
// (car clause*)
imports::car.with(|value| value.get()).invoke(&[clause_star_.clone()])}])},env.clone(),tail_p.clone()])};
// (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))
let e = {
// (astify-cond (cdr clause*) env tail?)
globals::astify_minus_cond.with(|value| value.get()).invoke(&[{
// (cdr clause*)
imports::cdr.with(|value| value.get()).invoke(&[clause_star_.clone()])},env.clone(),tail_p.clone()])};{
// (make-alternative i t e)
imports::make_minus_alternative.with(|value| value.get()).invoke(&[i.clone(),t.clone(),e.clone()])}}}}}}})}))
        };
        {
            // (define (astify-constant exp env) ...)
            globals::astify_minus_constant.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let exp = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (make-constant exp)
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (astify-definition var-name value env) ...)
            globals::astify_minus_definition.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let var_minus_name = args[0].clone();
                        let value = args[1].clone();
                        let env = args[2].clone();
                        {
                            // (let ((var (ensure-var! var-name env)) (val (astify value env #f))) (global-add-definition! var val) (make-definition var-name var val))
                            {
                                let [var, val] = [
                                    {
                                        // (ensure-var! var-name env)
                                        imports::ensure_minus_var_i
                                            .with(|value| value.get())
                                            .invoke(&[var_minus_name.clone(), env.clone()])
                                    },
                                    {
                                        // (astify value env #f)
                                        globals::astify.with(|value| value.get()).invoke(&[
                                            value.clone(),
                                            env.clone(),
                                            Scm::False,
                                        ])
                                    },
                                ];
                                {
                                    {
                                        // (global-add-definition! var val)
                                        imports::global_minus_add_minus_definition_i
                                            .with(|value| value.get())
                                            .invoke(&[var.clone(), val.clone()])
                                    };
                                    {
                                        // (make-definition var-name var val)
                                        imports::make_minus_definition
                                            .with(|value| value.get())
                                            .invoke(&[
                                                var_minus_name.clone(),
                                                var.clone(),
                                                val.clone(),
                                            ])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-sequence exp* env tail?) ...)
            globals::astify_minus_sequence.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let exp_star_ = args[0].clone();
                        let env = args[1].clone();
                        let tail_p = args[2].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? exp*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (error "empty sequence")
                                    imports::error
                                        .with(|value| value.get())
                                        .invoke(&[Scm::from("empty sequence")])
                                }
                            } else if ({
                                // (null? (cdr exp*))
                                imports::null_p.with(|value| value.get()).invoke(&[{
                                    // (cdr exp*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[exp_star_.clone()])
                                }])
                            })
                            .is_true()
                            {
                                {
                                    // (astify (car exp*) env tail?)
                                    globals::astify.with(|value| value.get()).invoke(&[
                                        {
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()])
                                        },
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                                }
                            } else {
                                {
                                    // (let* ((first (astify (car exp*) env #f)) (rest (astify-sequence (cdr exp*) env tail?))) (make-sequence first rest))
                                    {
                                        // (let ((first (astify (car exp*) env #f))) (let ((rest (astify-sequence (cdr exp*) env tail?))) (begin (make-sequence first rest))))
                                        {
                                            let first = {
                                                // (astify (car exp*) env #f)
                                                globals::astify.with(|value| value.get()).invoke(&[
                                                    {
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()])
                                                    },
                                                    env.clone(),
                                                    Scm::False,
                                                ])
                                            };
                                            // (let ((rest (astify-sequence (cdr exp*) env tail?))) (begin (make-sequence first rest)))
                                            let rest = {
                                                // (astify-sequence (cdr exp*) env tail?)
                                                globals::astify_minus_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        {
                                                            // (cdr exp*)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[exp_star_.clone()])
                                                        },
                                                        env.clone(),
                                                        tail_p.clone(),
                                                    ])
                                            };
                                            {
                                                // (make-sequence first rest)
                                                imports::make_minus_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[first.clone(), rest.clone()])
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-unspecified) ...)
            globals::astify_minus_unspecified.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 0 {
                            panic!("invalid arity")
                        }
                        {
                            // (make-constant (quote *UNSPECIFIED*))
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("*UNSPECIFIED*")])
                        }
                    })
                })
            })
        }
    };
}
