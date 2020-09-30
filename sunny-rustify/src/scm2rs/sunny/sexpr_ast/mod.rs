#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::sexpr_minus__g_ast;
    pub use super::globals::sexpr_minus__g_export;
    pub use super::globals::sexpr_minus__g_import;
    pub use super::globals::sexpr_minus__g_sequence;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import_minus_all: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import-all"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_regular_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->regular-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->reference"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static objectify_minus_symbol: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL objectify-symbol"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static wrap_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL wrap-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testsuite"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::write::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::env::initialize();
    crate::sunny::library::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (sexpr->ast exp env tail?) ...)
            globals::sexpr_minus__g_ast.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let exp = args[0].clone();
                        let env = args[1].clone();
                        let tail_p = args[2].clone();
                        {
                            // (cond ...)
                            if ({
                                // (keyword? exp)
                                imports::keyword_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            })
                            .is_true()
                            {
                                exp.clone()
                            } else if ({
                                // (ast-node? exp)
                                imports::ast_minus_node_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            })
                            .is_true()
                            {
                                exp.clone()
                            } else if ({
                                // (pair? exp)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (cond ...)
                                    if ({
                                        // (and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env)))
                                        if ({
                                            // (eq? (quote testsuite) (car exp))
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("testsuite"),
                                                {
                                                    // (car exp)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[exp.clone()])
                                                },
                                            ])
                                        })
                                        .is_true()
                                        {
                                            {
                                                // (not (lookup (quote testsuite) env))
                                                imports::not.with(|value| value.get()).invoke(&[{
                                                    // (lookup (quote testsuite) env)
                                                    imports::lookup
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            Scm::symbol("testsuite"),
                                                            env.clone(),
                                                        ])
                                                }])
                                            }
                                        } else {
                                            Scm::False
                                        }
                                    })
                                    .is_true()
                                    {
                                        {
                                            // (sexpr->testsuite (cadr exp) (cddr exp) env)
                                            globals::sexpr_minus__g_testsuite
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cadr exp)
                                                        imports::cadr
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone()])
                                                    },
                                                    {
                                                        // (cddr exp)
                                                        imports::cddr
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone()])
                                                    },
                                                    env.clone(),
                                                ])
                                        }
                                    } else {
                                        {
                                            // (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))
                                            {
                                                let f_minus_obj = {
                                                    // (sexpr->ast (car exp) env #f)
                                                    globals::sexpr_minus__g_ast
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            {
                                                                // (car exp)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[exp.clone()])
                                                            },
                                                            env.clone(),
                                                            Scm::False,
                                                        ])
                                                };
                                                if ({
                                                    // (keyword? f-obj)
                                                    imports::keyword_p
                                                        .with(|value| value.get())
                                                        .invoke(&[f_minus_obj.clone()])
                                                })
                                                .is_true()
                                                {
                                                    {
                                                        // ((keyword-handler f-obj) exp env tail?)
                                                        {
                                                            // (keyword-handler f-obj)
                                                            imports::keyword_minus_handler
                                                                .with(|value| value.get())
                                                                .invoke(&[f_minus_obj.clone()])
                                                        }
                                                        .invoke(&[
                                                            exp.clone(),
                                                            env.clone(),
                                                            tail_p.clone(),
                                                        ])
                                                    }
                                                } else {
                                                    {
                                                        // (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))
                                                        globals::wrap_minus_sexpr
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone(), {
                                                                // (sexpr->application f-obj (cdr exp) env tail?)
                                                                globals::sexpr_minus__g_application
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        f_minus_obj.clone(),
                                                                        {
                                                                            // (cdr exp)
                                                                            imports::cdr
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    exp.clone()
                                                                                ])
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
                            } else if ({
                                // (symbol? exp)
                                imports::symbol_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (objectify-symbol exp env)
                                    globals::objectify_minus_symbol
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone(), env.clone()])
                                }
                            } else {
                                {
                                    // (sexpr->constant exp env)
                                    globals::sexpr_minus__g_constant
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone(), env.clone()])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (wrap-sexpr exp node) ...)
            globals::wrap_minus_sexpr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let exp = args[0].clone();
                        let node = args[1].clone();
                        {
                            // (make-comment exp node)
                            imports::make_minus_comment
                                .with(|value| value.get())
                                .invoke(&[exp.clone(), node.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->constant exp env) ...)
            globals::sexpr_minus__g_constant.with(|value| {
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
            // (define (objectify-symbol name env) ...)
            globals::objectify_minus_symbol.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (let ((var (ensure-var! name env))) (if (keyword? var) var (make-reference name var)))
                            {
                                let var = {
                                    // (ensure-var! name env)
                                    imports::ensure_minus_var_i
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), env.clone()])
                                };
                                if ({
                                    // (keyword? var)
                                    imports::keyword_p
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()])
                                })
                                .is_true()
                                {
                                    var.clone()
                                } else {
                                    {
                                        // (make-reference name var)
                                        imports::make_minus_reference
                                            .with(|value| value.get())
                                            .invoke(&[name.clone(), var.clone()])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->reference name env) ...)
            globals::sexpr_minus__g_reference.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (let ((var (ensure-var! name env))) (make-reference name var))
                            {
                                let var = {
                                    // (ensure-var! name env)
                                    imports::ensure_minus_var_i
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), env.clone()])
                                };
                                {
                                    // (make-reference name var)
                                    imports::make_minus_reference
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), var.clone()])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->application func arg* env tail?) ...)
            globals::sexpr_minus__g_application.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 4 {
                            panic!("invalid arity")
                        }
                        let func = args[0].clone();
                        let arg_star_ = args[1].clone();
                        let env = args[2].clone();
                        let tail_p = args[3].clone();
                        if ({
                            // (eq? (quote ABSTRACTION) (func (quote kind)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("ABSTRACTION"),
                                {
                                    // (func (quote kind))
                                    func.clone().invoke(&[Scm::symbol("kind")])
                                },
                            ])
                        })
                        .is_true()
                        {
                            {
                                // (sexpr->fixlet (sexpr->ast func env #f) arg* env tail?)
                                globals::sexpr_minus__g_fixlet
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (sexpr->ast func env #f)
                                            globals::sexpr_minus__g_ast
                                                .with(|value| value.get())
                                                .invoke(&[func.clone(), env.clone(), Scm::False])
                                        },
                                        arg_star_.clone(),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            }
                        } else {
                            {
                                // (sexpr->regular-application func arg* env tail?)
                                globals::sexpr_minus__g_regular_minus_application
                                    .with(|value| value.get())
                                    .invoke(&[
                                        func.clone(),
                                        arg_star_.clone(),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->regular-application func arg* env tail?) ...)
            globals::sexpr_minus__g_regular_minus_application.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 4 {
                            panic!("invalid arity")
                        }
                        let func = args[0].clone();
                        let arg_star_ = args[1].clone();
                        let env = args[2].clone();
                        let tail_p = args[3].clone();
                        {
                            // (let ((args (sexpr->args arg* env))) (make-application func args tail?))
                            {
                                let args_ = {
                                    // (sexpr->args arg* env)
                                    globals::sexpr_minus__g_args
                                        .with(|value| value.get())
                                        .invoke(&[arg_star_.clone(), env.clone()])
                                };
                                {
                                    // (make-application func args tail?)
                                    imports::make_minus_application
                                        .with(|value| value.get())
                                        .invoke(&[func.clone(), args_.clone(), tail_p.clone()])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->fixlet func arg* env tail?) ...)
            globals::sexpr_minus__g_fixlet.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 4 {
                            panic!("invalid arity")
                        }
                        let func = args[0].clone();
                        let arg_star_ = args[1].clone();
                        let env = args[2].clone();
                        let tail_p = args[3].clone();
                        {
                            // (let* ((args (sexpr->args arg* env))) (make-fixlet (func (quote get-params)) (func (quote get-vars)) args (func (quote get-body))))
                            {
                                // (let ((args (sexpr->args arg* env))) (begin (make-fixlet (func (quote get-params)) (func (quote get-vars)) args (func (quote get-body)))))
                                {
                                    let args_ = {
                                        // (sexpr->args arg* env)
                                        globals::sexpr_minus__g_args
                                            .with(|value| value.get())
                                            .invoke(&[arg_star_.clone(), env.clone()])
                                    };
                                    {
                                        // (make-fixlet (func (quote get-params)) (func (quote get-vars)) args (func (quote get-body)))
                                        imports::make_minus_fixlet.with(|value| value.get()).invoke(
                                            &[
                                                {
                                                    // (func (quote get-params))
                                                    func.clone()
                                                        .invoke(&[Scm::symbol("get-params")])
                                                },
                                                {
                                                    // (func (quote get-vars))
                                                    func.clone().invoke(&[Scm::symbol("get-vars")])
                                                },
                                                args_.clone(),
                                                {
                                                    // (func (quote get-body))
                                                    func.clone().invoke(&[Scm::symbol("get-body")])
                                                },
                                            ],
                                        )
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->args arg* env) ...)
            globals::sexpr_minus__g_args.with(|value| {
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
                                // (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))
                                imports::make_minus_args.with(|value| value.get()).invoke(&[
                                    {
                                        // (sexpr->ast (car arg*) env #f)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[
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
                                        // (sexpr->args (cdr arg*) env)
                                        globals::sexpr_minus__g_args
                                            .with(|value| value.get())
                                            .invoke(&[
                                                {
                                                    // (cdr arg*)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[arg_star_.clone()])
                                                },
                                                env.clone(),
                                            ])
                                    },
                                ])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->abstraction param* body env) ...)
            globals::sexpr_minus__g_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let param_star_ = args[0].clone();let body = args[1].clone();let env = args[2].clone();{
// (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-vararg-abstraction (proper-list-part param* (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))) (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))))
{let [local_minus_env, body, ] = [{
// (adjoin-local-env param* env)
imports::adjoin_minus_local_minus_env.with(|value| value.get()).invoke(&[param_star_.clone(),env.clone()])},{
// (scan-out-defines body)
imports::scan_minus_out_minus_defines.with(|value| value.get()).invoke(&[body.clone()])}];if ({
// (dotted-list? param*)
imports::dotted_minus_list_p.with(|value| value.get()).invoke(&[param_star_.clone()])}).is_true() {{
// (make-vararg-abstraction (proper-list-part param* (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t)))
imports::make_minus_vararg_minus_abstraction.with(|value| value.get()).invoke(&[{
// (proper-list-part param* (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))
imports::proper_minus_list_minus_part.with(|value| value.get()).invoke(&[param_star_.clone(),{
// (last-cdr param*)
imports::last_minus_cdr.with(|value| value.get()).invoke(&[param_star_.clone()])},{
// (map (lambda (p) (lookup p local-env)) (proper-list-part param*))
imports::map.with(|value| value.get()).invoke(&[{let local_minus_env = local_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p = args[0].clone();{
// (lookup p local-env)
imports::lookup.with(|value| value.get()).invoke(&[p.clone(),local_minus_env.clone()])}})},{
// (proper-list-part param*)
imports::proper_minus_list_minus_part.with(|value| value.get()).invoke(&[param_star_.clone()])}])},{
// (lookup (last-cdr param*) local-env)
imports::lookup.with(|value| value.get()).invoke(&[{
// (last-cdr param*)
imports::last_minus_cdr.with(|value| value.get()).invoke(&[param_star_.clone()])},local_minus_env.clone()])},{
// (sexpr->sequence body local-env #t)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[body.clone(),local_minus_env.clone(),Scm::True])}])}])}} else {{
// (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))
imports::make_minus_abstraction.with(|value| value.get()).invoke(&[param_star_.clone(),{
// (map (lambda (p) (lookup p local-env)) param*)
imports::map.with(|value| value.get()).invoke(&[{let local_minus_env = local_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p = args[0].clone();{
// (lookup p local-env)
imports::lookup.with(|value| value.get()).invoke(&[p.clone(),local_minus_env.clone()])}})},param_star_.clone()])},{
// (sexpr->sequence body local-env #t)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[body.clone(),local_minus_env.clone(),Scm::True])}])}}}}})}))
        };
        {
            // (define (sexpr->sequence expr* env tail?) ...)
            globals::sexpr_minus__g_sequence.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let expr_star_ = args[0].clone();
                        let env = args[1].clone();
                        let tail_p = args[2].clone();
                        {
                            if ({
                                // (null? expr*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[expr_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (error "empty sequence")
                                    imports::error
                                        .with(|value| value.get())
                                        .invoke(&[Scm::from("empty sequence")])
                                }
                            } else {
                                Scm::symbol("*UNSPECIFIED*")
                            };
                            if ({
                                // (null? (cdr expr*))
                                imports::null_p.with(|value| value.get()).invoke(&[{
                                    // (cdr expr*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[expr_star_.clone()])
                                }])
                            })
                            .is_true()
                            {
                                {
                                    // (sexpr->ast (car expr*) env tail?)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (car expr*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[expr_star_.clone()])
                                            },
                                            env.clone(),
                                            tail_p.clone(),
                                        ])
                                }
                            } else {
                                {
                                    // (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))
                                    {
                                        let first = {
                                            // (sexpr->ast (car expr*) env #f)
                                            globals::sexpr_minus__g_ast
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (car expr*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[expr_star_.clone()])
                                                    },
                                                    env.clone(),
                                                    Scm::False,
                                                ])
                                        };
                                        {
                                            // (make-sequence first (sexpr->sequence (cdr expr*) env tail?))
                                            imports::make_minus_sequence
                                                .with(|value| value.get())
                                                .invoke(&[first.clone(), {
                                                    // (sexpr->sequence (cdr expr*) env tail?)
                                                    globals::sexpr_minus__g_sequence
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            {
                                                                // (cdr expr*)
                                                                imports::cdr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[expr_star_.clone()])
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
                    })
                })
            })
        };
        {
            // (define (sexpr->import stmt* env) ...)
            globals::sexpr_minus__g_import.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let stmt_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? stmt*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[stmt_star_.clone()])
                            })
                            .is_true()
                            {
                                Scm::Nil
                            } else if ({
                                // (equal? (quote (sunny testing)) (car stmt*))
                                imports::equal_p.with(|value| value.get()).invoke(&[
                                    Scm::pair(
                                        Scm::symbol("sunny"),
                                        Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                    ),
                                    {
                                        // (car stmt*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()])
                                    },
                                ])
                            })
                            .is_true()
                            {
                                {
                                    // (sexpr->import (cdr stmt*) env)
                                    globals::sexpr_minus__g_import
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (cdr stmt*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[stmt_star_.clone()])
                                            },
                                            env.clone(),
                                        ])
                                }
                            } else if ({
                                // (eq? (quote only) (caar stmt*))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("only"),
                                    {
                                        // (caar stmt*)
                                        imports::caar
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()])
                                    },
                                ])
                            })
                            .is_true()
                            {
                                {
                                    // (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (sexpr->import-only (cadar stmt*) (cddar stmt*) env)
                                            globals::sexpr_minus__g_import_minus_only
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cadar stmt*)
                                                        imports::cadar
                                                            .with(|value| value.get())
                                                            .invoke(&[stmt_star_.clone()])
                                                    },
                                                    {
                                                        // (cddar stmt*)
                                                        imports::cddar
                                                            .with(|value| value.get())
                                                            .invoke(&[stmt_star_.clone()])
                                                    },
                                                    env.clone(),
                                                ])
                                        },
                                        {
                                            // (sexpr->import (cdr stmt*) env)
                                            globals::sexpr_minus__g_import
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cdr stmt*)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[stmt_star_.clone()])
                                                    },
                                                    env.clone(),
                                                ])
                                        },
                                    ])
                                }
                            } else {
                                {
                                    // (cons (sexpr->import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (sexpr->import-all (car stmt*) env)
                                            globals::sexpr_minus__g_import_minus_all
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (car stmt*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[stmt_star_.clone()])
                                                    },
                                                    env.clone(),
                                                ])
                                        },
                                        {
                                            // (sexpr->import (cdr stmt*) env)
                                            globals::sexpr_minus__g_import
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cdr stmt*)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[stmt_star_.clone()])
                                                    },
                                                    env.clone(),
                                                ])
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
            // (define (sexpr->export export-spec* env) ...)
            globals::sexpr_minus__g_export.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let export_minus_spec_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? export-spec*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[export_minus_spec_star_.clone()])
                            })
                            .is_true()
                            {
                                Scm::Nil
                            } else {
                                {
                                    // (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (make-export env (car export-spec*) (car export-spec*))
                                            imports::make_minus_export
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    env.clone(),
                                                    {
                                                        // (car export-spec*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                    {
                                                        // (car export-spec*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                ])
                                        },
                                        {
                                            // (sexpr->export (cdr export-spec*) env)
                                            globals::sexpr_minus__g_export
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cdr export-spec*)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                    env.clone(),
                                                ])
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
            // (define (sexpr->testsuite name cases env) ...)
            globals::sexpr_minus__g_testsuite.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let cases = args[1].clone();
                        let env = args[2].clone();
                        {
                            // (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases))
                            imports::make_minus_testsuite
                                .with(|value| value.get())
                                .invoke(&[name.clone(), {
                                    // (map (lambda (case) (sexpr->testcase case env)) cases)
                                    imports::map.with(|value| value.get()).invoke(&[
                                        {
                                            let env = env.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let case = args[0].clone();
                                                {
                                                    // (sexpr->testcase case env)
                                                    globals::sexpr_minus__g_testcase
                                                        .with(|value| value.get())
                                                        .invoke(&[case.clone(), env.clone()])
                                                }
                                            })
                                        },
                                        cases.clone(),
                                    ])
                                }])
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->testcase case env) ...)
            globals::sexpr_minus__g_testcase.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let case = args[0].clone();let env = args[1].clone();{
// (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f))))
{
// (let ((given (quote *uninitialized*)) (when (quote *uninitialized*)) (then (quote *uninitialized*)) (dispatch (quote *uninitialized*))) (begin (set! given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (set! when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (set! then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (set! dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f)))))
{let [given, when, then, dispatch, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let dispatch = dispatch.into_boxed();{let then = then.into_boxed();{let when = when.into_boxed();{let given = given.into_boxed();{given.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("let*"),{
// (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
imports::map.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let assignment = args[0].clone();{
// (list (car assignment) (caddr assignment))
imports::list.with(|value| value.get()).invoke(&[{
// (car assignment)
imports::car.with(|value| value.get()).invoke(&[assignment.clone()])},{
// (caddr assignment)
imports::caddr.with(|value| value.get()).invoke(&[assignment.clone()])}])}})},{
// (cdr stmt)
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])},body.clone()])}})});when.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
{
// (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))) (loop (cdr stmt))))
{let loop_ = Scm::symbol("*uninitialized*");{let loop_ = loop_.into_boxed();{loop_.set({let body = body.clone();let loop_ = loop_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let stmt_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? stmt*)
imports::null_p.with(|value| value.get()).invoke(&[stmt_star_.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote <-) (cadar stmt*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("<-"),{
// (cadar stmt*)
imports::cadar.with(|value| value.get()).invoke(&[stmt_star_.clone()])}])}).is_true() {{
// (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("let"),{
// (list (list (caar stmt*) (caddar stmt*)))
imports::list.with(|value| value.get()).invoke(&[{
// (list (caar stmt*) (caddar stmt*))
imports::list.with(|value| value.get()).invoke(&[{
// (caar stmt*)
imports::caar.with(|value| value.get()).invoke(&[stmt_star_.clone()])},{
// (caddar stmt*)
imports::caddar.with(|value| value.get()).invoke(&[stmt_star_.clone()])}])}])},{
// (loop (cdr stmt*))
loop_.get().invoke(&[{
// (cdr stmt*)
imports::cdr.with(|value| value.get()).invoke(&[stmt_star_.clone()])}])}])}} else {{
// (list (quote begin) (car stmt*) (loop (cdr stmt*)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("begin"),{
// (car stmt*)
imports::car.with(|value| value.get()).invoke(&[stmt_star_.clone()])},{
// (loop (cdr stmt*))
loop_.get().invoke(&[{
// (cdr stmt*)
imports::cdr.with(|value| value.get()).invoke(&[stmt_star_.clone()])}])}])}}}})});{
// (loop (cdr stmt))
loop_.get().invoke(&[{
// (cdr stmt)
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])}}}}}}})});then.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("begin"),{
// (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
imports::append.with(|value| value.get()).invoke(&[{
// (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
imports::map.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let pred = args[0].clone();{
// (list (quote assert) pred)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("assert"),pred.clone()])}})},{
// (cdr stmt)
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])},body.clone()])}])}})});dispatch.set({let given = given.clone();let dispatch = dispatch.clone();let when = when.clone();let then = then.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let section_star_ = args[0].clone();let body = args[1].clone();{
// (cond ...)
if ({
// (null? section*)
imports::null_p.with(|value| value.get()).invoke(&[section_star_.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote given) (caar section*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("given"),{
// (caar section*)
imports::caar.with(|value| value.get()).invoke(&[section_star_.clone()])}])}).is_true() {{
// (given (car section*) (dispatch (cdr section*) body))
given.get().invoke(&[{
// (car section*)
imports::car.with(|value| value.get()).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])}])}} else if ({
// (eq? (quote when) (caar section*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("when"),{
// (caar section*)
imports::caar.with(|value| value.get()).invoke(&[section_star_.clone()])}])}).is_true() {{
// (when (car section*) (dispatch (cdr section*) body))
when.get().invoke(&[{
// (car section*)
imports::car.with(|value| value.get()).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])}])}} else if ({
// (eq? (quote then) (caar section*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("then"),{
// (caar section*)
imports::caar.with(|value| value.get()).invoke(&[section_star_.clone()])}])}).is_true() {{
// (then (car section*) (dispatch (cdr section*) body))
then.get().invoke(&[{
// (car section*)
imports::car.with(|value| value.get()).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])}])}} else {{
// (error "invalid testcase")
imports::error.with(|value| value.get()).invoke(&[Scm::from("invalid testcase")])}}}})});{
// (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f)))
{let body = {
// (dispatch (cddr case) (quote ()))
dispatch.get().invoke(&[{
// (cddr case)
imports::cddr.with(|value| value.get()).invoke(&[case.clone()])},Scm::Nil])};{
// (make-testcase (cadr case) (sexpr->ast body env #f))
imports::make_minus_testcase.with(|value| value.get()).invoke(&[{
// (cadr case)
imports::cadr.with(|value| value.get()).invoke(&[case.clone()])},{
// (sexpr->ast body env #f)
globals::sexpr_minus__g_ast.with(|value| value.get()).invoke(&[body.clone(),env.clone(),Scm::False])}])}}}}}}}}}}}})}))
        };
        {
            // (define (sexpr->import-all lib env) ...)
            globals::sexpr_minus__g_import_minus_all.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let lib = args[0].clone();
                        let env = args[1].clone();
                        {
                            {
                                // (adjoin-import*! (library-exports (library-decls (get-lib lib))) env)
                                imports::adjoin_minus_import_star__i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (library-exports (library-decls (get-lib lib)))
                                            imports::library_minus_exports
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (library-decls (get-lib lib))
                                                    imports::library_minus_decls
                                                        .with(|value| value.get())
                                                        .invoke(&[{
                                                            // (get-lib lib)
                                                            imports::get_minus_lib
                                                                .with(|value| value.get())
                                                                .invoke(&[lib.clone()])
                                                        }])
                                                }])
                                        },
                                        env.clone(),
                                    ])
                            };
                            {
                                // (make-import lib)
                                imports::make_minus_import
                                    .with(|value| value.get())
                                    .invoke(&[lib.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (sexpr->import-only lib names env) ...)
            globals::sexpr_minus__g_import_minus_only.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 3 {
                            panic!("invalid arity")
                        }
                        let lib = args[0].clone();
                        let names = args[1].clone();
                        let env = args[2].clone();
                        {
                            {
                                // (check-imports names (library-exports (library-decls (get-lib lib))) lib)
                                imports::check_minus_imports
                                    .with(|value| value.get())
                                    .invoke(&[
                                        names.clone(),
                                        {
                                            // (library-exports (library-decls (get-lib lib)))
                                            imports::library_minus_exports
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (library-decls (get-lib lib))
                                                    imports::library_minus_decls
                                                        .with(|value| value.get())
                                                        .invoke(&[{
                                                            // (get-lib lib)
                                                            imports::get_minus_lib
                                                                .with(|value| value.get())
                                                                .invoke(&[lib.clone()])
                                                        }])
                                                }])
                                        },
                                        lib.clone(),
                                    ])
                            };
                            {
                                // (adjoin-import*! names env)
                                imports::adjoin_minus_import_star__i
                                    .with(|value| value.get())
                                    .invoke(&[names.clone(), env.clone()])
                            };
                            {
                                // (make-import-only lib names)
                                imports::make_minus_import_minus_only
                                    .with(|value| value.get())
                                    .invoke(&[lib.clone(), names.clone()])
                            }
                        }
                    })
                })
            })
        }
    };
}
