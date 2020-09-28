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
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import_minus_all: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import-all"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_regular_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->regular-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->reference"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static objectify_minus_symbol: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL objectify-symbol"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_and: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->and"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_cond: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->cond"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_rec: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-rec"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_seq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-seq"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_let: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-let"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_definition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->definition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static wrap_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL wrap-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->assignment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->ast"))}
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
        // (define (sexpr->ast exp env tail?) (cond ((keyword? exp) exp) ((ast-node? exp) exp) ((pair? exp) (cond ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((definition? exp) (wrap-sexpr exp (sexpr->definition exp env))) ((abstraction? exp) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))))) ((symbol? exp) (objectify-symbol exp env)) (else (sexpr->constant exp env))))
        globals::sexpr_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (cond ((keyword? exp) exp) ((ast-node? exp) exp) ((pair? exp) (cond ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((definition? exp) (wrap-sexpr exp (sexpr->definition exp env))) ((abstraction? exp) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))))) ((symbol? exp) (objectify-symbol exp env)) (else (sexpr->constant exp env))))
                    {
                        // (cond ((keyword? exp) exp) ((ast-node? exp) exp) ((pair? exp) (cond ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((definition? exp) (wrap-sexpr exp (sexpr->definition exp env))) ((abstraction? exp) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))))) ((symbol? exp) (objectify-symbol exp env)) (else (sexpr->constant exp env)))
                        if (
                            // (keyword? exp)
                            imports::keyword_p
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        )
                        .is_true()
                        {
                            exp.clone()
                        } else if (
                            // (ast-node? exp)
                            imports::ast_minus_node_p
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        )
                        .is_true()
                        {
                            exp.clone()
                        } else if (
                            // (pair? exp)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        )
                        .is_true()
                        {
                            // (cond ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((definition? exp) (wrap-sexpr exp (sexpr->definition exp env))) ((abstraction? exp) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))))
                            if (
                                // (eq? (quote set!) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("set!"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->assignment (cadr exp) (caddr exp) env)
                                globals::sexpr_minus__g_assignment
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (caddr exp)
                                        imports::caddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (definition? exp)
                                imports::definition_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->definition exp env))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->definition exp env)
                                        globals::sexpr_minus__g_definition
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone(), env.clone()]),
                                    ])
                            } else if (
                                // (abstraction? exp)
                                imports::abstraction_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            )
                            .is_true()
                            {
                                // (sexpr->abstraction (cadr exp) (cddr exp) env)
                                globals::sexpr_minus__g_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (cddr exp)
                                        imports::cddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (eq? (quote begin) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("begin"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->sequence (cdr exp) env tail?)
                                globals::sexpr_minus__g_sequence
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr exp)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            } else if (
                                // (eq? (quote let) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("let"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-let (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_let
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote let*) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("let*"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-seq (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_seq
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote letrec) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("letrec"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-rec (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_rec
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote cond) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("cond"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->cond (cond-clauses exp) env tail?)
                                        globals::sexpr_minus__g_cond
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cond-clauses exp)
                                                imports::cond_minus_clauses
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote and) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("and"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->and (cdr exp) env tail?)
                                        globals::sexpr_minus__g_and
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cdr exp)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env)))
                                if (
                                    // (eq? (quote testsuite) (car exp))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("testsuite"),
                                        // (car exp)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                    ])
                                )
                                .is_true()
                                {
                                    // (not (lookup (quote testsuite) env))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (lookup (quote testsuite) env)
                                        imports::lookup
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("testsuite"), env.clone()]),
                                    ])
                                } else {
                                    Scm::False
                                }
                            )
                            .is_true()
                            {
                                // (sexpr->testsuite (cadr exp) (cddr exp) env)
                                globals::sexpr_minus__g_testsuite
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (cddr exp)
                                        imports::cddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env)))
                                if (
                                    // (eq? (quote assert) (car exp))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("assert"),
                                        // (car exp)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                    ])
                                )
                                .is_true()
                                {
                                    // (not (lookup (quote assert) env))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (lookup (quote assert) env)
                                        imports::lookup
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("assert"), env.clone()]),
                                    ])
                                } else {
                                    Scm::False
                                }
                            )
                            .is_true()
                            {
                                // (sexpr->assert (cadr exp) env)
                                globals::sexpr_minus__g_assert
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else {
                                // (let ((f-obj (sexpr->ast (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))
                                {
                                    let [f_minus_obj] = [
                                        // (sexpr->ast (car exp) env #f)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car exp)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                Scm::False,
                                            ]),
                                    ];
                                    // (letrec () (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))))
                                    {
                                        if (
                                            // (keyword? f-obj)
                                            imports::keyword_p
                                                .with(|value| value.get())
                                                .invoke(&[f_minus_obj.clone()])
                                        )
                                        .is_true()
                                        {
                                            // ((keyword-handler f-obj) exp env tail?)

                                            // (keyword-handler f-obj)
                                            imports::keyword_minus_handler
                                                .with(|value| value.get())
                                                .invoke(&[f_minus_obj.clone()])
                                                .invoke(&[exp.clone(), env.clone(), tail_p.clone()])
                                        } else {
                                            // (wrap-sexpr exp (sexpr->application f-obj (cdr exp) env tail?))
                                            globals::wrap_minus_sexpr
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    exp.clone(),
                                                    // (sexpr->application f-obj (cdr exp) env tail?)
                                                    globals::sexpr_minus__g_application
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            f_minus_obj.clone(),
                                                            // (cdr exp)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[exp.clone()]),
                                                            env.clone(),
                                                            tail_p.clone(),
                                                        ]),
                                                ])
                                        }
                                    }
                                }
                            }
                        } else if (
                            // (symbol? exp)
                            imports::symbol_p
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        )
                        .is_true()
                        {
                            // (objectify-symbol exp env)
                            globals::objectify_minus_symbol
                                .with(|value| value.get())
                                .invoke(&[exp.clone(), env.clone()])
                        } else {
                            // (sexpr->constant exp env)
                            globals::sexpr_minus__g_constant
                                .with(|value| value.get())
                                .invoke(&[exp.clone(), env.clone()])
                        }
                    }
                })
            })
        });
        // (define (wrap-sexpr exp node) (make-comment exp node))
        globals::wrap_minus_sexpr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let node = args[1].clone();
                    // (letrec () (make-comment exp node))
                    {
                        // (make-comment exp node)
                        imports::make_minus_comment
                            .with(|value| value.get())
                            .invoke(&[exp.clone(), node.clone()])
                    }
                })
            })
        });
        // (define (sexpr->constant exp env) (make-constant exp))
        globals::sexpr_minus__g_constant.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (make-constant exp))
                    {
                        // (make-constant exp)
                        imports::make_minus_constant
                            .with(|value| value.get())
                            .invoke(&[exp.clone()])
                    }
                })
            })
        });
        // (define (objectify-symbol name env) (let ((var (ensure-var! name env))) (if (keyword? var) var (make-reference name var))))
        globals::objectify_minus_symbol.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let ((var (ensure-var! name env))) (if (keyword? var) var (make-reference name var))))
                    {
                        // (let ((var (ensure-var! name env))) (if (keyword? var) var (make-reference name var)))
                        {
                            let [var] = [
                                // (ensure-var! name env)
                                imports::ensure_minus_var_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            // (letrec () (if (keyword? var) var (make-reference name var)))
                            {
                                if (
                                    // (keyword? var)
                                    imports::keyword_p
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()])
                                )
                                .is_true()
                                {
                                    var.clone()
                                } else {
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
        });
        // (define (sexpr->reference name env) (let ((var (ensure-var! name env))) (make-reference name var)))
        globals::sexpr_minus__g_reference.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let ((var (ensure-var! name env))) (make-reference name var)))
                    {
                        // (let ((var (ensure-var! name env))) (make-reference name var))
                        {
                            let [var] = [
                                // (ensure-var! name env)
                                imports::ensure_minus_var_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            // (letrec () (make-reference name var))
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
        });
        // (define (sexpr->assignment name exp env) (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val)))
        globals::sexpr_minus__g_assignment.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let exp = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val)))
                    {
                        // (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val))
                        {
                            let [val, var] = [
                                // (sexpr->ast exp env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone(), env.clone(), Scm::False]),
                                // (ensure-var! name env)
                                imports::ensure_minus_var_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            // (letrec () (variable-set-mutable! var) (make-assignment name var val))
                            {
                                {
                                    // (variable-set-mutable! var)
                                    imports::variable_minus_set_minus_mutable_i
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()]);
                                    // (make-assignment name var val)
                                    imports::make_minus_assignment
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), var.clone(), val.clone()])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->definition exp env) (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val)))
        globals::sexpr_minus__g_definition.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let exp = args[0].clone();let env = args[1].clone();
// (letrec () (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val)))
{
// (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val))
{let [name, ] = [
// (definition-variable exp)
imports::definition_minus_variable.with(|value| value.get()).invoke(&[exp.clone(),]),];
// (letrec () (let* ((value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val)))
{
// (let* ((value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val))
{let [value, ] = [
// (definition-value exp)
imports::definition_minus_value.with(|value| value.get()).invoke(&[exp.clone(),]),];
// (letrec () (let* ((var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val)))
{
// (let* ((var (ensure-var! name env)) (val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val))
{let [var, ] = [
// (ensure-var! name env)
imports::ensure_minus_var_i.with(|value| value.get()).invoke(&[name.clone(),env.clone(),]),];
// (letrec () (let* ((val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val)))
{
// (let* ((val (sexpr->ast value env #f))) (global-add-definition! var val) (make-definition name var val))
{let [val, ] = [
// (sexpr->ast value env #f)
globals::sexpr_minus__g_ast.with(|value| value.get()).invoke(&[value.clone(),env.clone(),Scm::False,]),];
// (letrec () (let* () (global-add-definition! var val) (make-definition name var val)))
{
// (let* () (global-add-definition! var val) (make-definition name var val))
{
// (global-add-definition! var val)
imports::global_minus_add_minus_definition_i.with(|value| value.get()).invoke(&[var.clone(),val.clone(),]);
// (make-definition name var val)
imports::make_minus_definition.with(|value| value.get()).invoke(&[name.clone(),var.clone(),val.clone(),])}}}}}}}}}}})}));
        // (define (sexpr->application func arg* env tail?) (if (eq? (quote ABSTRACTION) (func (quote kind))) (sexpr->fixlet (sexpr->ast func env #f) arg* env tail?) (sexpr->regular-application func arg* env tail?)))
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
                    // (letrec () (if (eq? (quote ABSTRACTION) (func (quote kind))) (sexpr->fixlet (sexpr->ast func env #f) arg* env tail?) (sexpr->regular-application func arg* env tail?)))
                    {
                        if (
                            // (eq? (quote ABSTRACTION) (func (quote kind)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("ABSTRACTION"),
                                // (func (quote kind))
                                func.clone().invoke(&[Scm::symbol("kind")]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->fixlet (sexpr->ast func env #f) arg* env tail?)
                            globals::sexpr_minus__g_fixlet
                                .with(|value| value.get())
                                .invoke(&[
                                    // (sexpr->ast func env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[func.clone(), env.clone(), Scm::False]),
                                    arg_star_.clone(),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else {
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
        });
        // (define (sexpr->regular-application func arg* env tail?) (let ((args (sexpr->args arg* env))) (make-application func args tail?)))
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
                    // (letrec () (let ((args (sexpr->args arg* env))) (make-application func args tail?)))
                    {
                        // (let ((args (sexpr->args arg* env))) (make-application func args tail?))
                        {
                            let [args_] = [
                                // (sexpr->args arg* env)
                                globals::sexpr_minus__g_args
                                    .with(|value| value.get())
                                    .invoke(&[arg_star_.clone(), env.clone()]),
                            ];
                            // (letrec () (make-application func args tail?))
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
        });
        // (define (sexpr->fixlet func arg* env tail?) (let* ((args (sexpr->args arg* env)) (func (func (quote inner-function)))) (make-fixlet (func (quote get-params)) (func (quote get-body)) args)))
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
                    // (letrec () (let* ((args (sexpr->args arg* env)) (func (func (quote inner-function)))) (make-fixlet (func (quote get-params)) (func (quote get-body)) args)))
                    {
                        // (let* ((args (sexpr->args arg* env)) (func (func (quote inner-function)))) (make-fixlet (func (quote get-params)) (func (quote get-body)) args))
                        {
                            let [args_] = [
                                // (sexpr->args arg* env)
                                globals::sexpr_minus__g_args
                                    .with(|value| value.get())
                                    .invoke(&[arg_star_.clone(), env.clone()]),
                            ];
                            // (letrec () (let* ((func (func (quote inner-function)))) (make-fixlet (func (quote get-params)) (func (quote get-body)) args)))
                            {
                                // (let* ((func (func (quote inner-function)))) (make-fixlet (func (quote get-params)) (func (quote get-body)) args))
                                {
                                    let [func] = [
                                        // (func (quote inner-function))
                                        func.clone().invoke(&[Scm::symbol("inner-function")]),
                                    ];
                                    // (letrec () (let* () (make-fixlet (func (quote get-params)) (func (quote get-body)) args)))
                                    {
                                        // (let* () (make-fixlet (func (quote get-params)) (func (quote get-body)) args))

                                        // (make-fixlet (func (quote get-params)) (func (quote get-body)) args)
                                        imports::make_minus_fixlet.with(|value| value.get()).invoke(
                                            &[
                                                // (func (quote get-params))
                                                func.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (func (quote get-body))
                                                func.clone().invoke(&[Scm::symbol("get-body")]),
                                                args_.clone(),
                                            ],
                                        )
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->args arg* env) (if (null? arg*) (make-null-arg) (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))))
        globals::sexpr_minus__g_args.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let arg_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (if (null? arg*) (make-null-arg) (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))))
                    {
                        if (
                            // (null? arg*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[arg_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-null-arg)
                            imports::make_minus_null_minus_arg
                                .with(|value| value.get())
                                .invoke(&[])
                        } else {
                            // (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))
                            imports::make_minus_args.with(|value| value.get()).invoke(&[
                                // (sexpr->ast (car arg*) env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car arg*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[arg_star_.clone()]),
                                        env.clone(),
                                        Scm::False,
                                    ]),
                                // (sexpr->args (cdr arg*) env)
                                globals::sexpr_minus__g_args
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr arg*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[arg_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-seq bindings body env tail?) (if (null? bindings) (sexpr->sequence body env tail?) (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)))
        globals::sexpr_minus__g_scope_minus_seq.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (if (null? bindings) (sexpr->sequence body env tail?) (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)))
                    {
                        if (
                            // (null? bindings)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[bindings.clone()])
                        )
                        .is_true()
                        {
                            // (sexpr->sequence body env tail?)
                            globals::sexpr_minus__g_sequence
                                .with(|value| value.get())
                                .invoke(&[body.clone(), env.clone(), tail_p.clone()])
                        } else {
                            // (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)
                            globals::sexpr_minus__g_scope_minus_let
                                .with(|value| value.get())
                                .invoke(&[
                                    // (list (car bindings))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (car bindings)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[bindings.clone()]),
                                    ]),
                                    // (list (cons (quote let*) (cons (cdr bindings) body)))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (cons (quote let*) (cons (cdr bindings) body))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            Scm::symbol("let*"),
                                            // (cons (cdr bindings) body)
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                // (cdr bindings)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[bindings.clone()]),
                                                body.clone(),
                                            ]),
                                        ]),
                                    ]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-rec bindings body env tail?) (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
        globals::sexpr_minus__g_scope_minus_rec.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
                    {
                        // (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                        {
                            let [params] = [
                                // (map (lambda (b) (car b)) bindings)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let b = args[0].clone();
                                            // (letrec () (car b))
                                            {
                                                // (car b)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[b.clone()])
                                            }
                                        })
                                    },
                                    bindings.clone(),
                                ]),
                            ];
                            // (letrec () (let* ((body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
                            {
                                // (let* ((body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                                {
                                    let [body_minus_env] = [
                                        // (adjoin-boxed-env params env)
                                        imports::adjoin_minus_boxed_minus_env
                                            .with(|value| value.get())
                                            .invoke(&[params.clone(), env.clone()]),
                                    ];
                                    // (letrec () (let* ((args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
                                    {
                                        // (let* ((args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                                        {
                                            let [args_] = [
                                                // (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings)
                                                imports::map.with(|value| value.get()).invoke(&[
                                                    {
                                                        let body_minus_env = body_minus_env.clone();
                                                        Scm::func(move |args: &[Scm]| {
                                                            if args.len() != 1 {
                                                                panic!("invalid arity")
                                                            }
                                                            let b = args[0].clone();
                                                            // (letrec () (sexpr->ast (cadr b) body-env #f))
                                                            {
                                                                // (sexpr->ast (cadr b) body-env #f)
                                                                globals::sexpr_minus__g_ast
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (cadr b)
                                                                        imports::cadr
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[b.clone()]),
                                                                        body_minus_env.clone(),
                                                                        Scm::False,
                                                                    ])
                                                            }
                                                        })
                                                    },
                                                    bindings.clone(),
                                                ]),
                                            ];
                                            // (letrec () (let* () (make-scope params (sexpr->sequence body body-env tail?) args)))
                                            {
                                                // (let* () (make-scope params (sexpr->sequence body body-env tail?) args))

                                                // (make-scope params (sexpr->sequence body body-env tail?) args)
                                                imports::make_minus_scope
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        params.clone(),
                                                        // (sexpr->sequence body body-env tail?)
                                                        globals::sexpr_minus__g_sequence
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                body.clone(),
                                                                body_minus_env.clone(),
                                                                tail_p.clone(),
                                                            ]),
                                                        args_.clone(),
                                                    ])
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-let bindings body env tail?) (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings)) (func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?)))
        globals::sexpr_minus__g_scope_minus_let.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings)) (func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?)))
                    {
                        // (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings)) (func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?))
                        {
                            let [param_star_] = [
                                // (map (lambda (b) (car b)) bindings)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let b = args[0].clone();
                                            // (letrec () (car b))
                                            {
                                                // (car b)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[b.clone()])
                                            }
                                        })
                                    },
                                    bindings.clone(),
                                ]),
                            ];
                            // (letrec () (let* ((arg* (map (lambda (b) (cadr b)) bindings)) (func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?)))
                            {
                                // (let* ((arg* (map (lambda (b) (cadr b)) bindings)) (func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?))
                                {
                                    let [arg_star_] = [
                                        // (map (lambda (b) (cadr b)) bindings)
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let b = args[0].clone();
                                                    // (letrec () (cadr b))
                                                    {
                                                        // (cadr b)
                                                        imports::cadr
                                                            .with(|value| value.get())
                                                            .invoke(&[b.clone()])
                                                    }
                                                })
                                            },
                                            bindings.clone(),
                                        ]),
                                    ];
                                    // (letrec () (let* ((func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?)))
                                    {
                                        // (let* ((func (sexpr->abstraction param* body env))) (sexpr->fixlet func arg* env tail?))
                                        {
                                            let [func] = [
                                                // (sexpr->abstraction param* body env)
                                                globals::sexpr_minus__g_abstraction
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        param_star_.clone(),
                                                        body.clone(),
                                                        env.clone(),
                                                    ]),
                                            ];
                                            // (letrec () (let* () (sexpr->fixlet func arg* env tail?)))
                                            {
                                                // (let* () (sexpr->fixlet func arg* env tail?))

                                                // (sexpr->fixlet func arg* env tail?)
                                                globals::sexpr_minus__g_fixlet
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        func.clone(),
                                                        arg_star_.clone(),
                                                        env.clone(),
                                                        tail_p.clone(),
                                                    ])
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->abstraction param* body env) (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-closure (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))) (make-closure (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))))))
        globals::sexpr_minus__g_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let param_star_ = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-closure (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))) (make-closure (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))))))
                    {
                        // (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-closure (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))) (make-closure (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t)))))
                        {
                            let [local_minus_env, body] = [
                                // (adjoin-local-env param* env)
                                imports::adjoin_minus_local_minus_env
                                    .with(|value| value.get())
                                    .invoke(&[param_star_.clone(), env.clone()]),
                                // (scan-out-defines body)
                                imports::scan_minus_out_minus_defines
                                    .with(|value| value.get())
                                    .invoke(&[body.clone()]),
                            ];
                            // (letrec () (if (dotted-list? param*) (make-closure (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))) (make-closure (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t)))))
                            {
                                if (
                                    // (dotted-list? param*)
                                    imports::dotted_minus_list_p
                                        .with(|value| value.get())
                                        .invoke(&[param_star_.clone()])
                                )
                                .is_true()
                                {
                                    // (make-closure (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t)))
                                    imports::make_minus_closure
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))
                                            imports::make_minus_vararg_minus_abstraction
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (proper-list-part param*)
                                                    imports::proper_minus_list_minus_part
                                                        .with(|value| value.get())
                                                        .invoke(&[param_star_.clone()]),
                                                    // (last-cdr param*)
                                                    imports::last_minus_cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[param_star_.clone()]),
                                                    // (map (lambda (p) (lookup p local-env)) (proper-list-part param*))
                                                    imports::map.with(|value| value.get()).invoke(
                                                        &[
                                                            {
                                                                let local_minus_env =
                                                                    local_minus_env.clone();
                                                                Scm::func(move |args: &[Scm]| {
                                                                    if args.len() != 1 {
                                                                        panic!("invalid arity")
                                                                    }
                                                                    let p = args[0].clone();
                                                                    // (letrec () (lookup p local-env))
                                                                    {
                                                                        // (lookup p local-env)
                                                                        imports::lookup
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                p.clone(),
                                                                                local_minus_env
                                                                                    .clone(),
                                                                            ])
                                                                    }
                                                                })
                                                            },
                                                            // (proper-list-part param*)
                                                            imports::proper_minus_list_minus_part
                                                                .with(|value| value.get())
                                                                .invoke(&[param_star_.clone()]),
                                                        ],
                                                    ),
                                                    // (lookup (last-cdr param*) local-env)
                                                    imports::lookup
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (last-cdr param*)
                                                            imports::last_minus_cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[param_star_.clone()]),
                                                            local_minus_env.clone(),
                                                        ]),
                                                    // (sexpr->sequence body local-env #t)
                                                    globals::sexpr_minus__g_sequence
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            body.clone(),
                                                            local_minus_env.clone(),
                                                            Scm::True,
                                                        ]),
                                                ]),
                                        ])
                                } else {
                                    // (make-closure (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t)))
                                    imports::make_minus_closure
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))
                                            imports::make_minus_abstraction
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    param_star_.clone(),
                                                    // (map (lambda (p) (lookup p local-env)) param*)
                                                    imports::map.with(|value| value.get()).invoke(
                                                        &[
                                                            {
                                                                let local_minus_env =
                                                                    local_minus_env.clone();
                                                                Scm::func(move |args: &[Scm]| {
                                                                    if args.len() != 1 {
                                                                        panic!("invalid arity")
                                                                    }
                                                                    let p = args[0].clone();
                                                                    // (letrec () (lookup p local-env))
                                                                    {
                                                                        // (lookup p local-env)
                                                                        imports::lookup
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                p.clone(),
                                                                                local_minus_env
                                                                                    .clone(),
                                                                            ])
                                                                    }
                                                                })
                                                            },
                                                            param_star_.clone(),
                                                        ],
                                                    ),
                                                    // (sexpr->sequence body local-env #t)
                                                    globals::sexpr_minus__g_sequence
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            body.clone(),
                                                            local_minus_env.clone(),
                                                            Scm::True,
                                                        ]),
                                                ]),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->sequence expr* env tail?) (if (null? expr*) (error "empty sequence")) (if (null? (cdr expr*)) (sexpr->ast (car expr*) env tail?) (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))))
        globals::sexpr_minus__g_sequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let expr_star_ = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (if (null? expr*) (error "empty sequence")) (if (null? (cdr expr*)) (sexpr->ast (car expr*) env tail?) (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))))
                    {
                        {
                            if (
                                // (null? expr*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[expr_star_.clone()])
                            )
                            .is_true()
                            {
                                // (error "empty sequence")
                                imports::error
                                    .with(|value| value.get())
                                    .invoke(&[Scm::from("empty sequence")])
                            } else {
                                Scm::symbol("*UNSPECIFIED*")
                            };
                            if (
                                // (null? (cdr expr*))
                                imports::null_p.with(|value| value.get()).invoke(&[
                                    // (cdr expr*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[expr_star_.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->ast (car expr*) env tail?)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car expr*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[expr_star_.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            } else {
                                // (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))
                                {
                                    let [first] = [
                                        // (sexpr->ast (car expr*) env #f)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car expr*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[expr_star_.clone()]),
                                                env.clone(),
                                                Scm::False,
                                            ]),
                                    ];
                                    // (letrec () (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))
                                    {
                                        // (make-sequence first (sexpr->sequence (cdr expr*) env tail?))
                                        imports::make_minus_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                first.clone(),
                                                // (sexpr->sequence (cdr expr*) env tail?)
                                                globals::sexpr_minus__g_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (cdr expr*)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[expr_star_.clone()]),
                                                        env.clone(),
                                                        tail_p.clone(),
                                                    ]),
                                            ])
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->cond clauses env tail?) (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))))
        globals::sexpr_minus__g_cond.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let clauses = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))))
                    {
                        // (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))))
                        if (
                            // (null? clauses)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[clauses.clone()])
                        )
                        .is_true()
                        {
                            // (make-constant (quote *UNSPECIFIED*))
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("*UNSPECIFIED*")])
                        } else if (
                            // (eq? (quote else) (cond-clause-condition (car clauses)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("else"),
                                // (cond-clause-condition (car clauses))
                                imports::cond_minus_clause_minus_condition
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car clauses)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[clauses.clone()]),
                                    ]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
                            globals::sexpr_minus__g_sequence
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cond-clause-sequence (car clauses))
                                    imports::cond_minus_clause_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car clauses)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[clauses.clone()]),
                                        ]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else if (
                            // (pair? clauses)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[clauses.clone()])
                        )
                        .is_true()
                        {
                            // (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                            {
                                let [condition] = [
                                    // (sexpr->ast (cond-clause-condition (car clauses)) env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (cond-clause-condition (car clauses))
                                            imports::cond_minus_clause_minus_condition
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (car clauses)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[clauses.clone()]),
                                                ]),
                                            env.clone(),
                                            Scm::False,
                                        ]),
                                ];
                                // (letrec () (let* ((sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))
                                {
                                    // (let* ((sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                                    {
                                        let [sequence] = [
                                            // (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
                                            globals::sexpr_minus__g_sequence
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cond-clause-sequence (car clauses))
                                                    imports::cond_minus_clause_minus_sequence
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (car clauses)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[clauses.clone()]),
                                                        ]),
                                                    env.clone(),
                                                    tail_p.clone(),
                                                ]),
                                        ];
                                        // (letrec () (let* ((rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))
                                        {
                                            // (let* ((rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                                            {
                                                let [rest] = [
                                                    // (sexpr->cond (cdr clauses) env tail?)
                                                    globals::sexpr_minus__g_cond
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (cdr clauses)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[clauses.clone()]),
                                                            env.clone(),
                                                            tail_p.clone(),
                                                        ]),
                                                ];
                                                // (letrec () (let* () (make-alternative condition sequence rest)))
                                                {
                                                    // (let* () (make-alternative condition sequence rest))

                                                    // (make-alternative condition sequence rest)
                                                    imports::make_minus_alternative
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            condition.clone(),
                                                            sequence.clone(),
                                                            rest.clone(),
                                                        ])
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            Scm::symbol("*UNSPECIFIED*")
                        }
                    }
                })
            })
        });
        // (define (sexpr->and args env tail?) (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f)))))
        globals::sexpr_minus__g_and.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let args_ = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f)))))
                    {
                        // (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f))))
                        if (
                            // (null? args)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[args_.clone()])
                        )
                        .is_true()
                        {
                            // (make-constant #t)
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[Scm::True])
                        } else if (
                            // (null? (cdr args))
                            imports::null_p.with(|value| value.get()).invoke(&[
                                // (cdr args)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[args_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->ast (car args) env tail?)
                            globals::sexpr_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car args)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[args_.clone()]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else {
                            // (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f))
                            imports::make_minus_alternative
                                .with(|value| value.get())
                                .invoke(&[
                                    // (sexpr->ast (car args) env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                            env.clone(),
                                            Scm::False,
                                        ]),
                                    // (sexpr->and (cdr args) env tail?)
                                    globals::sexpr_minus__g_and
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (cdr args)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                            env.clone(),
                                            tail_p.clone(),
                                        ]),
                                    // (make-constant #f)
                                    imports::make_minus_constant
                                        .with(|value| value.get())
                                        .invoke(&[Scm::False]),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->import stmt* env) (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (sexpr->import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
        globals::sexpr_minus__g_import.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let stmt_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (sexpr->import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
                    {
                        // (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (sexpr->import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))))
                        if (
                            // (null? stmt*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[stmt_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else if (
                            // (equal? (quote (sunny testing)) (car stmt*))
                            imports::equal_p.with(|value| value.get()).invoke(&[
                                Scm::pair(
                                    Scm::symbol("sunny"),
                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                ),
                                // (car stmt*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[stmt_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->import (cdr stmt*) env)
                            globals::sexpr_minus__g_import
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr stmt*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[stmt_star_.clone()]),
                                    env.clone(),
                                ])
                        } else if (
                            // (eq? (quote only) (caar stmt*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("only"),
                                // (caar stmt*)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[stmt_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cons (sexpr->import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (sexpr->import-only (cadar stmt*) (cddar stmt*) env)
                                globals::sexpr_minus__g_import_minus_only
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadar stmt*)
                                        imports::cadar
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        // (cddar stmt*)
                                        imports::cddar
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                                // (sexpr->import (cdr stmt*) env)
                                globals::sexpr_minus__g_import
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr stmt*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        } else {
                            // (cons (sexpr->import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (sexpr->import-all (car stmt*) env)
                                globals::sexpr_minus__g_import_minus_all
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car stmt*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                                // (sexpr->import (cdr stmt*) env)
                                globals::sexpr_minus__g_import
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr stmt*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->export export-spec* env) (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env)))))
        globals::sexpr_minus__g_export.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let export_minus_spec_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env)))))
                    {
                        // (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))))
                        if (
                            // (null? export-spec*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[export_minus_spec_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else {
                            // (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (make-export env (car export-spec*) (car export-spec*))
                                imports::make_minus_export
                                    .with(|value| value.get())
                                    .invoke(&[
                                        env.clone(),
                                        // (car export-spec*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                        // (car export-spec*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                    ]),
                                // (sexpr->export (cdr export-spec*) env)
                                globals::sexpr_minus__g_export
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr export-spec*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->testsuite name cases env) (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases)))
        globals::sexpr_minus__g_testsuite.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let cases = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases)))
                    {
                        // (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases))
                        imports::make_minus_testsuite
                            .with(|value| value.get())
                            .invoke(&[
                                name.clone(),
                                // (map (lambda (case) (sexpr->testcase case env)) cases)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        let env = env.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let case = args[0].clone();
                                            // (letrec () (sexpr->testcase case env))
                                            {
                                                // (sexpr->testcase case env)
                                                globals::sexpr_minus__g_testcase
                                                    .with(|value| value.get())
                                                    .invoke(&[case.clone(), env.clone()])
                                            }
                                        })
                                    },
                                    cases.clone(),
                                ]),
                            ])
                    }
                })
            })
        });
        // (define (sexpr->testcase case env) (define (given stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)) (define (when stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt))) (define (then stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))) (define (dispatch section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f))))
        globals::sexpr_minus__g_testcase.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let case = args[0].clone();
                    let env = args[1].clone();
                    // (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f))))
                    {
                        let given = Scm::uninitialized().into_boxed();
                        let when = Scm::uninitialized().into_boxed();
                        let then = Scm::uninitialized().into_boxed();
                        let dispatch = Scm::uninitialized().into_boxed();
                        given.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))
                                {
                                    // (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("let*"),
                                        // (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let assignment = args[0].clone();
                                                    // (letrec () (list (car assignment) (caddr assignment)))
                                                    {
                                                        // (list (car assignment) (caddr assignment))
                                                        imports::list
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (car assignment)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[assignment.clone()]),
                                                                // (caddr assignment)
                                                                imports::caddr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[assignment.clone()]),
                                                            ])
                                                    }
                                                })
                                            },
                                            // (cdr stmt)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[stmt.clone()]),
                                        ]),
                                        body.clone(),
                                    ])
                                }
                            })
                        });
                        when.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
                                {
                                    let loop_ = Scm::uninitialized().into_boxed();
                                    loop_.set({
                                        let body = body.clone();
                                        let loop_ = loop_.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let stmt_star_ = args[0].clone();
                                            // (letrec () (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))
                                            {
                                                // (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))
                                                if (
                                                    // (null? stmt*)
                                                    imports::null_p
                                                        .with(|value| value.get())
                                                        .invoke(&[stmt_star_.clone()])
                                                )
                                                .is_true()
                                                {
                                                    body.clone()
                                                } else if (
                                                    // (eq? (quote <-) (cadar stmt*))
                                                    imports::eq_p.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("<-"),
                                                            // (cadar stmt*)
                                                            imports::cadar
                                                                .with(|value| value.get())
                                                                .invoke(&[stmt_star_.clone()]),
                                                        ],
                                                    )
                                                )
                                                .is_true()
                                                {
                                                    // (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))
                                                    imports::list.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("let"),
                                                            // (list (list (caar stmt*) (caddar stmt*)))
                                                            imports::list
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (list (caar stmt*) (caddar stmt*))
                                                                    imports::list
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            // (caar stmt*)
                                                                            imports::caar
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    stmt_star_
                                                                                        .clone(),
                                                                                ]),
                                                                            // (caddar stmt*)
                                                                            imports::caddar
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    stmt_star_
                                                                                        .clone(),
                                                                                ]),
                                                                        ]),
                                                                ]),
                                                            // (loop (cdr stmt*))
                                                            loop_.get().invoke(&[
                                                                // (cdr stmt*)
                                                                imports::cdr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[stmt_star_.clone()]),
                                                            ]),
                                                        ],
                                                    )
                                                } else {
                                                    // (list (quote begin) (car stmt*) (loop (cdr stmt*)))
                                                    imports::list.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("begin"),
                                                            // (car stmt*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[stmt_star_.clone()]),
                                                            // (loop (cdr stmt*))
                                                            loop_.get().invoke(&[
                                                                // (cdr stmt*)
                                                                imports::cdr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[stmt_star_.clone()]),
                                                            ]),
                                                        ],
                                                    )
                                                }
                                            }
                                        })
                                    });

                                    // (loop (cdr stmt))
                                    loop_.get().invoke(&[
                                        // (cdr stmt)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt.clone()]),
                                    ])
                                }
                            })
                        });
                        then.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))
                                {
                                    // (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("begin"),
                                        // (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
                                        globals::append.with(|value| value.get()).invoke(&[
                                            // (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
                                            imports::map.with(|value| value.get()).invoke(&[
                                                {
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 1 {
                                                            panic!("invalid arity")
                                                        }
                                                        let pred = args[0].clone();
                                                        // (letrec () (list (quote assert) pred))
                                                        {
                                                            // (list (quote assert) pred)
                                                            imports::list
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    Scm::symbol("assert"),
                                                                    pred.clone(),
                                                                ])
                                                        }
                                                    })
                                                },
                                                // (cdr stmt)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[stmt.clone()]),
                                            ]),
                                            body.clone(),
                                        ]),
                                    ])
                                }
                            })
                        });
                        dispatch.set({
                            let given = given.clone();
                            let dispatch = dispatch.clone();
                            let when = when.clone();
                            let then = then.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let section_star_ = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))
                                {
                                    // (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))
                                    if (
                                        // (null? section*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[section_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        body.clone()
                                    } else if (
                                        // (eq? (quote given) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("given"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (given (car section*) (dispatch (cdr section*) body))
                                        given.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else if (
                                        // (eq? (quote when) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("when"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (when (car section*) (dispatch (cdr section*) body))
                                        when.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else if (
                                        // (eq? (quote then) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("then"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (then (car section*) (dispatch (cdr section*) body))
                                        then.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else {
                                        // (error "invalid testcase")
                                        imports::error
                                            .with(|value| value.get())
                                            .invoke(&[Scm::from("invalid testcase")])
                                    }
                                }
                            })
                        });

                        // (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f)))
                        {
                            let [body] = [
                                // (dispatch (cddr case) (quote ()))
                                dispatch.get().invoke(&[
                                    // (cddr case)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[case.clone()]),
                                    Scm::Nil,
                                ]),
                            ];
                            // (letrec () (make-testcase (cadr case) (sexpr->ast body env #f)))
                            {
                                // (make-testcase (cadr case) (sexpr->ast body env #f))
                                imports::make_minus_testcase
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr case)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[case.clone()]),
                                        // (sexpr->ast body env #f)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[body.clone(), env.clone(), Scm::False]),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->assert cond env) (make-assert (sexpr->ast cond env #f)))
        globals::sexpr_minus__g_assert.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let cond = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (make-assert (sexpr->ast cond env #f)))
                    {
                        // (make-assert (sexpr->ast cond env #f))
                        imports::make_minus_assert
                            .with(|value| value.get())
                            .invoke(&[
                                // (sexpr->ast cond env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[cond.clone(), env.clone(), Scm::False]),
                            ])
                    }
                })
            })
        });
        // (define (sexpr->import-all lib env) (adjoin-import*! (library-exports (library-decls (get-lib lib))) env) (make-import lib))
        globals::sexpr_minus__g_import_minus_all.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (adjoin-import*! (library-exports (library-decls (get-lib lib))) env) (make-import lib))
                    {
                        {
                            // (adjoin-import*! (library-exports (library-decls (get-lib lib))) env)
                            imports::adjoin_minus_import_star__i
                                .with(|value| value.get())
                                .invoke(&[
                                    // (library-exports (library-decls (get-lib lib)))
                                    imports::library_minus_exports
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (library-decls (get-lib lib))
                                            imports::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (get-lib lib)
                                                    imports::get_minus_lib
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ]),
                                        ]),
                                    env.clone(),
                                ]);
                            // (make-import lib)
                            imports::make_minus_import
                                .with(|value| value.get())
                                .invoke(&[lib.clone()])
                        }
                    }
                })
            })
        });
        // (define (sexpr->import-only lib names env) (check-imports names (library-exports (library-decls (get-lib lib))) lib) (adjoin-import*! names env) (make-import-only lib names))
        globals::sexpr_minus__g_import_minus_only.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    let names = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (check-imports names (library-exports (library-decls (get-lib lib))) lib) (adjoin-import*! names env) (make-import-only lib names))
                    {
                        {
                            // (check-imports names (library-exports (library-decls (get-lib lib))) lib)
                            imports::check_minus_imports
                                .with(|value| value.get())
                                .invoke(&[
                                    names.clone(),
                                    // (library-exports (library-decls (get-lib lib)))
                                    imports::library_minus_exports
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (library-decls (get-lib lib))
                                            imports::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (get-lib lib)
                                                    imports::get_minus_lib
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ]),
                                        ]),
                                    lib.clone(),
                                ]);
                            // (adjoin-import*! names env)
                            imports::adjoin_minus_import_star__i
                                .with(|value| value.get())
                                .invoke(&[names.clone(), env.clone()]);
                            // (make-import-only lib names)
                            imports::make_minus_import_minus_only
                                .with(|value| value.get())
                                .invoke(&[lib.clone(), names.clone()])
                        }
                    }
                })
            })
        })
    };
}
