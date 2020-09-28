#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::globals::abstraction_p;
    pub use super::globals::and_minus_args;
    pub use super::globals::begin_minus_statements;
    pub use super::globals::cond_minus_clause_minus_condition;
    pub use super::globals::cond_minus_clause_minus_sequence;
    pub use super::globals::cond_minus_clauses;
    pub use super::globals::cond_minus_else_minus_clause_p;
    pub use super::globals::definition_minus_signature;
    pub use super::globals::definition_minus_value;
    pub use super::globals::definition_minus_variable;
    pub use super::globals::definition_p;
    pub use super::globals::if_minus_alternative;
    pub use super::globals::if_minus_condition;
    pub use super::globals::if_minus_consequence;
    pub use super::globals::import_minus_libnames;
    pub use super::globals::import_p;
    pub use super::globals::library_p;
    pub use super::globals::scan_minus_out_minus_defines;
    pub use super::globals::set_i_minus_value;
    pub use super::globals::set_i_minus_variable;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static set_i_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set!-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static set_i_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set!-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scan_minus_out_minus_defines: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scan-out-defines"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_libnames: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-libnames"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL importset-libname"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_consequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-consequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_signature: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-signature"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_else_minus_clause_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-else-clause?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clauses: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clauses"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static begin_minus_statements: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL begin-statements"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static and_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL and-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static abstraction_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL abstraction?"))}
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
    crate::sunny::utils::initialize();
    {
        (/*NOP*/);
        // (define (abstraction? expr) ...)
        globals::abstraction_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (and (pair? expr) (eq? (quote lambda) (car expr))))
                    {
                        // (and (pair? expr) (eq? (quote lambda) (car expr)))
                        if (
                            // (pair? expr)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (quote lambda) (car expr))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("lambda"),
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (and-args expr) ...)
        globals::and_minus_args.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cdr expr))
                    {
                        // (cdr expr)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (begin-statements expr) ...)
        globals::begin_minus_statements.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cdr expr))
                    {
                        // (cdr expr)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (cond-clauses expr) ...)
        globals::cond_minus_clauses.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cdr expr))
                    {
                        // (cdr expr)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (cond-clause-condition clause) ...)
        globals::cond_minus_clause_minus_condition.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let clause = args[0].clone();
                    // (letrec () (car clause))
                    {
                        // (car clause)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[clause.clone()])
                    }
                })
            })
        });
        // (define (cond-clause-sequence clause) ...)
        globals::cond_minus_clause_minus_sequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let clause = args[0].clone();
                    // (letrec () (cdr clause))
                    {
                        // (cdr clause)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[clause.clone()])
                    }
                })
            })
        });
        // (define (cond-else-clause? clause) ...)
        globals::cond_minus_else_minus_clause_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let clause = args[0].clone();
                    // (letrec () (eq? (quote else) (car clause)))
                    {
                        // (eq? (quote else) (car clause))
                        imports::eq_p.with(|value| value.get()).invoke(&[
                            Scm::symbol("else"),
                            // (car clause)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[clause.clone()]),
                        ])
                    }
                })
            })
        });
        // (define (definition? expr) ...)
        globals::definition_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (and (pair? expr) (eq? (car expr) (quote define))))
                    {
                        // (and (pair? expr) (eq? (car expr) (quote define)))
                        if (
                            // (pair? expr)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (car expr) (quote define))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                                Scm::symbol("define"),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (definition-signature expr) ...)
        globals::definition_minus_signature.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cadr expr)) (list (cadr expr) (quote ...)) (cdr expr)))
                    {
                        if (
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (list (cadr expr) (quote ...))
                            imports::list.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                                Scm::symbol("..."),
                            ])
                        } else {
                            // (cdr expr)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    }
                })
            })
        });
        // (define (definition-variable expr) ...)
        globals::definition_minus_variable.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cadr expr)) (caadr expr) (cadr expr)))
                    {
                        if (
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (caadr expr)
                            imports::caadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        } else {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    }
                })
            })
        });
        // (define (definition-value expr) ...)
        globals::definition_minus_value.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cadr expr)) (cons (quote lambda) (cons (cdadr expr) (cddr expr))) (caddr expr)))
                    {
                        if (
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cons (quote lambda) (cons (cdadr expr) (cddr expr)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                Scm::symbol("lambda"),
                                // (cons (cdadr expr) (cddr expr))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    // (cdadr expr)
                                    imports::cdadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                    // (cddr expr)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ]),
                            ])
                        } else {
                            // (caddr expr)
                            imports::caddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    }
                })
            })
        });
        // (define (if-condition expr) ...)
        globals::if_minus_condition.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cadr expr))
                    {
                        // (cadr expr)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (if-consequence expr) ...)
        globals::if_minus_consequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (caddr expr))
                    {
                        // (caddr expr)
                        imports::caddr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (if-alternative expr) ...)
        globals::if_minus_alternative.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cdddr expr)) (cadddr expr) (quote (quote *UNSPECIFIED*))))
                    {
                        if (
                            // (pair? (cdddr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cdddr expr)
                                imports::cdddr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cadddr expr)
                            imports::cadddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        } else {
                            Scm::pair(
                                Scm::symbol("quote"),
                                Scm::pair(Scm::symbol("*UNSPECIFIED*"), Scm::Nil),
                            )
                        }
                    }
                })
            })
        });
        // (define (import? expr) ...)
        globals::import_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (and (pair? expr) (eq? (car expr) (quote import))))
                    {
                        // (and (pair? expr) (eq? (car expr) (quote import)))
                        if (
                            // (pair? expr)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (car expr) (quote import))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                                Scm::symbol("import"),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (import-libnames exp*) ...)
        globals::import_minus_libnames.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let exp_star_ = args[0].clone();
                    // (letrec () (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*))))
                    {
                        // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
                        imports::filter.with(|value| value.get()).invoke(&[
                            {
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let libname = args[0].clone();
                                    // (letrec () (not (equal? libname (quote (sunny testing)))))
                                    {
                                        // (not (equal? libname (quote (sunny testing))))
                                        imports::not.with(|value| value.get()).invoke(&[
                                            // (equal? libname (quote (sunny testing)))
                                            imports::equal_p.with(|value| value.get()).invoke(&[
                                                libname.clone(),
                                                Scm::pair(
                                                    Scm::symbol("sunny"),
                                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                                ),
                                            ]),
                                        ])
                                    }
                                })
                            },
                            // (map importset-libname (cdr exp*))
                            imports::map.with(|value| value.get()).invoke(&[
                                globals::importset_minus_libname.with(|value| value.get()),
                                // (cdr exp*)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (importset-libname expr) ...)
        globals::importset_minus_libname.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cond ((eq? (quote only) (car expr)) (importset-libname (cadr expr))) ((eq? (quote except) (car expr)) (importset-libname (cadr expr))) (else expr)))
                    {
                        // (cond ...)
                        if (
                            // (eq? (quote only) (car expr))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("only"),
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (importset-libname (cadr expr))
                            globals::importset_minus_libname
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ])
                        } else if (
                            // (eq? (quote except) (car expr))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("except"),
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (importset-libname (cadr expr))
                            globals::importset_minus_libname
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ])
                        } else {
                            expr.clone()
                        }
                    }
                })
            })
        });
        // (define (library? exp*) ...)
        globals::library_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let exp_star_ = args[0].clone();
                    // (letrec () (and (pair? exp*) (eq? (quote define-library) (car exp*))))
                    {
                        // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
                        if (
                            // (pair? exp*)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[exp_star_.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (quote define-library) (car exp*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("define-library"),
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (scan-out-defines body) ...)
        globals::scan_minus_out_minus_defines.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let body = args[0].clone();
                    // (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (list (cons (quote letrec) (cons (initializations body) (transform body)))))
                    {
                        let initializations = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        initializations.set({
                            let initializations = initializations.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let exp_star_ = args[0].clone();
                                // (letrec () (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))
                                {
                                    // (cond ...)
                                    if (
                                        // (null? exp*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[exp_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else if (
                                        // (definition? (car exp*))
                                        globals::definition_p.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (list (definition-variable (car exp*)) (definition-value (car exp*)))
                                            imports::list.with(|value| value.get()).invoke(&[
                                                // (definition-variable (car exp*))
                                                globals::definition_minus_variable
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()]),
                                                    ]),
                                                // (definition-value (car exp*))
                                                globals::definition_minus_value
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()]),
                                                    ]),
                                            ]),
                                            // (initializations (cdr exp*))
                                            initializations.get().invoke(&[
                                                // (cdr exp*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                            ]),
                                        ])
                                    } else {
                                        // (initializations (cdr exp*))
                                        initializations.get().invoke(&[
                                            // (cdr exp*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    }
                                }
                            })
                        });
                        transform.set({
                            let transform = transform.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let exp_star_ = args[0].clone();
                                // (letrec () (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))
                                {
                                    // (cond ...)
                                    if (
                                        // (null? exp*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[exp_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else if (
                                        // (definition? (car exp*))
                                        globals::definition_p.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (transform (cdr exp*))
                                        transform.get().invoke(&[
                                            // (cdr exp*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    } else {
                                        // (cons (car exp*) (transform (cdr exp*)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                            // (transform (cdr exp*))
                                            transform.get().invoke(&[
                                                // (cdr exp*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                            ]),
                                        ])
                                    }
                                }
                            })
                        });

                        // (list (cons (quote letrec) (cons (initializations body) (transform body))))
                        imports::list.with(|value| value.get()).invoke(&[
                            // (cons (quote letrec) (cons (initializations body) (transform body)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                Scm::symbol("letrec"),
                                // (cons (initializations body) (transform body))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    // (initializations body)
                                    initializations.get().invoke(&[body.clone()]),
                                    // (transform body)
                                    transform.get().invoke(&[body.clone()]),
                                ]),
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (set!-variable expr) ...)
        globals::set_i_minus_variable.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cadr expr))
                    {
                        // (cadr expr)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (set!-value expr) ...)
        globals::set_i_minus_value.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (caddr expr))
                    {
                        // (caddr expr)
                        imports::caddr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        })
    };
}
