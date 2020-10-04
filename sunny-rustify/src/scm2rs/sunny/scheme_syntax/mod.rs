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
    pub use super::globals::assert_minus_condition;
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
    pub use super::globals::importset_minus_libname;
    pub use super::globals::importset_minus_only_minus_names;
    pub use super::globals::importset_minus_only_p;
    pub use super::globals::lambda_minus_body;
    pub use super::globals::lambda_minus_params;
    pub use super::globals::let_minus_args;
    pub use super::globals::let_minus_body;
    pub use super::globals::let_minus_vars;
    pub use super::globals::let_star__minus_bindings;
    pub use super::globals::library_p;
    pub use super::globals::scan_minus_out_minus_defines;
    pub use super::globals::set_i_minus_value;
    pub use super::globals::set_i_minus_variable;
    pub use super::globals::testcase_minus_body;
    pub use super::globals::testcase_minus_description;
    pub use super::globals::testsuite_minus_cases;
    pub use super::globals::testsuite_minus_name;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static abstraction_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION abstraction?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static and_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION and-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static assert_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION assert-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static begin_minus_statements: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION begin-statements"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION cond-clause-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION cond-clause-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clauses: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION cond-clauses"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_else_minus_clause_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION cond-else-clause?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_signature: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION definition-signature"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION definition-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION definition-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION definition?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION if-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION if-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_consequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION if-consequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_libnames: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION import-libnames"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION import?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_except_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION importset-except?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION importset-libname"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_only_minus_names: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION importset-only-names"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_only_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION importset-only?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static lambda_minus_body: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION lambda-body"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static lambda_minus_params: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION lambda-params"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static let_star__minus_bindings: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION let*-bindings"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static let_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION let-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static let_minus_body: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION let-body"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static let_minus_vars: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION let-vars"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION library?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scan_minus_out_minus_defines: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION scan-out-defines"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static set_i_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION set!-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static set_i_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION set!-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static testcase_minus_body: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION testcase-body"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static testcase_minus_description: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION testcase-description"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static testsuite_minus_cases: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION testsuite-cases"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static testsuite_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION testsuite-name"))}
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
        {
            // (define (abstraction? expr) ...)
            globals::abstraction_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (and (pair? expr) (eq? (quote lambda) (car expr)))
                            if ({
                                // (pair? expr)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (eq? (quote lambda) (car expr))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("lambda"),
                                        {
                                            // (car expr)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[expr.clone()])
                                        },
                                    ])
                                }
                            } else {
                                Scm::False
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (and-args expr) ...)
            globals::and_minus_args.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cdr expr)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (assert-condition expr) ...)
            globals::assert_minus_condition.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (begin-statements expr) ...)
            globals::begin_minus_statements.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cdr expr)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (cond-clauses expr) ...)
            globals::cond_minus_clauses.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cdr expr)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (cond-clause-condition clause) ...)
            globals::cond_minus_clause_minus_condition.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let clause = args[0].clone();
                        {
                            // (car clause)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[clause.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (cond-clause-sequence clause) ...)
            globals::cond_minus_clause_minus_sequence.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let clause = args[0].clone();
                        {
                            // (cdr clause)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[clause.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (cond-else-clause? clause) ...)
            globals::cond_minus_else_minus_clause_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let clause = args[0].clone();
                        {
                            // (eq? (quote else) (car clause))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("else"),
                                {
                                    // (car clause)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[clause.clone()])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (definition? expr) ...)
            globals::definition_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (and (pair? expr) (eq? (car expr) (quote define)))
                            if ({
                                // (pair? expr)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (eq? (car expr) (quote define))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        {
                                            // (car expr)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[expr.clone()])
                                        },
                                        Scm::symbol("define"),
                                    ])
                                }
                            } else {
                                Scm::False
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (definition-signature expr) ...)
            globals::definition_minus_signature.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        if ({
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[{
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (list (cadr expr) (quote ...))
                                imports::list.with(|value| value.get()).invoke(&[
                                    {
                                        // (cadr expr)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[expr.clone()])
                                    },
                                    Scm::symbol("..."),
                                ])
                            }
                        } else {
                            {
                                // (cdr expr)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (definition-variable expr) ...)
            globals::definition_minus_variable.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        if ({
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[{
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (caadr expr)
                                imports::caadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }
                        } else {
                            {
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (definition-value expr) ...)
            globals::definition_minus_value.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        if ({
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[{
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (cons (quote lambda) (cons (cdadr expr) (cddr expr)))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    Scm::symbol("lambda"),
                                    {
                                        // (cons (cdadr expr) (cddr expr))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            {
                                                // (cdadr expr)
                                                imports::cdadr
                                                    .with(|value| value.get())
                                                    .invoke(&[expr.clone()])
                                            },
                                            {
                                                // (cddr expr)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[expr.clone()])
                                            },
                                        ])
                                    },
                                ])
                            }
                        } else {
                            {
                                // (caddr expr)
                                imports::caddr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (if-condition expr) ...)
            globals::if_minus_condition.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (if-consequence expr) ...)
            globals::if_minus_consequence.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
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
        {
            // (define (if-alternative expr) ...)
            globals::if_minus_alternative.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        if ({
                            // (pair? (cdddr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[{
                                // (cdddr expr)
                                imports::cdddr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (cadddr expr)
                                imports::cadddr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }
                        } else {
                            Scm::pair(
                                Scm::symbol("quote"),
                                Scm::pair(Scm::symbol("*UNSPECIFIED*"), Scm::Nil),
                            )
                        }
                    })
                })
            })
        };
        {
            // (define (import? expr) ...)
            globals::import_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (and (pair? expr) (eq? (car expr) (quote import)))
                            if ({
                                // (pair? expr)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (eq? (car expr) (quote import))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        {
                                            // (car expr)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[expr.clone()])
                                        },
                                        Scm::symbol("import"),
                                    ])
                                }
                            } else {
                                Scm::False
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (import-libnames exp*) ...)
            globals::import_minus_libnames.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let exp_star_ = args[0].clone();
                        {
                            // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
                            imports::filter.with(|value| value.get()).invoke(&[
                                {
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let libname = args[0].clone();
                                        {
                                            // (not (equal? libname (quote (sunny testing))))
                                            imports::not.with(|value| value.get()).invoke(&[{
                                                // (equal? libname (quote (sunny testing)))
                                                imports::equal_p.with(|value| value.get()).invoke(
                                                    &[
                                                        libname.clone(),
                                                        Scm::pair(
                                                            Scm::symbol("sunny"),
                                                            Scm::pair(
                                                                Scm::symbol("testing"),
                                                                Scm::Nil,
                                                            ),
                                                        ),
                                                    ],
                                                )
                                            }])
                                        }
                                    })
                                },
                                {
                                    // (map importset-libname (cdr exp*))
                                    imports::map.with(|value| value.get()).invoke(&[
                                        globals::importset_minus_libname.with(|value| value.get()),
                                        {
                                            // (cdr exp*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()])
                                        },
                                    ])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (importset-libname stmt) ...)
            globals::importset_minus_libname.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let stmt = args[0].clone();
                        {
                            // (cond ...)
                            if ({
                                // (importset-only? stmt)
                                globals::importset_minus_only_p
                                    .with(|value| value.get())
                                    .invoke(&[stmt.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (importset-libname (cadr stmt))
                                    globals::importset_minus_libname
                                        .with(|value| value.get())
                                        .invoke(&[{
                                            // (cadr stmt)
                                            imports::cadr
                                                .with(|value| value.get())
                                                .invoke(&[stmt.clone()])
                                        }])
                                }
                            } else if ({
                                // (importset-except? stmt)
                                globals::importset_minus_except_p
                                    .with(|value| value.get())
                                    .invoke(&[stmt.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (importset-libname (cadr stmt))
                                    globals::importset_minus_libname
                                        .with(|value| value.get())
                                        .invoke(&[{
                                            // (cadr stmt)
                                            imports::cadr
                                                .with(|value| value.get())
                                                .invoke(&[stmt.clone()])
                                        }])
                                }
                            } else {
                                stmt.clone()
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (importset-except? stmt) ...)
            globals::importset_minus_except_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let stmt = args[0].clone();
                        {
                            // (eq? (quote except) (car stmt))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("except"),
                                {
                                    // (car stmt)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[stmt.clone()])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (importset-only? stmt) ...)
            globals::importset_minus_only_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let stmt = args[0].clone();
                        {
                            // (eq? (quote only) (car stmt))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("only"),
                                {
                                    // (car stmt)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[stmt.clone()])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (importset-only-names stmt) ...)
            globals::importset_minus_only_minus_names.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let stmt = args[0].clone();
                        {
                            // (cddr stmt)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[stmt.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (lambda-body expr) ...)
            globals::lambda_minus_body.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cddr expr)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (lambda-params expr) ...)
            globals::lambda_minus_params.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (let-args expr) ...)
            globals::let_minus_args.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (map cadr (cadr expr))
                            imports::map.with(|value| value.get()).invoke(&[
                                imports::cadr.with(|value| value.get()),
                                {
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (let-body expr) ...)
            globals::let_minus_body.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cddr expr)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (let-vars expr) ...)
            globals::let_minus_vars.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (map car (cadr expr))
                            imports::map.with(|value| value.get()).invoke(&[
                                imports::car.with(|value| value.get()),
                                {
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (let*-bindings expr) ...)
            globals::let_star__minus_bindings.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (library? exp*) ...)
            globals::library_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let exp_star_ = args[0].clone();
                        {
                            // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
                            if ({
                                // (pair? exp*)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (eq? (quote define-library) (car exp*))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("define-library"),
                                        {
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()])
                                        },
                                    ])
                                }
                            } else {
                                Scm::False
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (scan-out-defines body) ...)
            globals::scan_minus_out_minus_defines.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let body = args[0].clone();{
// (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body)))))))
{
// (let ((initializations (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (set! transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))))
{let [initializations, transform, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let transform = transform.into_boxed();{let initializations = initializations.into_boxed();{initializations.set({let initializations = initializations.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? exp*)
imports::null_p.with(|value| value.get()).invoke(&[exp_star_.clone()])}).is_true() {Scm::Nil} else if ({
// (definition? (car exp*))
globals::definition_p.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{
// (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
imports::cons.with(|value| value.get()).invoke(&[{
// (list (definition-variable (car exp*)) (definition-value (car exp*)))
imports::list.with(|value| value.get()).invoke(&[{
// (definition-variable (car exp*))
globals::definition_minus_variable.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])},{
// (definition-value (car exp*))
globals::definition_minus_value.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}])},{
// (initializations (cdr exp*))
initializations.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}])}} else {{
// (initializations (cdr exp*))
initializations.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}}}})});transform.set({let transform = transform.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? exp*)
imports::null_p.with(|value| value.get()).invoke(&[exp_star_.clone()])}).is_true() {Scm::Nil} else if ({
// (definition? (car exp*))
globals::definition_p.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{
// (transform (cdr exp*))
transform.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}} else {{
// (cons (car exp*) (transform (cdr exp*)))
imports::cons.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])},{
// (transform (cdr exp*))
transform.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}])}}}})});{
// (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))
{let ini = {
// (initializations body)
initializations.get().invoke(&[body.clone()])};if ({
// (null? ini)
imports::null_p.with(|value| value.get()).invoke(&[ini.clone()])}).is_true() {body.clone()} else {{
// (list (cons (quote letrec) (cons ini (transform body))))
imports::list.with(|value| value.get()).invoke(&[{
// (cons (quote letrec) (cons ini (transform body)))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("letrec"),{
// (cons ini (transform body))
imports::cons.with(|value| value.get()).invoke(&[ini.clone(),{
// (transform body)
transform.get().invoke(&[body.clone()])}])}])}])}}}}}}}}}}})}))
        };
        {
            // (define (set!-variable expr) ...)
            globals::set_i_minus_variable.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (set!-value expr) ...)
            globals::set_i_minus_value.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
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
        {
            // (define (testcase-body expr) ...)
            globals::testcase_minus_body.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cddr expr)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (testcase-description expr) ...)
            globals::testcase_minus_description.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (testsuite-cases expr) ...)
            globals::testsuite_minus_cases.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cddr expr)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (testsuite-name expr) ...)
            globals::testsuite_minus_name.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let expr = args[0].clone();
                        {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    })
                })
            })
        }
    };
}
