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
    pub fn abstraction_p(args: &[Scm]) {
        {
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
                        imports::eq_p
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("lambda"), {
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            }])
                    }
                } else {
                    Scm::False
                }
            }
        }
    }
    pub fn and_minus_args(args: &[Scm]) {
        {
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
        }
    }
    pub fn assert_minus_condition(args: &[Scm]) {
        {
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
        }
    }
    pub fn begin_minus_statements(args: &[Scm]) {
        {
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
        }
    }
    pub fn cond_minus_clause_minus_condition(args: &[Scm]) {
        {
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
        }
    }
    pub fn cond_minus_clause_minus_sequence(args: &[Scm]) {
        {
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
        }
    }
    pub fn cond_minus_clauses(args: &[Scm]) {
        {
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
        }
    }
    pub fn cond_minus_else_minus_clause_p(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let clause = args[0].clone();
            {
                // (eq? (quote else) (car clause))
                imports::eq_p
                    .with(|value| value.get())
                    .invoke(&[Scm::symbol("else"), {
                        // (car clause)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[clause.clone()])
                    }])
            }
        }
    }
    pub fn definition_minus_signature(args: &[Scm]) {
        {
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
        }
    }
    pub fn definition_minus_value(args: &[Scm]) {
        {
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
                    imports::cons
                        .with(|value| value.get())
                        .invoke(&[Scm::symbol("lambda"), {
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
                        }])
                }
            } else {
                {
                    // (caddr expr)
                    imports::caddr
                        .with(|value| value.get())
                        .invoke(&[expr.clone()])
                }
            }
        }
    }
    pub fn definition_minus_variable(args: &[Scm]) {
        {
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
        }
    }
    pub fn definition_p(args: &[Scm]) {
        {
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
        }
    }
    pub fn if_minus_alternative(args: &[Scm]) {
        {
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
        }
    }
    pub fn if_minus_condition(args: &[Scm]) {
        {
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
        }
    }
    pub fn if_minus_consequence(args: &[Scm]) {
        {
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
        }
    }
    pub fn import_minus_libnames(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let exp_star_ = args[0].clone();
            {
                // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
                imports::filter.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let libname = args[0].clone();
                            {
                                // (not (equal? libname (quote (sunny testing))))
                                imports::not.with(|value| value.get()).invoke(&[{
                                    // (equal? libname (quote (sunny testing)))
                                    imports::equal_p.with(|value| value.get()).invoke(&[
                                        libname.clone(),
                                        Scm::pair(
                                            Scm::symbol("sunny"),
                                            Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                        ),
                                    ])
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
        }
    }
    pub fn import_p(args: &[Scm]) {
        {
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
        }
    }
    pub fn importset_minus_except_p(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let stmt = args[0].clone();
            {
                // (eq? (quote except) (car stmt))
                imports::eq_p
                    .with(|value| value.get())
                    .invoke(&[Scm::symbol("except"), {
                        // (car stmt)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[stmt.clone()])
                    }])
            }
        }
    }
    pub fn importset_minus_libname(args: &[Scm]) {
        {
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
        }
    }
    pub fn importset_minus_only_minus_names(args: &[Scm]) {
        {
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
        }
    }
    pub fn importset_minus_only_p(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let stmt = args[0].clone();
            {
                // (eq? (quote only) (car stmt))
                imports::eq_p
                    .with(|value| value.get())
                    .invoke(&[Scm::symbol("only"), {
                        // (car stmt)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[stmt.clone()])
                    }])
            }
        }
    }
    pub fn lambda_minus_body(args: &[Scm]) {
        {
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
        }
    }
    pub fn lambda_minus_params(args: &[Scm]) {
        {
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
        }
    }
    pub fn let_star__minus_bindings(args: &[Scm]) {
        {
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
        }
    }
    pub fn let_minus_args(args: &[Scm]) {
        {
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
        }
    }
    pub fn let_minus_body(args: &[Scm]) {
        {
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
        }
    }
    pub fn let_minus_vars(args: &[Scm]) {
        {
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
        }
    }
    pub fn library_p(args: &[Scm]) {
        {
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
        }
    }
    pub fn scan_minus_out_minus_defines(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let body = args[0].clone();
            {
                // (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body)))))))
                {
                    // (let ((initializations (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (set! transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))))
                    {
                        let [initializations, transform] = [
                            Scm::symbol("*uninitialized*"),
                            Scm::symbol("*uninitialized*"),
                        ];
                        {
                            let transform = transform.into_boxed();
                            {
                                let initializations = initializations.into_boxed();
                                {
                                    initializations.set({// Closure
let initializations = initializations.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
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
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}}}})});
                                    transform.set({
                                        // Closure
                                        let transform = transform.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let exp_star_ = args[0].clone();
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
                                                    Scm::Nil
                                                } else if ({
                                                    // (definition? (car exp*))
                                                    globals::definition_p
                                                        .with(|value| value.get())
                                                        .invoke(&[{
                                                            // (car exp*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[exp_star_.clone()])
                                                        }])
                                                })
                                                .is_true()
                                                {
                                                    {
                                                        // (transform (cdr exp*))
                                                        transform.get().invoke(&[{
                                                            // (cdr exp*)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[exp_star_.clone()])
                                                        }])
                                                    }
                                                } else {
                                                    {
                                                        // (cons (car exp*) (transform (cdr exp*)))
                                                        imports::cons
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                {
                                                                    // (car exp*)
                                                                    imports::car
                                                                        .with(|value| value.get())
                                                                        .invoke(
                                                                            &[exp_star_.clone()],
                                                                        )
                                                                },
                                                                {
                                                                    // (transform (cdr exp*))
                                                                    transform.get().invoke(&[{
                                                                        // (cdr exp*)
                                                                        imports::cdr
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ])
                                                                    }])
                                                                },
                                                            ])
                                                    }
                                                }
                                            }
                                        })
                                    });
                                    {
                                        // (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))
                                        {
                                            let ini = {
                                                // (initializations body)
                                                initializations.get().invoke(&[body.clone()])
                                            };
                                            if ({
                                                // (null? ini)
                                                imports::null_p
                                                    .with(|value| value.get())
                                                    .invoke(&[ini.clone()])
                                            })
                                            .is_true()
                                            {
                                                body.clone()
                                            } else {
                                                {
                                                    // (list (cons (quote letrec) (cons ini (transform body))))
                                                    imports::list.with(|value| value.get()).invoke(
                                                        &[{
                                                            // (cons (quote letrec) (cons ini (transform body)))
                                                            imports::cons
                                                                .with(|value| value.get())
                                                                .invoke(&[Scm::symbol("letrec"), {
                                                                    // (cons ini (transform body))
                                                                    imports::cons
                                                                        .with(|value| value.get())
                                                                        .invoke(&[ini.clone(), {
                                                                            // (transform body)
                                                                            transform.get().invoke(
                                                                                &[body.clone()],
                                                                            )
                                                                        }])
                                                                }])
                                                        }],
                                                    )
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    pub fn set_i_minus_value(args: &[Scm]) {
        {
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
        }
    }
    pub fn set_i_minus_variable(args: &[Scm]) {
        {
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
        }
    }
    pub fn testcase_minus_body(args: &[Scm]) {
        {
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
        }
    }
    pub fn testcase_minus_description(args: &[Scm]) {
        {
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
        }
    }
    pub fn testsuite_minus_cases(args: &[Scm]) {
        {
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
        }
    }
    pub fn testsuite_minus_name(args: &[Scm]) {
        {
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
        }
    }
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
            (/*NOP*/)
        };
        {
            // (define (and-args expr) ...)
            (/*NOP*/)
        };
        {
            // (define (assert-condition expr) ...)
            (/*NOP*/)
        };
        {
            // (define (begin-statements expr) ...)
            (/*NOP*/)
        };
        {
            // (define (cond-clauses expr) ...)
            (/*NOP*/)
        };
        {
            // (define (cond-clause-condition clause) ...)
            (/*NOP*/)
        };
        {
            // (define (cond-clause-sequence clause) ...)
            (/*NOP*/)
        };
        {
            // (define (cond-else-clause? clause) ...)
            (/*NOP*/)
        };
        {
            // (define (definition? expr) ...)
            (/*NOP*/)
        };
        {
            // (define (definition-signature expr) ...)
            (/*NOP*/)
        };
        {
            // (define (definition-variable expr) ...)
            (/*NOP*/)
        };
        {
            // (define (definition-value expr) ...)
            (/*NOP*/)
        };
        {
            // (define (if-condition expr) ...)
            (/*NOP*/)
        };
        {
            // (define (if-consequence expr) ...)
            (/*NOP*/)
        };
        {
            // (define (if-alternative expr) ...)
            (/*NOP*/)
        };
        {
            // (define (import? expr) ...)
            (/*NOP*/)
        };
        {
            // (define (import-libnames exp*) ...)
            (/*NOP*/)
        };
        {
            // (define (importset-libname stmt) ...)
            (/*NOP*/)
        };
        {
            // (define (importset-except? stmt) ...)
            (/*NOP*/)
        };
        {
            // (define (importset-only? stmt) ...)
            (/*NOP*/)
        };
        {
            // (define (importset-only-names stmt) ...)
            (/*NOP*/)
        };
        {
            // (define (lambda-body expr) ...)
            (/*NOP*/)
        };
        {
            // (define (lambda-params expr) ...)
            (/*NOP*/)
        };
        {
            // (define (let-args expr) ...)
            (/*NOP*/)
        };
        {
            // (define (let-body expr) ...)
            (/*NOP*/)
        };
        {
            // (define (let-vars expr) ...)
            (/*NOP*/)
        };
        {
            // (define (let*-bindings expr) ...)
            (/*NOP*/)
        };
        {
            // (define (library? exp*) ...)
            (/*NOP*/)
        };
        {
            // (define (scan-out-defines body) ...)
            (/*NOP*/)
        };
        {
            // (define (set!-variable expr) ...)
            (/*NOP*/)
        };
        {
            // (define (set!-value expr) ...)
            (/*NOP*/)
        };
        {
            // (define (testcase-body expr) ...)
            (/*NOP*/)
        };
        {
            // (define (testcase-description expr) ...)
            (/*NOP*/)
        };
        {
            // (define (testsuite-cases expr) ...)
            (/*NOP*/)
        };
        {
            // (define (testsuite-name expr) ...)
            (/*NOP*/)
        }
    };
}
