#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::abstraction_p;
    pub use super::and_minus_args;
    pub use super::assert_minus_condition;
    pub use super::begin_minus_statements;
    pub use super::cond_minus_clause_minus_condition;
    pub use super::cond_minus_clause_minus_sequence;
    pub use super::cond_minus_clauses;
    pub use super::cond_minus_else_minus_clause_p;
    pub use super::definition_minus_signature;
    pub use super::definition_minus_value;
    pub use super::definition_minus_variable;
    pub use super::definition_p;
    pub use super::if_minus_alternative;
    pub use super::if_minus_condition;
    pub use super::if_minus_consequence;
    pub use super::import_minus_libnames;
    pub use super::import_p;
    pub use super::importset_minus_libname;
    pub use super::importset_minus_only_minus_names;
    pub use super::importset_minus_only_p;
    pub use super::lambda_minus_body;
    pub use super::lambda_minus_params;
    pub use super::let_minus_args;
    pub use super::let_minus_body;
    pub use super::let_minus_vars;
    pub use super::let_star__minus_bindings;
    pub use super::library_p;
    pub use super::scan_minus_out_minus_defines;
    pub use super::set_i_minus_value;
    pub use super::set_i_minus_variable;
    pub use super::testcase_minus_body;
    pub use super::testcase_minus_description;
    pub use super::testsuite_minus_cases;
    pub use super::testsuite_minus_name;
}

pub fn abstraction_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (and (pair? expr) (eq? (quote lambda) (car expr)))
            if ({
                // (pair? expr)
                Scm::func(imports::pair_p).invoke(&[expr.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote lambda) (car expr))
                    Scm::func(imports::eq_p).invoke(&[Scm::symbol("lambda"), {
                        // (car expr)
                        Scm::func(imports::car).invoke(&[expr.clone()])
                    }])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn and_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cdr expr)
            Scm::func(imports::cdr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn assert_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn begin_minus_statements(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cdr expr)
            Scm::func(imports::cdr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause = args[0].clone();
        {
            // (car clause)
            Scm::func(imports::car).invoke(&[clause.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_sequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause = args[0].clone();
        {
            // (cdr clause)
            Scm::func(imports::cdr).invoke(&[clause.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clauses(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cdr expr)
            Scm::func(imports::cdr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn cond_minus_else_minus_clause_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause = args[0].clone();
        {
            // (eq? (quote else) (car clause))
            Scm::func(imports::eq_p).invoke(&[Scm::symbol("else"), {
                // (car clause)
                Scm::func(imports::car).invoke(&[clause.clone()])
            }])
        }
    }
    .into()
}
pub fn definition_minus_signature(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        if ({
            // (pair? (cadr expr))
            Scm::func(imports::pair_p).invoke(&[{
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }])
        })
        .is_true()
        {
            {
                // (list (cadr expr) (quote ...))
                Scm::func(imports::list).invoke(&[
                    {
                        // (cadr expr)
                        Scm::func(imports::cadr).invoke(&[expr.clone()])
                    },
                    Scm::symbol("..."),
                ])
            }
        } else {
            {
                // (cdr expr)
                Scm::func(imports::cdr).invoke(&[expr.clone()])
            }
        }
    }
    .into()
}
pub fn definition_minus_value(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        if ({
            // (pair? (cadr expr))
            Scm::func(imports::pair_p).invoke(&[{
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }])
        })
        .is_true()
        {
            {
                // (cons (quote lambda) (cons (cdadr expr) (cddr expr)))
                Scm::func(imports::cons).invoke(&[Scm::symbol("lambda"), {
                    // (cons (cdadr expr) (cddr expr))
                    Scm::func(imports::cons).invoke(&[
                        {
                            // (cdadr expr)
                            Scm::func(imports::cdadr).invoke(&[expr.clone()])
                        },
                        {
                            // (cddr expr)
                            Scm::func(imports::cddr).invoke(&[expr.clone()])
                        },
                    ])
                }])
            }
        } else {
            {
                // (caddr expr)
                Scm::func(imports::caddr).invoke(&[expr.clone()])
            }
        }
    }
    .into()
}
pub fn definition_minus_variable(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        if ({
            // (pair? (cadr expr))
            Scm::func(imports::pair_p).invoke(&[{
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }])
        })
        .is_true()
        {
            {
                // (caadr expr)
                Scm::func(imports::caadr).invoke(&[expr.clone()])
            }
        } else {
            {
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }
        }
    }
    .into()
}
pub fn definition_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote define)))
            if ({
                // (pair? expr)
                Scm::func(imports::pair_p).invoke(&[expr.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote define))
                    Scm::func(imports::eq_p).invoke(&[
                        {
                            // (car expr)
                            Scm::func(imports::car).invoke(&[expr.clone()])
                        },
                        Scm::symbol("define"),
                    ])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn if_minus_alternative(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        if ({
            // (pair? (cdddr expr))
            Scm::func(imports::pair_p).invoke(&[{
                // (cdddr expr)
                Scm::func(imports::cdddr).invoke(&[expr.clone()])
            }])
        })
        .is_true()
        {
            {
                // (cadddr expr)
                Scm::func(imports::cadddr).invoke(&[expr.clone()])
            }
        } else {
            Scm::pair(
                Scm::symbol("quote"),
                Scm::pair(Scm::symbol("*UNSPECIFIED*"), Scm::Nil),
            )
        }
    }
    .into()
}
pub fn if_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn if_minus_consequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (caddr expr)
            Scm::func(imports::caddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn import_minus_libnames(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let exp_star_ = args[0].clone();
        {
            // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
            Scm::func(imports::filter).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let libname = args[0].clone();
                        {
                            // (not (equal? libname (quote (sunny testing))))
                            Scm::func(imports::not).invoke(&[{
                                // (equal? libname (quote (sunny testing)))
                                Scm::func(imports::equal_p).invoke(&[
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
                    Scm::func(imports::map).invoke(&[Scm::func(importset_minus_libname), {
                        // (cdr exp*)
                        Scm::func(imports::cdr).invoke(&[exp_star_.clone()])
                    }])
                },
            ])
        }
    }
    .into()
}
pub fn import_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote import)))
            if ({
                // (pair? expr)
                Scm::func(imports::pair_p).invoke(&[expr.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote import))
                    Scm::func(imports::eq_p).invoke(&[
                        {
                            // (car expr)
                            Scm::func(imports::car).invoke(&[expr.clone()])
                        },
                        Scm::symbol("import"),
                    ])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn importset_minus_except_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt = args[0].clone();
        {
            // (eq? (quote except) (car stmt))
            Scm::func(imports::eq_p).invoke(&[Scm::symbol("except"), {
                // (car stmt)
                Scm::func(imports::car).invoke(&[stmt.clone()])
            }])
        }
    }
    .into()
}
pub fn importset_minus_libname(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt = args[0].clone();
        {
            // (cond ...)
            if ({
                // (importset-only? stmt)
                Scm::func(importset_minus_only_p).invoke(&[stmt.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        Scm::func(imports::cadr).invoke(&[stmt.clone()])
                    }])
                }
            } else if ({
                // (importset-except? stmt)
                Scm::func(importset_minus_except_p).invoke(&[stmt.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        Scm::func(imports::cadr).invoke(&[stmt.clone()])
                    }])
                }
            } else {
                stmt.clone()
            }
        }
    }
    .into()
}
pub fn importset_minus_only_minus_names(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt = args[0].clone();
        {
            // (cddr stmt)
            Scm::func(imports::cddr).invoke(&[stmt.clone()])
        }
    }
    .into()
}
pub fn importset_minus_only_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt = args[0].clone();
        {
            // (eq? (quote only) (car stmt))
            Scm::func(imports::eq_p).invoke(&[Scm::symbol("only"), {
                // (car stmt)
                Scm::func(imports::car).invoke(&[stmt.clone()])
            }])
        }
    }
    .into()
}
pub fn lambda_minus_body(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cddr expr)
            Scm::func(imports::cddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn lambda_minus_params(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn let_star__minus_bindings(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn let_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (map cadr (cadr expr))
            Scm::func(imports::map).invoke(&[Scm::func(imports::cadr), {
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }])
        }
    }
    .into()
}
pub fn let_minus_body(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cddr expr)
            Scm::func(imports::cddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn let_minus_vars(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (map car (cadr expr))
            Scm::func(imports::map).invoke(&[Scm::func(imports::car), {
                // (cadr expr)
                Scm::func(imports::cadr).invoke(&[expr.clone()])
            }])
        }
    }
    .into()
}
pub fn library_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let exp_star_ = args[0].clone();
        {
            // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
            if ({
                // (pair? exp*)
                Scm::func(imports::pair_p).invoke(&[exp_star_.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote define-library) (car exp*))
                    Scm::func(imports::eq_p).invoke(&[Scm::symbol("define-library"), {
                        // (car exp*)
                        Scm::func(imports::car).invoke(&[exp_star_.clone()])
                    }])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn scan_minus_out_minus_defines(args: &[Scm]) -> Scm {
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
                                initializations.set({
                                    // Closure
                                    let initializations = initializations.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let exp_star_ = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (null? exp*)
                                                Scm::func(imports::null_p)
                                                    .invoke(&[exp_star_.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                Scm::func(definition_p).invoke(&[{
                                                    // (car exp*)
                                                    Scm::func(imports::car)
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
                                                    Scm::func(imports::cons).invoke(&[
                                                        {
                                                            // (list (definition-variable (car exp*)) (definition-value (car exp*)))
                                                            Scm::func(imports::list).invoke(&[
                                                                {
                                                                    // (definition-variable (car exp*))
                                                                    Scm::func(
                                                                        definition_minus_variable,
                                                                    )
                                                                    .invoke(&[{
                                                                        // (car exp*)
                                                                        Scm::func(imports::car)
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ])
                                                                    }])
                                                                },
                                                                {
                                                                    // (definition-value (car exp*))
                                                                    Scm::func(
                                                                        definition_minus_value,
                                                                    )
                                                                    .invoke(&[{
                                                                        // (car exp*)
                                                                        Scm::func(imports::car)
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ])
                                                                    }])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (initializations (cdr exp*))
                                                            initializations.get().invoke(&[{
                                                                // (cdr exp*)
                                                                Scm::func(imports::cdr)
                                                                    .invoke(&[exp_star_.clone()])
                                                            }])
                                                        },
                                                    ])
                                                }
                                            } else {
                                                {
                                                    // (initializations (cdr exp*))
                                                    initializations.get().invoke(&[{
                                                        // (cdr exp*)
                                                        Scm::func(imports::cdr)
                                                            .invoke(&[exp_star_.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
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
                                                Scm::func(imports::null_p)
                                                    .invoke(&[exp_star_.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                Scm::func(definition_p).invoke(&[{
                                                    // (car exp*)
                                                    Scm::func(imports::car)
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (transform (cdr exp*))
                                                    transform.get().invoke(&[{
                                                        // (cdr exp*)
                                                        Scm::func(imports::cdr)
                                                            .invoke(&[exp_star_.clone()])
                                                    }])
                                                }
                                            } else {
                                                {
                                                    // (cons (car exp*) (transform (cdr exp*)))
                                                    Scm::func(imports::cons).invoke(&[
                                                        {
                                                            // (car exp*)
                                                            Scm::func(imports::car)
                                                                .invoke(&[exp_star_.clone()])
                                                        },
                                                        {
                                                            // (transform (cdr exp*))
                                                            transform.get().invoke(&[{
                                                                // (cdr exp*)
                                                                Scm::func(imports::cdr)
                                                                    .invoke(&[exp_star_.clone()])
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
                                            Scm::func(imports::null_p).invoke(&[ini.clone()])
                                        })
                                        .is_true()
                                        {
                                            body.clone()
                                        } else {
                                            {
                                                // (list (cons (quote letrec) (cons ini (transform body))))
                                                Scm::func(imports::list).invoke(&[{
                                                    // (cons (quote letrec) (cons ini (transform body)))
                                                    Scm::func(imports::cons).invoke(&[
                                                        Scm::symbol("letrec"),
                                                        {
                                                            // (cons ini (transform body))
                                                            Scm::func(imports::cons).invoke(&[
                                                                ini.clone(),
                                                                {
                                                                    // (transform body)
                                                                    transform
                                                                        .get()
                                                                        .invoke(&[body.clone()])
                                                                },
                                                            ])
                                                        },
                                                    ])
                                                }])
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
    .into()
}
pub fn set_i_minus_value(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (caddr expr)
            Scm::func(imports::caddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn set_i_minus_variable(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_body(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cddr expr)
            Scm::func(imports::cddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_description(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_cases(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cddr expr)
            Scm::func(imports::cddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
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
