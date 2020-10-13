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
        let expr__2 = args[0].clone();
        {
            // (and (pair? expr) (eq? (quote lambda) (car expr)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__2.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote lambda) (car expr))
                    imports::eq_p(&[Scm::symbol("lambda"), {
                        // (car expr)
                        imports::car(&[expr__2.clone()])
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
        let expr__3 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__3.clone()])
        }
    }
    .into()
}
pub fn assert_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__4 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__4.clone()])
        }
    }
    .into()
}
pub fn begin_minus_statements(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__5 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__5.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__0 = args[0].clone();
        {
            // (car clause)
            imports::car(&[clause__0.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_sequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__1 = args[0].clone();
        {
            // (cdr clause)
            imports::cdr(&[clause__1.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clauses(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__6 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__6.clone()])
        }
    }
    .into()
}
pub fn cond_minus_else_minus_clause_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__2 = args[0].clone();
        {
            // (eq? (quote else) (car clause))
            imports::eq_p(&[Scm::symbol("else"), {
                // (car clause)
                imports::car(&[clause__2.clone()])
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
        let expr__8 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__8.clone()])
            }])
        })
        .is_true()
        {
            {
                // (list (cadr expr) (quote ...))
                imports::list(&[
                    {
                        // (cadr expr)
                        imports::cadr(&[expr__8.clone()])
                    },
                    Scm::symbol("..."),
                ])
            }
        } else {
            {
                // (cdr expr)
                imports::cdr(&[expr__8.clone()])
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
        let expr__10 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__10.clone()])
            }])
        })
        .is_true()
        {
            {
                // (cons (quote lambda) (cons (cdadr expr) (cddr expr)))
                imports::cons(&[Scm::symbol("lambda"), {
                    // (cons (cdadr expr) (cddr expr))
                    imports::cons(&[
                        {
                            // (cdadr expr)
                            imports::cdadr(&[expr__10.clone()])
                        },
                        {
                            // (cddr expr)
                            imports::cddr(&[expr__10.clone()])
                        },
                    ])
                }])
            }
        } else {
            {
                // (caddr expr)
                imports::caddr(&[expr__10.clone()])
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
        let expr__9 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__9.clone()])
            }])
        })
        .is_true()
        {
            {
                // (caadr expr)
                imports::caadr(&[expr__9.clone()])
            }
        } else {
            {
                // (cadr expr)
                imports::cadr(&[expr__9.clone()])
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
        let expr__7 = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote define)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__7.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote define))
                    imports::eq_p(&[
                        {
                            // (car expr)
                            imports::car(&[expr__7.clone()])
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
        let expr__13 = args[0].clone();
        if ({
            // (pair? (cdddr expr))
            imports::pair_p(&[{
                // (cdddr expr)
                imports::cdddr(&[expr__13.clone()])
            }])
        })
        .is_true()
        {
            {
                // (cadddr expr)
                imports::cadddr(&[expr__13.clone()])
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
        let expr__11 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__11.clone()])
        }
    }
    .into()
}
pub fn if_minus_consequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__12 = args[0].clone();
        {
            // (caddr expr)
            imports::caddr(&[expr__12.clone()])
        }
    }
    .into()
}
pub fn import_minus_libnames(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let exp_star___0 = args[0].clone();
        {
            // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
            imports::filter(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let libname__1 = args[0].clone();
                        {
                            // (not (equal? libname (quote (sunny testing))))
                            imports::not(&[{
                                // (equal? libname (quote (sunny testing)))
                                imports::equal_p(&[
                                    libname__1.clone(),
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
                    imports::map(&[Scm::func(importset_minus_libname), {
                        // (cdr exp*)
                        imports::cdr(&[exp_star___0.clone()])
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
        let expr__14 = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote import)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__14.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote import))
                    imports::eq_p(&[
                        {
                            // (car expr)
                            imports::car(&[expr__14.clone()])
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
        let stmt__1 = args[0].clone();
        {
            // (eq? (quote except) (car stmt))
            imports::eq_p(&[Scm::symbol("except"), {
                // (car stmt)
                imports::car(&[stmt__1.clone()])
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
        let stmt__0 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (importset-only? stmt)
                Scm::func(importset_minus_only_p).invoke(&[stmt__0.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        imports::cadr(&[stmt__0.clone()])
                    }])
                }
            } else if ({
                // (importset-except? stmt)
                Scm::func(importset_minus_except_p).invoke(&[stmt__0.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        imports::cadr(&[stmt__0.clone()])
                    }])
                }
            } else {
                stmt__0.clone()
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
        let stmt__3 = args[0].clone();
        {
            // (cddr stmt)
            imports::cddr(&[stmt__3.clone()])
        }
    }
    .into()
}
pub fn importset_minus_only_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt__2 = args[0].clone();
        {
            // (eq? (quote only) (car stmt))
            imports::eq_p(&[Scm::symbol("only"), {
                // (car stmt)
                imports::car(&[stmt__2.clone()])
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
        let expr__15 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__15.clone()])
        }
    }
    .into()
}
pub fn lambda_minus_params(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__16 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__16.clone()])
        }
    }
    .into()
}
pub fn let_star__minus_bindings(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__20 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__20.clone()])
        }
    }
    .into()
}
pub fn let_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__17 = args[0].clone();
        {
            // (map cadr (cadr expr))
            imports::map(&[Scm::func(imports::cadr), {
                // (cadr expr)
                imports::cadr(&[expr__17.clone()])
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
        let expr__18 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__18.clone()])
        }
    }
    .into()
}
pub fn let_minus_vars(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__19 = args[0].clone();
        {
            // (map car (cadr expr))
            imports::map(&[Scm::func(imports::car), {
                // (cadr expr)
                imports::cadr(&[expr__19.clone()])
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
        let exp_star___1 = args[0].clone();
        {
            // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
            if ({
                // (pair? exp*)
                imports::pair_p(&[exp_star___1.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote define-library) (car exp*))
                    imports::eq_p(&[Scm::symbol("define-library"), {
                        // (car exp*)
                        imports::car(&[exp_star___1.clone()])
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
        let body__14 = args[0].clone();
        {
            // (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body)))))))
            {
                // (let ((initializations (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (set! transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))))
                {
                    let [initializations__0, transform__29] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let transform__29 = transform__29.into_boxed();
                        {
                            let initializations__0 = initializations__0.into_boxed();
                            {
                                initializations__0.set({
                                    // Closure
                                    let initializations__0 = initializations__0.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let exp_star___2 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (null? exp*)
                                                imports::null_p(&[exp_star___2.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                definition_p(&[{
                                                    // (car exp*)
                                                    imports::car(&[exp_star___2.clone()])
                                                }])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
                                                    imports::cons(&[
                                                        {
                                                            // (list (definition-variable (car exp*)) (definition-value (car exp*)))
                                                            imports::list(&[
                                                                {
                                                                    // (definition-variable (car exp*))
                                                                    definition_minus_variable(&[{
                                                                        // (car exp*)
                                                                        imports::car(&[
                                                                            exp_star___2.clone(),
                                                                        ])
                                                                    }])
                                                                },
                                                                {
                                                                    // (definition-value (car exp*))
                                                                    definition_minus_value(&[{
                                                                        // (car exp*)
                                                                        imports::car(&[
                                                                            exp_star___2.clone(),
                                                                        ])
                                                                    }])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (initializations (cdr exp*))
                                                            initializations__0.get().invoke(&[{
                                                                // (cdr exp*)
                                                                imports::cdr(
                                                                    &[exp_star___2.clone()],
                                                                )
                                                            }])
                                                        },
                                                    ])
                                                }
                                            } else {
                                                {
                                                    // (initializations (cdr exp*))
                                                    initializations__0.get().invoke(&[{
                                                        // (cdr exp*)
                                                        imports::cdr(&[exp_star___2.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                transform__29.set({
                                    // Closure
                                    let transform__29 = transform__29.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let exp_star___3 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (null? exp*)
                                                imports::null_p(&[exp_star___3.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                definition_p(&[{
                                                    // (car exp*)
                                                    imports::car(&[exp_star___3.clone()])
                                                }])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (transform (cdr exp*))
                                                    transform__29.get().invoke(&[{
                                                        // (cdr exp*)
                                                        imports::cdr(&[exp_star___3.clone()])
                                                    }])
                                                }
                                            } else {
                                                {
                                                    // (cons (car exp*) (transform (cdr exp*)))
                                                    imports::cons(&[
                                                        {
                                                            // (car exp*)
                                                            imports::car(&[exp_star___3.clone()])
                                                        },
                                                        {
                                                            // (transform (cdr exp*))
                                                            transform__29.get().invoke(&[{
                                                                // (cdr exp*)
                                                                imports::cdr(
                                                                    &[exp_star___3.clone()],
                                                                )
                                                            }])
                                                        },
                                                    ])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                {
                                    // (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))
                                    {
                                        let ini__0 = {
                                            // (initializations body)
                                            initializations__0.get().invoke(&[body__14.clone()])
                                        };
                                        if ({
                                            // (null? ini)
                                            imports::null_p(&[ini__0.clone()])
                                        })
                                        .is_true()
                                        {
                                            body__14.clone()
                                        } else {
                                            {
                                                // (list (cons (quote letrec) (cons ini (transform body))))
                                                imports::list(&[{
                                                    // (cons (quote letrec) (cons ini (transform body)))
                                                    imports::cons(&[Scm::symbol("letrec"), {
                                                        // (cons ini (transform body))
                                                        imports::cons(&[ini__0.clone(), {
                                                            // (transform body)
                                                            transform__29
                                                                .get()
                                                                .invoke(&[body__14.clone()])
                                                        }])
                                                    }])
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
        let expr__22 = args[0].clone();
        {
            // (caddr expr)
            imports::caddr(&[expr__22.clone()])
        }
    }
    .into()
}
pub fn set_i_minus_variable(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__21 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__21.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_body(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__23 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__23.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_description(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__24 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__24.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_cases(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__25 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__25.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__26 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__26.clone()])
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
