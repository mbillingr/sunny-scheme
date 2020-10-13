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
        let expr__734 = args[0].clone();
        {
            // (and (pair? expr) (eq? (quote lambda) (car expr)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__734.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote lambda) (car expr))
                    imports::eq_p(&[Scm::symbol("lambda"), {
                        // (car expr)
                        imports::car(&[expr__734.clone()])
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
        let expr__735 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__735.clone()])
        }
    }
    .into()
}
pub fn assert_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__736 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__736.clone()])
        }
    }
    .into()
}
pub fn begin_minus_statements(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__737 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__737.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_condition(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__739 = args[0].clone();
        {
            // (car clause)
            imports::car(&[clause__739.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clause_minus_sequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__740 = args[0].clone();
        {
            // (cdr clause)
            imports::cdr(&[clause__740.clone()])
        }
    }
    .into()
}
pub fn cond_minus_clauses(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__738 = args[0].clone();
        {
            // (cdr expr)
            imports::cdr(&[expr__738.clone()])
        }
    }
    .into()
}
pub fn cond_minus_else_minus_clause_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let clause__741 = args[0].clone();
        {
            // (eq? (quote else) (car clause))
            imports::eq_p(&[Scm::symbol("else"), {
                // (car clause)
                imports::car(&[clause__741.clone()])
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
        let expr__743 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__743.clone()])
            }])
        })
        .is_true()
        {
            {
                // (list (cadr expr) (quote ...))
                imports::list(&[
                    {
                        // (cadr expr)
                        imports::cadr(&[expr__743.clone()])
                    },
                    Scm::symbol("..."),
                ])
            }
        } else {
            {
                // (cdr expr)
                imports::cdr(&[expr__743.clone()])
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
        let expr__745 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__745.clone()])
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
                            imports::cdadr(&[expr__745.clone()])
                        },
                        {
                            // (cddr expr)
                            imports::cddr(&[expr__745.clone()])
                        },
                    ])
                }])
            }
        } else {
            {
                // (caddr expr)
                imports::caddr(&[expr__745.clone()])
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
        let expr__744 = args[0].clone();
        if ({
            // (pair? (cadr expr))
            imports::pair_p(&[{
                // (cadr expr)
                imports::cadr(&[expr__744.clone()])
            }])
        })
        .is_true()
        {
            {
                // (caadr expr)
                imports::caadr(&[expr__744.clone()])
            }
        } else {
            {
                // (cadr expr)
                imports::cadr(&[expr__744.clone()])
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
        let expr__742 = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote define)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__742.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote define))
                    imports::eq_p(&[
                        {
                            // (car expr)
                            imports::car(&[expr__742.clone()])
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
        let expr__748 = args[0].clone();
        if ({
            // (pair? (cdddr expr))
            imports::pair_p(&[{
                // (cdddr expr)
                imports::cdddr(&[expr__748.clone()])
            }])
        })
        .is_true()
        {
            {
                // (cadddr expr)
                imports::cadddr(&[expr__748.clone()])
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
        let expr__746 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__746.clone()])
        }
    }
    .into()
}
pub fn if_minus_consequence(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__747 = args[0].clone();
        {
            // (caddr expr)
            imports::caddr(&[expr__747.clone()])
        }
    }
    .into()
}
pub fn import_minus_libnames(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let exp_star___751 = args[0].clone();
        {
            // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
            imports::filter(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let libname__750 = args[0].clone();
                        {
                            // (not (equal? libname (quote (sunny testing))))
                            imports::not(&[{
                                // (equal? libname (quote (sunny testing)))
                                imports::equal_p(&[
                                    libname__750.clone(),
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
                        imports::cdr(&[exp_star___751.clone()])
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
        let expr__749 = args[0].clone();
        {
            // (and (pair? expr) (eq? (car expr) (quote import)))
            if ({
                // (pair? expr)
                imports::pair_p(&[expr__749.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (car expr) (quote import))
                    imports::eq_p(&[
                        {
                            // (car expr)
                            imports::car(&[expr__749.clone()])
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
        let stmt__753 = args[0].clone();
        {
            // (eq? (quote except) (car stmt))
            imports::eq_p(&[Scm::symbol("except"), {
                // (car stmt)
                imports::car(&[stmt__753.clone()])
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
        let stmt__752 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (importset-only? stmt)
                Scm::func(importset_minus_only_p).invoke(&[stmt__752.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        imports::cadr(&[stmt__752.clone()])
                    }])
                }
            } else if ({
                // (importset-except? stmt)
                Scm::func(importset_minus_except_p).invoke(&[stmt__752.clone()])
            })
            .is_true()
            {
                {
                    // (importset-libname (cadr stmt))
                    Scm::func(importset_minus_libname).invoke(&[{
                        // (cadr stmt)
                        imports::cadr(&[stmt__752.clone()])
                    }])
                }
            } else {
                stmt__752.clone()
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
        let stmt__755 = args[0].clone();
        {
            // (cddr stmt)
            imports::cddr(&[stmt__755.clone()])
        }
    }
    .into()
}
pub fn importset_minus_only_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let stmt__754 = args[0].clone();
        {
            // (eq? (quote only) (car stmt))
            imports::eq_p(&[Scm::symbol("only"), {
                // (car stmt)
                imports::car(&[stmt__754.clone()])
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
        let expr__756 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__756.clone()])
        }
    }
    .into()
}
pub fn lambda_minus_params(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__757 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__757.clone()])
        }
    }
    .into()
}
pub fn let_star__minus_bindings(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__761 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__761.clone()])
        }
    }
    .into()
}
pub fn let_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__758 = args[0].clone();
        {
            // (map cadr (cadr expr))
            imports::map(&[Scm::func(imports::cadr), {
                // (cadr expr)
                imports::cadr(&[expr__758.clone()])
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
        let expr__759 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__759.clone()])
        }
    }
    .into()
}
pub fn let_minus_vars(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__760 = args[0].clone();
        {
            // (map car (cadr expr))
            imports::map(&[Scm::func(imports::car), {
                // (cadr expr)
                imports::cadr(&[expr__760.clone()])
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
        let exp_star___762 = args[0].clone();
        {
            // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
            if ({
                // (pair? exp*)
                imports::pair_p(&[exp_star___762.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote define-library) (car exp*))
                    imports::eq_p(&[Scm::symbol("define-library"), {
                        // (car exp*)
                        imports::car(&[exp_star___762.clone()])
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
        let body__767 = args[0].clone();
        {
            // (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body)))))))
            {
                // (let ((initializations (quote *uninitialized*)) (transform (quote *uninitialized*))) (begin (set! initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (set! transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))) (let ((ini (initializations body))) (if (null? ini) body (list (cons (quote letrec) (cons ini (transform body))))))))
                {
                    let [initializations__763, transform__765] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let transform__765 = transform__765.into_boxed();
                        {
                            let initializations__763 = initializations__763.into_boxed();
                            {
                                initializations__763.set({
                                    // Closure
                                    let initializations__763 = initializations__763.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let exp_star___764 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (null? exp*)
                                                imports::null_p(&[exp_star___764.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                definition_p(&[{
                                                    // (car exp*)
                                                    imports::car(&[exp_star___764.clone()])
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
                                                                            exp_star___764.clone(),
                                                                        ])
                                                                    }])
                                                                },
                                                                {
                                                                    // (definition-value (car exp*))
                                                                    definition_minus_value(&[{
                                                                        // (car exp*)
                                                                        imports::car(&[
                                                                            exp_star___764.clone(),
                                                                        ])
                                                                    }])
                                                                },
                                                            ])
                                                        },
                                                        {
                                                            // (initializations (cdr exp*))
                                                            initializations__763.get().invoke(&[{
                                                                // (cdr exp*)
                                                                imports::cdr(&[
                                                                    exp_star___764.clone()
                                                                ])
                                                            }])
                                                        },
                                                    ])
                                                }
                                            } else {
                                                {
                                                    // (initializations (cdr exp*))
                                                    initializations__763.get().invoke(&[{
                                                        // (cdr exp*)
                                                        imports::cdr(&[exp_star___764.clone()])
                                                    }])
                                                }
                                            }
                                        }
                                    })
                                });
                                Scm::anything();
                                transform__765.set({
                                    // Closure
                                    let transform__765 = transform__765.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let exp_star___766 = args[0].clone();
                                        {
                                            // (cond ...)
                                            if ({
                                                // (null? exp*)
                                                imports::null_p(&[exp_star___766.clone()])
                                            })
                                            .is_true()
                                            {
                                                Scm::Nil
                                            } else if ({
                                                // (definition? (car exp*))
                                                definition_p(&[{
                                                    // (car exp*)
                                                    imports::car(&[exp_star___766.clone()])
                                                }])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (transform (cdr exp*))
                                                    transform__765.get().invoke(&[{
                                                        // (cdr exp*)
                                                        imports::cdr(&[exp_star___766.clone()])
                                                    }])
                                                }
                                            } else {
                                                {
                                                    // (cons (car exp*) (transform (cdr exp*)))
                                                    imports::cons(&[
                                                        {
                                                            // (car exp*)
                                                            imports::car(&[exp_star___766.clone()])
                                                        },
                                                        {
                                                            // (transform (cdr exp*))
                                                            transform__765.get().invoke(&[{
                                                                // (cdr exp*)
                                                                imports::cdr(&[
                                                                    exp_star___766.clone()
                                                                ])
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
                                        let ini__768 = {
                                            // (initializations body)
                                            initializations__763.get().invoke(&[body__767.clone()])
                                        };
                                        if ({
                                            // (null? ini)
                                            imports::null_p(&[ini__768.clone()])
                                        })
                                        .is_true()
                                        {
                                            body__767.clone()
                                        } else {
                                            {
                                                // (list (cons (quote letrec) (cons ini (transform body))))
                                                imports::list(&[{
                                                    // (cons (quote letrec) (cons ini (transform body)))
                                                    imports::cons(&[Scm::symbol("letrec"), {
                                                        // (cons ini (transform body))
                                                        imports::cons(&[ini__768.clone(), {
                                                            // (transform body)
                                                            transform__765
                                                                .get()
                                                                .invoke(&[body__767.clone()])
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
        let expr__770 = args[0].clone();
        {
            // (caddr expr)
            imports::caddr(&[expr__770.clone()])
        }
    }
    .into()
}
pub fn set_i_minus_variable(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__769 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__769.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_body(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__771 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__771.clone()])
        }
    }
    .into()
}
pub fn testcase_minus_description(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__772 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__772.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_cases(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__773 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr__773.clone()])
        }
    }
    .into()
}
pub fn testsuite_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr__774 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr__774.clone()])
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
