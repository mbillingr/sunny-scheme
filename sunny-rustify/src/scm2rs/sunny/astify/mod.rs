#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
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
    pub use super::globals::astify_minus_export;
    pub use super::globals::astify_minus_import;
    pub use super::globals::astify_minus_sequence;
    pub use super::globals::astify_minus_symbol;
    pub use super::globals::astify_minus_testsuite;
}

mod globals {
    use sunny_core::{Mut, Scm};
    pub fn astify(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let exp = args[0].clone();
            let env = args[1].clone();
            let tail_p = args[2].clone();
            {
                // (cond ...)
                if ({
                    // (pair? exp)
                    imports::pair_p
                        .with(|value| value.get())
                        .invoke(&[exp.clone()])
                })
                .is_true()
                {
                    {
                        // (let ((f-obj (astify (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (astify-comment exp (astify-application f-obj (cdr exp) env tail?))))
                        {
                            let f_minus_obj = {
                                // (astify (car exp) env #f)
                                globals::astify.with(|value| value.get()).invoke(&[
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
                                    // (astify-comment exp (astify-application f-obj (cdr exp) env tail?))
                                    globals::astify_minus_comment
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone(), {
                                            // (astify-application f-obj (cdr exp) env tail?)
                                            globals::astify_minus_application
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    f_minus_obj.clone(),
                                                    {
                                                        // (cdr exp)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[exp.clone()])
                                                    },
                                                    env.clone(),
                                                    tail_p.clone(),
                                                ])
                                        }])
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
                        // (astify-symbol exp env)
                        globals::astify_minus_symbol
                            .with(|value| value.get())
                            .invoke(&[exp.clone(), env.clone()])
                    }
                } else {
                    {
                        // (astify-constant exp env)
                        globals::astify_minus_constant
                            .with(|value| value.get())
                            .invoke(&[exp.clone(), env.clone()])
                    }
                }
            }
        }
    }
    pub fn astify_minus_abstraction(args: &[Scm]) {
        {
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
                                            imports::lookup_star_.with(|value| value.get()).invoke(
                                                &[param_star_.clone(), local_minus_env.clone()],
                                            )
                                        },
                                        body_minus_ast.clone(),
                                    ])
                            }
                        }
                    }
                }
            }
        }
    }
    pub fn astify_minus_alternative(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_and(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_application(args: &[Scm]) {
        {
            if args.len() != 4 {
                panic!("invalid arity")
            }
            let proc = args[0].clone();
            let arg_star_ = args[1].clone();
            let env = args[2].clone();
            let tail_p = args[3].clone();
            if ({
                // (eq? (quote ABSTRACTION) (proc (quote kind)))
                imports::eq_p
                    .with(|value| value.get())
                    .invoke(&[Scm::symbol("ABSTRACTION"), {
                        // (proc (quote kind))
                        proc.clone().invoke(&[Scm::symbol("kind")])
                    }])
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
        }
    }
    pub fn astify_minus_args(args: &[Scm]) {
        {
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
                            globals::astify_minus_args
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
        }
    }
    pub fn astify_minus_assert(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_assignment(args: &[Scm]) {
        {
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
                                .invoke(&[var_minus_name.clone(), var.clone(), val.clone()])
                        }
                    }
                }
            }
        }
    }
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_comment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE astify-comment"))}
    pub fn astify_minus_cond(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let clause_star_ = args[0].clone();
            let env = args[1].clone();
            let tail_p = args[2].clone();
            {
                // (cond ...)
                if ({
                    // (null? clause*)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[clause_star_.clone()])
                })
                .is_true()
                {
                    {
                        // (astify-unspecified)
                        globals::astify_minus_unspecified
                            .with(|value| value.get())
                            .invoke(&[])
                    }
                } else if ({
                    // (cond-else-clause? (car clause*))
                    imports::cond_minus_else_minus_clause_p
                        .with(|value| value.get())
                        .invoke(&[{
                            // (car clause*)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[clause_star_.clone()])
                        }])
                })
                .is_true()
                {
                    {
                        // (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
                        globals::astify_minus_sequence
                            .with(|value| value.get())
                            .invoke(&[
                                {
                                    // (cond-clause-sequence (car clause*))
                                    imports::cond_minus_clause_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[{
                                            // (car clause*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[clause_star_.clone()])
                                        }])
                                },
                                env.clone(),
                                tail_p.clone(),
                            ])
                    }
                } else {
                    {
                        // (let* ((i (astify (cond-clause-condition (car clause*)) env #f)) (t (astify-sequence (cond-clause-sequence (car clause*)) env tail?)) (e (astify-cond (cdr clause*) env tail?))) (make-alternative i t e))
                        {
                            // (let ((i (astify (cond-clause-condition (car clause*)) env #f))) (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))))
                            {
                                let i = {
                                    // (astify (cond-clause-condition (car clause*)) env #f)
                                    globals::astify.with(|value| value.get()).invoke(&[
                                        {
                                            // (cond-clause-condition (car clause*))
                                            imports::cond_minus_clause_minus_condition
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (car clause*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[clause_star_.clone()])
                                                }])
                                        },
                                        env.clone(),
                                        Scm::False,
                                    ])
                                };
                                // (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e))))
                                let t = {
                                    // (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
                                    globals::astify_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (cond-clause-sequence (car clause*))
                                                imports::cond_minus_clause_minus_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[{
                                                        // (car clause*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[clause_star_.clone()])
                                                    }])
                                            },
                                            env.clone(),
                                            tail_p.clone(),
                                        ])
                                };
                                // (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))
                                let e = {
                                    // (astify-cond (cdr clause*) env tail?)
                                    globals::astify_minus_cond
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (cdr clause*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[clause_star_.clone()])
                                            },
                                            env.clone(),
                                            tail_p.clone(),
                                        ])
                                };
                                {
                                    // (make-alternative i t e)
                                    imports::make_minus_alternative
                                        .with(|value| value.get())
                                        .invoke(&[i.clone(), t.clone(), e.clone()])
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    pub fn astify_minus_constant(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_definition(args: &[Scm]) {
        {
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
                                .invoke(&[var_minus_name.clone(), var.clone(), val.clone()])
                        }
                    }
                }
            }
        }
    }
    pub fn astify_minus_export(args: &[Scm]) {
        {
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
                        // (cons (make-export env (car export-spec*) (car export-spec*)) (astify-export (cdr export-spec*) env))
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
                                                .invoke(&[export_minus_spec_star_.clone()])
                                        },
                                        {
                                            // (car export-spec*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[export_minus_spec_star_.clone()])
                                        },
                                    ])
                            },
                            {
                                // (astify-export (cdr export-spec*) env)
                                globals::astify_minus_export
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (cdr export-spec*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[export_minus_spec_star_.clone()])
                                        },
                                        env.clone(),
                                    ])
                            },
                        ])
                    }
                }
            }
        }
    }
    pub fn astify_minus_import(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let stmt_star_ = args[0].clone();
            let env = args[1].clone();
            if ({
                // (null? stmt*)
                imports::null_p
                    .with(|value| value.get())
                    .invoke(&[stmt_star_.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else {
                {
                    // (let ((libname (importset-libname (car stmt*)))) (cond ((equal? (quote (sunny testing)) libname) (astify-import (cdr stmt*) env)) ((importset-only? (car stmt*)) (cons (astify-import-only libname (importset-only-names (car stmt*)) env) (astify-import (cdr stmt*) env))) (else (cons (astify-import-all libname env) (astify-import (cdr stmt*) env)))))
                    {
                        let libname = {
                            // (importset-libname (car stmt*))
                            imports::importset_minus_libname
                                .with(|value| value.get())
                                .invoke(&[{
                                    // (car stmt*)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[stmt_star_.clone()])
                                }])
                        };
                        {
                            // (cond ...)
                            if ({
                                // (equal? (quote (sunny testing)) libname)
                                imports::equal_p.with(|value| value.get()).invoke(&[
                                    Scm::pair(
                                        Scm::symbol("sunny"),
                                        Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                    ),
                                    libname.clone(),
                                ])
                            })
                            .is_true()
                            {
                                {
                                    // (astify-import (cdr stmt*) env)
                                    globals::astify_minus_import
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
                                // (importset-only? (car stmt*))
                                imports::importset_minus_only_p
                                    .with(|value| value.get())
                                    .invoke(&[{
                                        // (car stmt*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()])
                                    }])
                            })
                            .is_true()
                            {
                                {
                                    // (cons (astify-import-only libname (importset-only-names (car stmt*)) env) (astify-import (cdr stmt*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (astify-import-only libname (importset-only-names (car stmt*)) env)
                                            globals::astify_minus_import_minus_only
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    libname.clone(),
                                                    {
                                                        // (importset-only-names (car stmt*))
                                                        imports::importset_minus_only_minus_names
                                                            .with(|value| value.get())
                                                            .invoke(&[{
                                                                // (car stmt*)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[stmt_star_.clone()])
                                                            }])
                                                    },
                                                    env.clone(),
                                                ])
                                        },
                                        {
                                            // (astify-import (cdr stmt*) env)
                                            globals::astify_minus_import
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
                                    // (cons (astify-import-all libname env) (astify-import (cdr stmt*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (astify-import-all libname env)
                                            globals::astify_minus_import_minus_all
                                                .with(|value| value.get())
                                                .invoke(&[libname.clone(), env.clone()])
                                        },
                                        {
                                            // (astify-import (cdr stmt*) env)
                                            globals::astify_minus_import
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
                    }
                }
            }
        }
    }
    pub fn astify_minus_import_minus_all(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let libname = args[0].clone();
            let env = args[1].clone();
            {
                {
                    // (adjoin-import*! (library-exports (library-decls (get-lib libname))) env)
                    imports::adjoin_minus_import_star__i
                        .with(|value| value.get())
                        .invoke(&[
                            {
                                // (library-exports (library-decls (get-lib libname)))
                                imports::library_minus_exports
                                    .with(|value| value.get())
                                    .invoke(&[{
                                        // (library-decls (get-lib libname))
                                        imports::library_minus_decls
                                            .with(|value| value.get())
                                            .invoke(&[{
                                                // (get-lib libname)
                                                imports::get_minus_lib
                                                    .with(|value| value.get())
                                                    .invoke(&[libname.clone()])
                                            }])
                                    }])
                            },
                            env.clone(),
                        ])
                };
                {
                    // (make-import libname)
                    imports::make_minus_import
                        .with(|value| value.get())
                        .invoke(&[libname.clone()])
                }
            }
        }
    }
    pub fn astify_minus_import_minus_only(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let libname = args[0].clone();
            let names = args[1].clone();
            let env = args[2].clone();
            {
                {
                    // (check-imports names (library-exports (library-decls (get-lib libname))) libname)
                    imports::check_minus_imports
                        .with(|value| value.get())
                        .invoke(&[
                            names.clone(),
                            {
                                // (library-exports (library-decls (get-lib libname)))
                                imports::library_minus_exports
                                    .with(|value| value.get())
                                    .invoke(&[{
                                        // (library-decls (get-lib libname))
                                        imports::library_minus_decls
                                            .with(|value| value.get())
                                            .invoke(&[{
                                                // (get-lib libname)
                                                imports::get_minus_lib
                                                    .with(|value| value.get())
                                                    .invoke(&[libname.clone()])
                                            }])
                                    }])
                            },
                            libname.clone(),
                        ])
                };
                {
                    // (adjoin-import*! names env)
                    imports::adjoin_minus_import_star__i
                        .with(|value| value.get())
                        .invoke(&[names.clone(), env.clone()])
                };
                {
                    // (make-import-only libname names)
                    imports::make_minus_import_minus_only
                        .with(|value| value.get())
                        .invoke(&[libname.clone(), names.clone()])
                }
            }
        }
    }
    pub fn astify_minus_sequence(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_symbol(args: &[Scm]) {
        {
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
        }
    }
    pub fn astify_minus_testcase(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let case = args[0].clone();
            let env = args[1].clone();
            {
                // (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f))))
                {
                    // (let ((given (quote *uninitialized*)) (when (quote *uninitialized*)) (then (quote *uninitialized*)) (dispatch (quote *uninitialized*))) (begin (set! given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (set! when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (set! then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (set! dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))))
                    {
                        let [given, when, then, dispatch] = [
                            Scm::symbol("*uninitialized*"),
                            Scm::symbol("*uninitialized*"),
                            Scm::symbol("*uninitialized*"),
                            Scm::symbol("*uninitialized*"),
                        ];
                        {
                            let dispatch = dispatch.into_boxed();
                            {
                                let then = then.into_boxed();
                                {
                                    let when = when.into_boxed();
                                    {
                                        let given = given.into_boxed();
                                        {
                                            given.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("let*"),{
// (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
imports::map.with(|value| value.get()).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let assignment = args[0].clone();{
// (list (car assignment) (caddr assignment))
imports::list.with(|value| value.get()).invoke(&[{
// (car assignment)
imports::car.with(|value| value.get()).invoke(&[assignment.clone()])},{
// (caddr assignment)
imports::caddr.with(|value| value.get()).invoke(&[assignment.clone()])}])}})},{
// (cdr stmt)
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])},body.clone()])}})});
                                            when.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
{
// (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))) (loop (cdr stmt))))
{let loop_ = Scm::symbol("*uninitialized*");{let loop_ = loop_.into_boxed();{loop_.set({// Closure
let body = body.clone();let loop_ = loop_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let stmt_star_ = args[0].clone();{
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
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])}}}}}}})});
                                            then.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("begin"),{
// (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
imports::append.with(|value| value.get()).invoke(&[{
// (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
imports::map.with(|value| value.get()).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let pred = args[0].clone();{
// (list (quote assert) pred)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("assert"),pred.clone()])}})},{
// (cdr stmt)
imports::cdr.with(|value| value.get()).invoke(&[stmt.clone()])}])},body.clone()])}])}})});
                                            dispatch.set({
                                                // Closure
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
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (null? section*)
                                                            imports::null_p
                                                                .with(|value| value.get())
                                                                .invoke(&[section_star_.clone()])
                                                        })
                                                        .is_true()
                                                        {
                                                            body.clone()
                                                        } else if ({
                                                            // (eq? (quote given) (caar section*))
                                                            imports::eq_p
                                                                .with(|value| value.get())
                                                                .invoke(&[Scm::symbol("given"), {
                                                                    // (caar section*)
                                                                    imports::caar
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            section_star_.clone()
                                                                        ])
                                                                }])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (given (car section*) (dispatch (cdr section*) body))
                                                                given.get().invoke(&[
                                                                    {
                                                                        // (car section*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                section_star_
                                                                                    .clone(),
                                                                            ])
                                                                    },
                                                                    {
                                                                        // (dispatch (cdr section*) body)
                                                                        dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])
                                                                    },
                                                                ])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote when) (caar section*))
                                                            imports::eq_p
                                                                .with(|value| value.get())
                                                                .invoke(&[Scm::symbol("when"), {
                                                                    // (caar section*)
                                                                    imports::caar
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            section_star_.clone()
                                                                        ])
                                                                }])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (when (car section*) (dispatch (cdr section*) body))
                                                                when.get().invoke(&[
                                                                    {
                                                                        // (car section*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                section_star_
                                                                                    .clone(),
                                                                            ])
                                                                    },
                                                                    {
                                                                        // (dispatch (cdr section*) body)
                                                                        dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])
                                                                    },
                                                                ])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote then) (caar section*))
                                                            imports::eq_p
                                                                .with(|value| value.get())
                                                                .invoke(&[Scm::symbol("then"), {
                                                                    // (caar section*)
                                                                    imports::caar
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            section_star_.clone()
                                                                        ])
                                                                }])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (then (car section*) (dispatch (cdr section*) body))
                                                                then.get().invoke(&[
                                                                    {
                                                                        // (car section*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                section_star_
                                                                                    .clone(),
                                                                            ])
                                                                    },
                                                                    {
                                                                        // (dispatch (cdr section*) body)
                                                                        dispatch.get().invoke(&[{
// (cdr section*)
imports::cdr.with(|value| value.get()).invoke(&[section_star_.clone()])},body.clone()])
                                                                    },
                                                                ])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "invalid testcase")
                                                                imports::error
                                                                    .with(|value| value.get())
                                                                    .invoke(&[Scm::from(
                                                                        "invalid testcase",
                                                                    )])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            {
                                                // (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))
                                                {
                                                    let body = {
                                                        // (dispatch (testcase-body case) (quote ()))
                                                        dispatch.get().invoke(&[
                                                            {
                                                                // (testcase-body case)
                                                                imports::testcase_minus_body
                                                                    .with(|value| value.get())
                                                                    .invoke(&[case.clone()])
                                                            },
                                                            Scm::Nil,
                                                        ])
                                                    };
                                                    {
                                                        // (make-testcase (testcase-description case) (astify body env #f))
                                                        imports::make_minus_testcase.with(|value| value.get()).invoke(&[{
// (testcase-description case)
imports::testcase_minus_description.with(|value| value.get()).invoke(&[case.clone()])},{
// (astify body env #f)
globals::astify.with(|value| value.get()).invoke(&[body.clone(),env.clone(),Scm::False])}])
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
    }
    pub fn astify_minus_testsuite(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let name = args[0].clone();
            let cases = args[1].clone();
            let env = args[2].clone();
            {
                // (make-testsuite name (map (lambda (c) (astify-testcase c env)) cases))
                imports::make_minus_testsuite
                    .with(|value| value.get())
                    .invoke(&[name.clone(), {
                        // (map (lambda (c) (astify-testcase c env)) cases)
                        imports::map.with(|value| value.get()).invoke(&[
                            {
                                // Closure
                                let env = env.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let c = args[0].clone();
                                    {
                                        // (astify-testcase c env)
                                        globals::astify_minus_testcase
                                            .with(|value| value.get())
                                            .invoke(&[c.clone(), env.clone()])
                                    }
                                })
                            },
                            cases.clone(),
                        ])
                    }])
            }
        }
    }
    pub fn astify_minus_unspecified(args: &[Scm]) {
        {
            if args.len() != 0 {
                panic!("invalid arity")
            }
            {
                // (make-constant (quote *UNSPECIFIED*))
                imports::make_minus_constant
                    .with(|value| value.get())
                    .invoke(&[Scm::symbol("*UNSPECIFIED*")])
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
    crate::sunny::ast::initialize();
    crate::sunny::library::initialize();
    crate::sunny::env::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (astify exp env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-abstraction param* body env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-alternative condition consequent alternative env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-and arg* env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-application proc arg* env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-args arg* env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-assert cond env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-assignment var-name value env) ...)
            (/*NOP*/)
        };
        {
            // (define astify-comment make-comment)
            globals::astify_minus_comment
                .with(|value| value.set(imports::make_minus_comment.with(|value| value.get())))
        };
        {
            // (define (astify-cond clause* env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-constant exp env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-definition var-name value env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-export export-spec* env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-import stmt* env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-import-all libname env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-import-only libname names env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-sequence exp* env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-symbol name env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-testsuite name cases env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-testcase case env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-unspecified) ...)
            (/*NOP*/)
        }
    };
}
