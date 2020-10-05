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
    pub use super::astify;
    pub use super::astify_minus_abstraction;
    pub use super::astify_minus_alternative;
    pub use super::astify_minus_and;
    pub use super::astify_minus_application;
    pub use super::astify_minus_assert;
    pub use super::astify_minus_assignment;
    pub use super::astify_minus_comment;
    pub use super::astify_minus_cond;
    pub use super::astify_minus_constant;
    pub use super::astify_minus_definition;
    pub use super::astify_minus_export;
    pub use super::astify_minus_import;
    pub use super::astify_minus_sequence;
    pub use super::astify_minus_symbol;
    pub use super::astify_minus_testsuite;
}

pub fn astify(args: &[Scm]) -> Scm {
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
                Scm::func(imports::pair_p).invoke(&[exp.clone()])
            })
            .is_true()
            {
                {
                    // (let ((f-obj (astify (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (astify-comment exp (astify-application f-obj (cdr exp) env tail?))))
                    {
                        let f_minus_obj = {
                            // (astify (car exp) env #f)
                            Scm::func(astify).invoke(&[
                                {
                                    // (car exp)
                                    Scm::func(imports::car).invoke(&[exp.clone()])
                                },
                                env.clone(),
                                Scm::False,
                            ])
                        };
                        if ({
                            // (keyword? f-obj)
                            Scm::func(imports::keyword_p).invoke(&[f_minus_obj.clone()])
                        })
                        .is_true()
                        {
                            {
                                // ((keyword-handler f-obj) exp env tail?)
                                {
                                    // (keyword-handler f-obj)
                                    Scm::func(imports::keyword_minus_handler)
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
                                Scm::func(astify_minus_comment).invoke(&[exp.clone(), {
                                    // (astify-application f-obj (cdr exp) env tail?)
                                    Scm::func(astify_minus_application).invoke(&[
                                        f_minus_obj.clone(),
                                        {
                                            // (cdr exp)
                                            Scm::func(imports::cdr).invoke(&[exp.clone()])
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
                Scm::func(imports::symbol_p).invoke(&[exp.clone()])
            })
            .is_true()
            {
                {
                    // (astify-symbol exp env)
                    Scm::func(astify_minus_symbol).invoke(&[exp.clone(), env.clone()])
                }
            } else {
                {
                    // (astify-constant exp env)
                    Scm::func(astify_minus_constant).invoke(&[exp.clone(), env.clone()])
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_abstraction(args: &[Scm]) -> Scm {
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
                        Scm::func(imports::adjoin_minus_local_minus_env)
                            .invoke(&[param_star_.clone(), env.clone()])
                    };
                    // (let ((body-sexpr (scan-out-defines body))) (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast)))))
                    let body_minus_sexpr = {
                        // (scan-out-defines body)
                        Scm::func(imports::scan_minus_out_minus_defines).invoke(&[body.clone()])
                    };
                    // (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction param* (lookup* param* local-env) body-ast))))
                    let body_minus_ast = {
                        // (astify-sequence body-sexpr local-env #t)
                        Scm::func(astify_minus_sequence).invoke(&[
                            body_minus_sexpr.clone(),
                            local_minus_env.clone(),
                            Scm::True,
                        ])
                    };
                    if ({
                        // (dotted-list? param*)
                        Scm::func(imports::dotted_minus_list_p).invoke(&[param_star_.clone()])
                    })
                    .is_true()
                    {
                        {
                            // (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast))
                            {
                                let [fix_minus_param, var_minus_param] = [
                                    {
                                        // (proper-list-part param*)
                                        Scm::func(imports::proper_minus_list_minus_part)
                                            .invoke(&[param_star_.clone()])
                                    },
                                    {
                                        // (last-cdr param*)
                                        Scm::func(imports::last_minus_cdr)
                                            .invoke(&[param_star_.clone()])
                                    },
                                ];
                                {
                                    // (make-vararg-abstraction fix-param var-param (lookup* fix-param local-env) (lookup var-param local-env) body-ast)
                                    Scm::func(imports::make_minus_vararg_minus_abstraction).invoke(
                                        &[
                                            fix_minus_param.clone(),
                                            var_minus_param.clone(),
                                            {
                                                // (lookup* fix-param local-env)
                                                Scm::func(imports::lookup_star_).invoke(&[
                                                    fix_minus_param.clone(),
                                                    local_minus_env.clone(),
                                                ])
                                            },
                                            {
                                                // (lookup var-param local-env)
                                                Scm::func(imports::lookup).invoke(&[
                                                    var_minus_param.clone(),
                                                    local_minus_env.clone(),
                                                ])
                                            },
                                            body_minus_ast.clone(),
                                        ],
                                    )
                                }
                            }
                        }
                    } else {
                        {
                            // (make-abstraction param* (lookup* param* local-env) body-ast)
                            Scm::func(imports::make_minus_abstraction).invoke(&[
                                param_star_.clone(),
                                {
                                    // (lookup* param* local-env)
                                    Scm::func(imports::lookup_star_)
                                        .invoke(&[param_star_.clone(), local_minus_env.clone()])
                                },
                                body_minus_ast.clone(),
                            ])
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_alternative(args: &[Scm]) -> Scm {
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
            Scm::func(imports::make_minus_alternative).invoke(&[
                {
                    // (astify condition env #f)
                    Scm::func(astify).invoke(&[condition.clone(), env.clone(), Scm::False])
                },
                {
                    // (astify consequent env tail?)
                    Scm::func(astify).invoke(&[consequent.clone(), env.clone(), tail_p.clone()])
                },
                {
                    // (astify alternative env tail?)
                    Scm::func(astify).invoke(&[alternative.clone(), env.clone(), tail_p.clone()])
                },
            ])
        }
    }
    .into()
}
pub fn astify_minus_and(args: &[Scm]) -> Scm {
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
                Scm::func(imports::null_p).invoke(&[arg_star_.clone()])
            })
            .is_true()
            {
                {
                    // (make-constant #t)
                    Scm::func(imports::make_minus_constant).invoke(&[Scm::True])
                }
            } else if ({
                // (null? (cdr arg*))
                Scm::func(imports::null_p).invoke(&[{
                    // (cdr arg*)
                    Scm::func(imports::cdr).invoke(&[arg_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify (car arg*) env tail?)
                    Scm::func(astify).invoke(&[
                        {
                            // (car arg*)
                            Scm::func(imports::car).invoke(&[arg_star_.clone()])
                        },
                        env.clone(),
                        tail_p.clone(),
                    ])
                }
            } else {
                {
                    // (make-alternative (astify (car arg*) env #f) (astify-and (cdr arg*) env tail?) (astify-constant #f env))
                    Scm::func(imports::make_minus_alternative).invoke(&[
                        {
                            // (astify (car arg*) env #f)
                            Scm::func(astify).invoke(&[
                                {
                                    // (car arg*)
                                    Scm::func(imports::car).invoke(&[arg_star_.clone()])
                                },
                                env.clone(),
                                Scm::False,
                            ])
                        },
                        {
                            // (astify-and (cdr arg*) env tail?)
                            Scm::func(astify_minus_and).invoke(&[
                                {
                                    // (cdr arg*)
                                    Scm::func(imports::cdr).invoke(&[arg_star_.clone()])
                                },
                                env.clone(),
                                tail_p.clone(),
                            ])
                        },
                        {
                            // (astify-constant #f env)
                            Scm::func(astify_minus_constant).invoke(&[Scm::False, env.clone()])
                        },
                    ])
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_application(args: &[Scm]) -> Scm {
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
            Scm::func(imports::eq_p).invoke(&[Scm::symbol("ABSTRACTION"), {
                // (proc (quote kind))
                proc.clone().invoke(&[Scm::symbol("kind")])
            }])
        })
        .is_true()
        {
            {
                // (make-fixlet (proc (quote get-params)) (proc (quote get-vars)) (astify-args arg* env) (proc (quote get-body)))
                Scm::func(imports::make_minus_fixlet).invoke(&[
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
                        Scm::func(astify_minus_args).invoke(&[arg_star_.clone(), env.clone()])
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
                Scm::func(imports::make_minus_application).invoke(&[
                    proc.clone(),
                    {
                        // (astify-args arg* env)
                        Scm::func(astify_minus_args).invoke(&[arg_star_.clone(), env.clone()])
                    },
                    tail_p.clone(),
                ])
            }
        }
    }
    .into()
}
pub fn astify_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let arg_star_ = args[0].clone();
        let env = args[1].clone();
        if ({
            // (null? arg*)
            Scm::func(imports::null_p).invoke(&[arg_star_.clone()])
        })
        .is_true()
        {
            {
                // (make-null-arg)
                Scm::func(imports::make_minus_null_minus_arg).invoke(&[])
            }
        } else {
            {
                // (make-args (astify (car arg*) env #f) (astify-args (cdr arg*) env))
                Scm::func(imports::make_minus_args).invoke(&[
                    {
                        // (astify (car arg*) env #f)
                        Scm::func(astify).invoke(&[
                            {
                                // (car arg*)
                                Scm::func(imports::car).invoke(&[arg_star_.clone()])
                            },
                            env.clone(),
                            Scm::False,
                        ])
                    },
                    {
                        // (astify-args (cdr arg*) env)
                        Scm::func(astify_minus_args).invoke(&[
                            {
                                // (cdr arg*)
                                Scm::func(imports::cdr).invoke(&[arg_star_.clone()])
                            },
                            env.clone(),
                        ])
                    },
                ])
            }
        }
    }
    .into()
}
pub fn astify_minus_assert(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let cond = args[0].clone();
        let env = args[1].clone();
        {
            // (make-assert (astify cond env #f))
            Scm::func(imports::make_minus_assert).invoke(&[{
                // (astify cond env #f)
                Scm::func(astify).invoke(&[cond.clone(), env.clone(), Scm::False])
            }])
        }
    }
    .into()
}
pub fn astify_minus_assignment(args: &[Scm]) -> Scm {
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
                        Scm::func(imports::ensure_minus_var_i)
                            .invoke(&[var_minus_name.clone(), env.clone()])
                    },
                    {
                        // (astify value env #f)
                        Scm::func(astify).invoke(&[value.clone(), env.clone(), Scm::False])
                    },
                ];
                {
                    {
                        // (variable-set-mutable! var)
                        Scm::func(imports::variable_minus_set_minus_mutable_i)
                            .invoke(&[var.clone()])
                    };
                    {
                        // (make-assignment var-name var val)
                        Scm::func(imports::make_minus_assignment).invoke(&[
                            var_minus_name.clone(),
                            var.clone(),
                            val.clone(),
                        ])
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_comment(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let exp = args[0].clone();
        let node = args[1].clone();
        {
            // (make-comment exp node)
            Scm::func(imports::make_minus_comment).invoke(&[exp.clone(), node.clone()])
        }
    }
    .into()
}
pub fn astify_minus_cond(args: &[Scm]) -> Scm {
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
                Scm::func(imports::null_p).invoke(&[clause_star_.clone()])
            })
            .is_true()
            {
                {
                    // (astify-unspecified)
                    Scm::func(astify_minus_unspecified).invoke(&[])
                }
            } else if ({
                // (cond-else-clause? (car clause*))
                Scm::func(imports::cond_minus_else_minus_clause_p).invoke(&[{
                    // (car clause*)
                    Scm::func(imports::car).invoke(&[clause_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
                    Scm::func(astify_minus_sequence).invoke(&[
                        {
                            // (cond-clause-sequence (car clause*))
                            Scm::func(imports::cond_minus_clause_minus_sequence).invoke(&[{
                                // (car clause*)
                                Scm::func(imports::car).invoke(&[clause_star_.clone()])
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
                                Scm::func(astify).invoke(&[
                                    {
                                        // (cond-clause-condition (car clause*))
                                        Scm::func(imports::cond_minus_clause_minus_condition)
                                            .invoke(&[{
                                                // (car clause*)
                                                Scm::func(imports::car)
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
                                Scm::func(astify_minus_sequence).invoke(&[
                                    {
                                        // (cond-clause-sequence (car clause*))
                                        Scm::func(imports::cond_minus_clause_minus_sequence).invoke(
                                            &[{
                                                // (car clause*)
                                                Scm::func(imports::car)
                                                    .invoke(&[clause_star_.clone()])
                                            }],
                                        )
                                    },
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                            };
                            // (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))
                            let e = {
                                // (astify-cond (cdr clause*) env tail?)
                                Scm::func(astify_minus_cond).invoke(&[
                                    {
                                        // (cdr clause*)
                                        Scm::func(imports::cdr).invoke(&[clause_star_.clone()])
                                    },
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                            };
                            {
                                // (make-alternative i t e)
                                Scm::func(imports::make_minus_alternative).invoke(&[
                                    i.clone(),
                                    t.clone(),
                                    e.clone(),
                                ])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_constant(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let exp = args[0].clone();
        let env = args[1].clone();
        {
            // (make-constant exp)
            Scm::func(imports::make_minus_constant).invoke(&[exp.clone()])
        }
    }
    .into()
}
pub fn astify_minus_definition(args: &[Scm]) -> Scm {
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
                        Scm::func(imports::ensure_minus_var_i)
                            .invoke(&[var_minus_name.clone(), env.clone()])
                    },
                    {
                        // (astify value env #f)
                        Scm::func(astify).invoke(&[value.clone(), env.clone(), Scm::False])
                    },
                ];
                {
                    {
                        // (global-add-definition! var val)
                        Scm::func(imports::global_minus_add_minus_definition_i)
                            .invoke(&[var.clone(), val.clone()])
                    };
                    {
                        // (make-definition var-name var val)
                        Scm::func(imports::make_minus_definition).invoke(&[
                            var_minus_name.clone(),
                            var.clone(),
                            val.clone(),
                        ])
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_export(args: &[Scm]) -> Scm {
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
                Scm::func(imports::null_p).invoke(&[export_minus_spec_star_.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else {
                {
                    // (cons (make-export env (car export-spec*) (car export-spec*)) (astify-export (cdr export-spec*) env))
                    Scm::func(imports::cons).invoke(&[
                        {
                            // (make-export env (car export-spec*) (car export-spec*))
                            Scm::func(imports::make_minus_export).invoke(&[
                                env.clone(),
                                {
                                    // (car export-spec*)
                                    Scm::func(imports::car)
                                        .invoke(&[export_minus_spec_star_.clone()])
                                },
                                {
                                    // (car export-spec*)
                                    Scm::func(imports::car)
                                        .invoke(&[export_minus_spec_star_.clone()])
                                },
                            ])
                        },
                        {
                            // (astify-export (cdr export-spec*) env)
                            Scm::func(astify_minus_export).invoke(&[
                                {
                                    // (cdr export-spec*)
                                    Scm::func(imports::cdr)
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
    .into()
}
pub fn astify_minus_import(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let stmt_star_ = args[0].clone();
        let env = args[1].clone();
        if ({
            // (null? stmt*)
            Scm::func(imports::null_p).invoke(&[stmt_star_.clone()])
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
                        Scm::func(imports::importset_minus_libname).invoke(&[{
                            // (car stmt*)
                            Scm::func(imports::car).invoke(&[stmt_star_.clone()])
                        }])
                    };
                    {
                        // (cond ...)
                        if ({
                            // (equal? (quote (sunny testing)) libname)
                            Scm::func(imports::equal_p).invoke(&[
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
                                Scm::func(astify_minus_import).invoke(&[
                                    {
                                        // (cdr stmt*)
                                        Scm::func(imports::cdr).invoke(&[stmt_star_.clone()])
                                    },
                                    env.clone(),
                                ])
                            }
                        } else if ({
                            // (importset-only? (car stmt*))
                            Scm::func(imports::importset_minus_only_p).invoke(&[{
                                // (car stmt*)
                                Scm::func(imports::car).invoke(&[stmt_star_.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (cons (astify-import-only libname (importset-only-names (car stmt*)) env) (astify-import (cdr stmt*) env))
                                Scm::func(imports::cons).invoke(&[
                                    {
                                        // (astify-import-only libname (importset-only-names (car stmt*)) env)
                                        Scm::func(astify_minus_import_minus_only).invoke(&[
                                            libname.clone(),
                                            {
                                                // (importset-only-names (car stmt*))
                                                Scm::func(imports::importset_minus_only_minus_names)
                                                    .invoke(&[{
                                                        // (car stmt*)
                                                        Scm::func(imports::car)
                                                            .invoke(&[stmt_star_.clone()])
                                                    }])
                                            },
                                            env.clone(),
                                        ])
                                    },
                                    {
                                        // (astify-import (cdr stmt*) env)
                                        Scm::func(astify_minus_import).invoke(&[
                                            {
                                                // (cdr stmt*)
                                                Scm::func(imports::cdr)
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
                                Scm::func(imports::cons).invoke(&[
                                    {
                                        // (astify-import-all libname env)
                                        Scm::func(astify_minus_import_minus_all)
                                            .invoke(&[libname.clone(), env.clone()])
                                    },
                                    {
                                        // (astify-import (cdr stmt*) env)
                                        Scm::func(astify_minus_import).invoke(&[
                                            {
                                                // (cdr stmt*)
                                                Scm::func(imports::cdr)
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
    .into()
}
pub fn astify_minus_import_minus_all(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let libname = args[0].clone();
        let env = args[1].clone();
        {
            {
                // (adjoin-import*! (library-exports (library-decls (get-lib libname))) env)
                Scm::func(imports::adjoin_minus_import_star__i).invoke(&[
                    {
                        // (library-exports (library-decls (get-lib libname)))
                        Scm::func(imports::library_minus_exports).invoke(&[{
                            // (library-decls (get-lib libname))
                            Scm::func(imports::library_minus_decls).invoke(&[{
                                // (get-lib libname)
                                Scm::func(imports::get_minus_lib).invoke(&[libname.clone()])
                            }])
                        }])
                    },
                    env.clone(),
                ])
            };
            {
                // (make-import libname)
                Scm::func(imports::make_minus_import).invoke(&[libname.clone()])
            }
        }
    }
    .into()
}
pub fn astify_minus_import_minus_only(args: &[Scm]) -> Scm {
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
                Scm::func(imports::check_minus_imports).invoke(&[
                    names.clone(),
                    {
                        // (library-exports (library-decls (get-lib libname)))
                        Scm::func(imports::library_minus_exports).invoke(&[{
                            // (library-decls (get-lib libname))
                            Scm::func(imports::library_minus_decls).invoke(&[{
                                // (get-lib libname)
                                Scm::func(imports::get_minus_lib).invoke(&[libname.clone()])
                            }])
                        }])
                    },
                    libname.clone(),
                ])
            };
            {
                // (adjoin-import*! names env)
                Scm::func(imports::adjoin_minus_import_star__i)
                    .invoke(&[names.clone(), env.clone()])
            };
            {
                // (make-import-only libname names)
                Scm::func(imports::make_minus_import_minus_only)
                    .invoke(&[libname.clone(), names.clone()])
            }
        }
    }
    .into()
}
pub fn astify_minus_sequence(args: &[Scm]) -> Scm {
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
                Scm::func(imports::null_p).invoke(&[exp_star_.clone()])
            })
            .is_true()
            {
                {
                    // (error "empty sequence")
                    Scm::func(imports::error).invoke(&[Scm::from("empty sequence")])
                }
            } else if ({
                // (null? (cdr exp*))
                Scm::func(imports::null_p).invoke(&[{
                    // (cdr exp*)
                    Scm::func(imports::cdr).invoke(&[exp_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify (car exp*) env tail?)
                    Scm::func(astify).invoke(&[
                        {
                            // (car exp*)
                            Scm::func(imports::car).invoke(&[exp_star_.clone()])
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
                                Scm::func(astify).invoke(&[
                                    {
                                        // (car exp*)
                                        Scm::func(imports::car).invoke(&[exp_star_.clone()])
                                    },
                                    env.clone(),
                                    Scm::False,
                                ])
                            };
                            // (let ((rest (astify-sequence (cdr exp*) env tail?))) (begin (make-sequence first rest)))
                            let rest = {
                                // (astify-sequence (cdr exp*) env tail?)
                                Scm::func(astify_minus_sequence).invoke(&[
                                    {
                                        // (cdr exp*)
                                        Scm::func(imports::cdr).invoke(&[exp_star_.clone()])
                                    },
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                            };
                            {
                                // (make-sequence first rest)
                                Scm::func(imports::make_minus_sequence)
                                    .invoke(&[first.clone(), rest.clone()])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_symbol(args: &[Scm]) -> Scm {
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
                    Scm::func(imports::ensure_minus_var_i).invoke(&[name.clone(), env.clone()])
                };
                if ({
                    // (keyword? var)
                    Scm::func(imports::keyword_p).invoke(&[var.clone()])
                })
                .is_true()
                {
                    var.clone()
                } else {
                    {
                        // (make-reference name var)
                        Scm::func(imports::make_minus_reference)
                            .invoke(&[name.clone(), var.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_testcase(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let case = args[0].clone();let env = args[1].clone();{
// (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f))))
{
// (let ((given (quote *uninitialized*)) (when (quote *uninitialized*)) (then (quote *uninitialized*)) (dispatch (quote *uninitialized*))) (begin (set! given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (set! when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (set! then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (set! dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))))
{let [given, when, then, dispatch, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let dispatch = dispatch.into_boxed();{let then = then.into_boxed();{let when = when.into_boxed();{let given = given.into_boxed();{given.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
Scm::func(imports::list).invoke(&[Scm::symbol("let*"),{
// (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
Scm::func(imports::map).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let assignment = args[0].clone();{
// (list (car assignment) (caddr assignment))
Scm::func(imports::list).invoke(&[{
// (car assignment)
Scm::func(imports::car).invoke(&[assignment.clone()])},{
// (caddr assignment)
Scm::func(imports::caddr).invoke(&[assignment.clone()])}])}})},{
// (cdr stmt)
Scm::func(imports::cdr).invoke(&[stmt.clone()])}])},body.clone()])}})});when.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
{
// (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))) (loop (cdr stmt))))
{let loop_ = Scm::symbol("*uninitialized*");{let loop_ = loop_.into_boxed();{loop_.set({// Closure
let body = body.clone();let loop_ = loop_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let stmt_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? stmt*)
Scm::func(imports::null_p).invoke(&[stmt_star_.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote <-) (cadar stmt*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("<-"),{
// (cadar stmt*)
Scm::func(imports::cadar).invoke(&[stmt_star_.clone()])}])}).is_true() {{
// (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))
Scm::func(imports::list).invoke(&[Scm::symbol("let"),{
// (list (list (caar stmt*) (caddar stmt*)))
Scm::func(imports::list).invoke(&[{
// (list (caar stmt*) (caddar stmt*))
Scm::func(imports::list).invoke(&[{
// (caar stmt*)
Scm::func(imports::caar).invoke(&[stmt_star_.clone()])},{
// (caddar stmt*)
Scm::func(imports::caddar).invoke(&[stmt_star_.clone()])}])}])},{
// (loop (cdr stmt*))
loop_.get().invoke(&[{
// (cdr stmt*)
Scm::func(imports::cdr).invoke(&[stmt_star_.clone()])}])}])}} else {{
// (list (quote begin) (car stmt*) (loop (cdr stmt*)))
Scm::func(imports::list).invoke(&[Scm::symbol("begin"),{
// (car stmt*)
Scm::func(imports::car).invoke(&[stmt_star_.clone()])},{
// (loop (cdr stmt*))
loop_.get().invoke(&[{
// (cdr stmt*)
Scm::func(imports::cdr).invoke(&[stmt_star_.clone()])}])}])}}}})});{
// (loop (cdr stmt))
loop_.get().invoke(&[{
// (cdr stmt)
Scm::func(imports::cdr).invoke(&[stmt.clone()])}])}}}}}}})});then.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt = args[0].clone();let body = args[1].clone();{
// (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
Scm::func(imports::cons).invoke(&[Scm::symbol("begin"),{
// (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
Scm::func(imports::append).invoke(&[{
// (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
Scm::func(imports::map).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let pred = args[0].clone();{
// (list (quote assert) pred)
Scm::func(imports::list).invoke(&[Scm::symbol("assert"),pred.clone()])}})},{
// (cdr stmt)
Scm::func(imports::cdr).invoke(&[stmt.clone()])}])},body.clone()])}])}})});dispatch.set({// Closure
let given = given.clone();let dispatch = dispatch.clone();let when = when.clone();let then = then.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let section_star_ = args[0].clone();let body = args[1].clone();{
// (cond ...)
if ({
// (null? section*)
Scm::func(imports::null_p).invoke(&[section_star_.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote given) (caar section*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("given"),{
// (caar section*)
Scm::func(imports::caar).invoke(&[section_star_.clone()])}])}).is_true() {{
// (given (car section*) (dispatch (cdr section*) body))
given.get().invoke(&[{
// (car section*)
Scm::func(imports::car).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
Scm::func(imports::cdr).invoke(&[section_star_.clone()])},body.clone()])}])}} else if ({
// (eq? (quote when) (caar section*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("when"),{
// (caar section*)
Scm::func(imports::caar).invoke(&[section_star_.clone()])}])}).is_true() {{
// (when (car section*) (dispatch (cdr section*) body))
when.get().invoke(&[{
// (car section*)
Scm::func(imports::car).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
Scm::func(imports::cdr).invoke(&[section_star_.clone()])},body.clone()])}])}} else if ({
// (eq? (quote then) (caar section*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("then"),{
// (caar section*)
Scm::func(imports::caar).invoke(&[section_star_.clone()])}])}).is_true() {{
// (then (car section*) (dispatch (cdr section*) body))
then.get().invoke(&[{
// (car section*)
Scm::func(imports::car).invoke(&[section_star_.clone()])},{
// (dispatch (cdr section*) body)
dispatch.get().invoke(&[{
// (cdr section*)
Scm::func(imports::cdr).invoke(&[section_star_.clone()])},body.clone()])}])}} else {{
// (error "invalid testcase")
Scm::func(imports::error).invoke(&[Scm::from("invalid testcase")])}}}})});{
// (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))
{let body = {
// (dispatch (testcase-body case) (quote ()))
dispatch.get().invoke(&[{
// (testcase-body case)
Scm::func(imports::testcase_minus_body).invoke(&[case.clone()])},Scm::Nil])};{
// (make-testcase (testcase-description case) (astify body env #f))
Scm::func(imports::make_minus_testcase).invoke(&[{
// (testcase-description case)
Scm::func(imports::testcase_minus_description).invoke(&[case.clone()])},{
// (astify body env #f)
Scm::func(astify).invoke(&[body.clone(),env.clone(),Scm::False])}])}}}}}}}}}}}}.into()
}
pub fn astify_minus_testsuite(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        let cases = args[1].clone();
        let env = args[2].clone();
        {
            // (make-testsuite name (map (lambda (c) (astify-testcase c env)) cases))
            Scm::func(imports::make_minus_testsuite).invoke(&[name.clone(), {
                // (map (lambda (c) (astify-testcase c env)) cases)
                Scm::func(imports::map).invoke(&[
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
                                Scm::func(astify_minus_testcase).invoke(&[c.clone(), env.clone()])
                            }
                        })
                    },
                    cases.clone(),
                ])
            }])
        }
    }
    .into()
}
pub fn astify_minus_unspecified(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (make-constant (quote *UNSPECIFIED*))
            Scm::func(imports::make_minus_constant).invoke(&[Scm::symbol("*UNSPECIFIED*")])
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
            // (define (astify-comment exp node) ...)
            (/*NOP*/)
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
