#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::table::exports::*;
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
    pub use super::make_minus_syntactic_minus_closure;
}

thread_local! {#[allow(non_upper_case_globals)] pub static SyntacticClosure: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE SyntacticClosure"))}
pub fn astify(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let exp__0 = args[0].clone();
        let env__15 = args[1].clone();
        let tail_p__2 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (syntactic-closure? exp)
                Scm::func(syntactic_minus_closure_p).invoke(&[exp__0.clone()])
            })
            .is_true()
            {
                {
                    // (astify-syntactic-closure exp env tail?)
                    Scm::func(astify_minus_syntactic_minus_closure).invoke(&[
                        exp__0.clone(),
                        env__15.clone(),
                        tail_p__2.clone(),
                    ])
                }
            } else if ({
                // (pair? exp)
                imports::pair_p(&[exp__0.clone()])
            })
            .is_true()
            {
                {
                    // (let ((f-obj (astify (car exp) env #f))) (if (keyword? f-obj) ((keyword-handler f-obj) exp env tail?) (astify-comment exp (astify-application f-obj (cdr exp) env tail?))))
                    {
                        let f_minus_obj__0 = {
                            // (astify (car exp) env #f)
                            Scm::func(astify).invoke(&[
                                {
                                    // (car exp)
                                    imports::car(&[exp__0.clone()])
                                },
                                env__15.clone(),
                                Scm::False,
                            ])
                        };
                        if ({
                            // (keyword? f-obj)
                            imports::keyword_p(&[f_minus_obj__0.clone()])
                        })
                        .is_true()
                        {
                            {
                                // ((keyword-handler f-obj) exp env tail?)
                                {
                                    // (keyword-handler f-obj)
                                    imports::keyword_minus_handler(&[f_minus_obj__0.clone()])
                                }
                                .invoke(&[
                                    exp__0.clone(),
                                    env__15.clone(),
                                    tail_p__2.clone(),
                                ])
                            }
                        } else {
                            {
                                // (astify-comment exp (astify-application f-obj (cdr exp) env tail?))
                                Scm::func(astify_minus_comment).invoke(&[exp__0.clone(), {
                                    // (astify-application f-obj (cdr exp) env tail?)
                                    Scm::func(astify_minus_application).invoke(&[
                                        f_minus_obj__0.clone(),
                                        {
                                            // (cdr exp)
                                            imports::cdr(&[exp__0.clone()])
                                        },
                                        env__15.clone(),
                                        tail_p__2.clone(),
                                    ])
                                }])
                            }
                        }
                    }
                }
            } else if ({
                // (symbol? exp)
                imports::symbol_p(&[exp__0.clone()])
            })
            .is_true()
            {
                {
                    // (astify-symbol exp env)
                    Scm::func(astify_minus_symbol).invoke(&[exp__0.clone(), env__15.clone()])
                }
            } else {
                {
                    // (astify-constant exp env)
                    Scm::func(astify_minus_constant).invoke(&[exp__0.clone(), env__15.clone()])
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
        let param_star___0 = args[0].clone();
        let body__15 = args[1].clone();
        let env__16 = args[2].clone();
        {
            // (let* ((local-env (adjoin-local-env param* env)) (body-sexpr (scan-out-defines body)) (body-ast (astify-sequence body-sexpr local-env #t))) (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction (lookup* param* local-env) body-ast)))
            {
                // (let ((local-env (adjoin-local-env param* env))) (let ((body-sexpr (scan-out-defines body))) (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction (lookup* param* local-env) body-ast))))))
                {
                    let local_minus_env__0 = {
                        // (adjoin-local-env param* env)
                        imports::adjoin_minus_local_minus_env(&[
                            param_star___0.clone(),
                            env__16.clone(),
                        ])
                    };
                    // (let ((body-sexpr (scan-out-defines body))) (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction (lookup* param* local-env) body-ast)))))
                    let body_minus_sexpr__0 = {
                        // (scan-out-defines body)
                        imports::scan_minus_out_minus_defines(&[body__15.clone()])
                    };
                    // (let ((body-ast (astify-sequence body-sexpr local-env #t))) (begin (if (dotted-list? param*) (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast)) (make-abstraction (lookup* param* local-env) body-ast))))
                    let body_minus_ast__0 = {
                        // (astify-sequence body-sexpr local-env #t)
                        Scm::func(astify_minus_sequence).invoke(&[
                            body_minus_sexpr__0.clone(),
                            local_minus_env__0.clone(),
                            Scm::True,
                        ])
                    };
                    if ({
                        // (dotted-list? param*)
                        imports::dotted_minus_list_p(&[param_star___0.clone()])
                    })
                    .is_true()
                    {
                        {
                            // (let ((fix-param (proper-list-part param*)) (var-param (last-cdr param*))) (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast))
                            {
                                let [fix_minus_param__0, var_minus_param__0] = [
                                    {
                                        // (proper-list-part param*)
                                        imports::proper_minus_list_minus_part(&[
                                            param_star___0.clone()
                                        ])
                                    },
                                    {
                                        // (last-cdr param*)
                                        imports::last_minus_cdr(&[param_star___0.clone()])
                                    },
                                ];
                                {
                                    // (make-vararg-abstraction (lookup* fix-param local-env) (lookup var-param local-env) body-ast)
                                    imports::make_minus_vararg_minus_abstraction(&[
                                        {
                                            // (lookup* fix-param local-env)
                                            imports::lookup_star_(&[
                                                fix_minus_param__0.clone(),
                                                local_minus_env__0.clone(),
                                            ])
                                        },
                                        {
                                            // (lookup var-param local-env)
                                            imports::lookup(&[
                                                var_minus_param__0.clone(),
                                                local_minus_env__0.clone(),
                                            ])
                                        },
                                        body_minus_ast__0.clone(),
                                    ])
                                }
                            }
                        }
                    } else {
                        {
                            // (make-abstraction (lookup* param* local-env) body-ast)
                            imports::make_minus_abstraction(&[
                                {
                                    // (lookup* param* local-env)
                                    imports::lookup_star_(&[
                                        param_star___0.clone(),
                                        local_minus_env__0.clone(),
                                    ])
                                },
                                body_minus_ast__0.clone(),
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
        let condition__2 = args[0].clone();
        let consequent__1 = args[1].clone();
        let alternative__1 = args[2].clone();
        let env__17 = args[3].clone();
        let tail_p__3 = args[4].clone();
        {
            // (make-alternative (astify condition env #f) (astify consequent env tail?) (astify alternative env tail?))
            imports::make_minus_alternative(&[
                {
                    // (astify condition env #f)
                    astify(&[condition__2.clone(), env__17.clone(), Scm::False])
                },
                {
                    // (astify consequent env tail?)
                    astify(&[consequent__1.clone(), env__17.clone(), tail_p__3.clone()])
                },
                {
                    // (astify alternative env tail?)
                    astify(&[alternative__1.clone(), env__17.clone(), tail_p__3.clone()])
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
        let arg_star___1 = args[0].clone();
        let env__18 = args[1].clone();
        let tail_p__4 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? arg*)
                imports::null_p(&[arg_star___1.clone()])
            })
            .is_true()
            {
                {
                    // (make-constant #t)
                    imports::make_minus_constant(&[Scm::True])
                }
            } else if ({
                // (null? (cdr arg*))
                imports::null_p(&[{
                    // (cdr arg*)
                    imports::cdr(&[arg_star___1.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify (car arg*) env tail?)
                    astify(&[
                        {
                            // (car arg*)
                            imports::car(&[arg_star___1.clone()])
                        },
                        env__18.clone(),
                        tail_p__4.clone(),
                    ])
                }
            } else {
                {
                    // (make-alternative (astify (car arg*) env #f) (astify-and (cdr arg*) env tail?) (astify-constant #f env))
                    imports::make_minus_alternative(&[
                        {
                            // (astify (car arg*) env #f)
                            astify(&[
                                {
                                    // (car arg*)
                                    imports::car(&[arg_star___1.clone()])
                                },
                                env__18.clone(),
                                Scm::False,
                            ])
                        },
                        {
                            // (astify-and (cdr arg*) env tail?)
                            Scm::func(astify_minus_and).invoke(&[
                                {
                                    // (cdr arg*)
                                    imports::cdr(&[arg_star___1.clone()])
                                },
                                env__18.clone(),
                                tail_p__4.clone(),
                            ])
                        },
                        {
                            // (astify-constant #f env)
                            Scm::func(astify_minus_constant).invoke(&[Scm::False, env__18.clone()])
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
        let proc__2 = args[0].clone();
        let arg_star___2 = args[1].clone();
        let env__19 = args[2].clone();
        let tail_p__5 = args[3].clone();
        {
            // (cond ...)
            if ({
                // (eq? (quote ABSTRACTION) (proc (quote kind)))
                imports::eq_p(&[Scm::symbol("ABSTRACTION"), {
                    // (proc (quote kind))
                    proc__2.clone().invoke(&[Scm::symbol("kind")])
                }])
            })
            .is_true()
            {
                {
                    // (make-fixlet (proc (quote get-vars)) (astify-args arg* env) (proc (quote get-body)))
                    imports::make_minus_fixlet(&[
                        {
                            // (proc (quote get-vars))
                            proc__2.clone().invoke(&[Scm::symbol("get-vars")])
                        },
                        {
                            // (astify-args arg* env)
                            Scm::func(astify_minus_args)
                                .invoke(&[arg_star___2.clone(), env__19.clone()])
                        },
                        {
                            // (proc (quote get-body))
                            proc__2.clone().invoke(&[Scm::symbol("get-body")])
                        },
                    ])
                }
            } else if ({
                // (and (eq? (quote REFERENCE) (proc (quote kind))) (bor (import-variable? (proc (quote get-var))) (global-function? (proc (quote get-var)))))
                if ({
                    // (eq? (quote REFERENCE) (proc (quote kind)))
                    imports::eq_p(&[Scm::symbol("REFERENCE"), {
                        // (proc (quote kind))
                        proc__2.clone().invoke(&[Scm::symbol("kind")])
                    }])
                })
                .is_true()
                {
                    {
                        // (bor (import-variable? (proc (quote get-var))) (global-function? (proc (quote get-var))))
                        imports::bor(&[
                            {
                                // (import-variable? (proc (quote get-var)))
                                imports::import_minus_variable_p(&[{
                                    // (proc (quote get-var))
                                    proc__2.clone().invoke(&[Scm::symbol("get-var")])
                                }])
                            },
                            {
                                // (global-function? (proc (quote get-var)))
                                imports::global_minus_function_p(&[{
                                    // (proc (quote get-var))
                                    proc__2.clone().invoke(&[Scm::symbol("get-var")])
                                }])
                            },
                        ])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (make-function-application (proc (quote get-var)) (astify-args arg* env) tail?)
                    imports::make_minus_function_minus_application(&[
                        {
                            // (proc (quote get-var))
                            proc__2.clone().invoke(&[Scm::symbol("get-var")])
                        },
                        {
                            // (astify-args arg* env)
                            Scm::func(astify_minus_args)
                                .invoke(&[arg_star___2.clone(), env__19.clone()])
                        },
                        tail_p__5.clone(),
                    ])
                }
            } else {
                {
                    // (make-application proc (astify-args arg* env) tail?)
                    imports::make_minus_application(&[
                        proc__2.clone(),
                        {
                            // (astify-args arg* env)
                            Scm::func(astify_minus_args)
                                .invoke(&[arg_star___2.clone(), env__19.clone()])
                        },
                        tail_p__5.clone(),
                    ])
                }
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
        let arg_star___3 = args[0].clone();
        let env__20 = args[1].clone();
        if ({
            // (null? arg*)
            imports::null_p(&[arg_star___3.clone()])
        })
        .is_true()
        {
            {
                // (make-null-arg)
                imports::make_minus_null_minus_arg(&[])
            }
        } else {
            {
                // (make-args (astify (car arg*) env #f) (astify-args (cdr arg*) env))
                imports::make_minus_args(&[
                    {
                        // (astify (car arg*) env #f)
                        astify(&[
                            {
                                // (car arg*)
                                imports::car(&[arg_star___3.clone()])
                            },
                            env__20.clone(),
                            Scm::False,
                        ])
                    },
                    {
                        // (astify-args (cdr arg*) env)
                        Scm::func(astify_minus_args).invoke(&[
                            {
                                // (cdr arg*)
                                imports::cdr(&[arg_star___3.clone()])
                            },
                            env__20.clone(),
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
        let cond__0 = args[0].clone();
        let env__21 = args[1].clone();
        {
            // (make-assert (astify cond env #f))
            imports::make_minus_assert(&[{
                // (astify cond env #f)
                astify(&[cond__0.clone(), env__21.clone(), Scm::False])
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
        let var_minus_name__0 = args[0].clone();
        let value__3 = args[1].clone();
        let env__22 = args[2].clone();
        {
            // (let ((var (ensure-var! var-name env)) (val (astify value env #f))) (variable-set-mutable! var) (make-assignment var val))
            {
                let [var__23, val__7] = [
                    {
                        // (ensure-var! var-name env)
                        imports::ensure_minus_var_i(&[var_minus_name__0.clone(), env__22.clone()])
                    },
                    {
                        // (astify value env #f)
                        astify(&[value__3.clone(), env__22.clone(), Scm::False])
                    },
                ];
                {
                    {
                        // (variable-set-mutable! var)
                        imports::variable_minus_set_minus_mutable_i(&[var__23.clone()])
                    };
                    {
                        // (make-assignment var val)
                        imports::make_minus_assignment(&[var__23.clone(), val__7.clone()])
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
        let exp__1 = args[0].clone();
        let node__19 = args[1].clone();
        {
            // (make-comment exp node)
            imports::make_minus_comment(&[exp__1.clone(), node__19.clone()])
        }
    }
    .into()
}
pub fn astify_minus_cond(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let clause_star___0 = args[0].clone();
        let env__23 = args[1].clone();
        let tail_p__6 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? clause*)
                imports::null_p(&[clause_star___0.clone()])
            })
            .is_true()
            {
                {
                    // (astify-unspecified)
                    Scm::func(astify_minus_unspecified).invoke(&[])
                }
            } else if ({
                // (cond-else-clause? (car clause*))
                imports::cond_minus_else_minus_clause_p(&[{
                    // (car clause*)
                    imports::car(&[clause_star___0.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
                    Scm::func(astify_minus_sequence).invoke(&[
                        {
                            // (cond-clause-sequence (car clause*))
                            imports::cond_minus_clause_minus_sequence(&[{
                                // (car clause*)
                                imports::car(&[clause_star___0.clone()])
                            }])
                        },
                        env__23.clone(),
                        tail_p__6.clone(),
                    ])
                }
            } else {
                {
                    // (let* ((i (astify (cond-clause-condition (car clause*)) env #f)) (t (astify-sequence (cond-clause-sequence (car clause*)) env tail?)) (e (astify-cond (cdr clause*) env tail?))) (make-alternative i t e))
                    {
                        // (let ((i (astify (cond-clause-condition (car clause*)) env #f))) (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))))
                        {
                            let i__2 = {
                                // (astify (cond-clause-condition (car clause*)) env #f)
                                astify(&[
                                    {
                                        // (cond-clause-condition (car clause*))
                                        imports::cond_minus_clause_minus_condition(&[{
                                            // (car clause*)
                                            imports::car(&[clause_star___0.clone()])
                                        }])
                                    },
                                    env__23.clone(),
                                    Scm::False,
                                ])
                            };
                            // (let ((t (astify-sequence (cond-clause-sequence (car clause*)) env tail?))) (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e))))
                            let t__14 = {
                                // (astify-sequence (cond-clause-sequence (car clause*)) env tail?)
                                Scm::func(astify_minus_sequence).invoke(&[
                                    {
                                        // (cond-clause-sequence (car clause*))
                                        imports::cond_minus_clause_minus_sequence(&[{
                                            // (car clause*)
                                            imports::car(&[clause_star___0.clone()])
                                        }])
                                    },
                                    env__23.clone(),
                                    tail_p__6.clone(),
                                ])
                            };
                            // (let ((e (astify-cond (cdr clause*) env tail?))) (begin (make-alternative i t e)))
                            let e__0 = {
                                // (astify-cond (cdr clause*) env tail?)
                                Scm::func(astify_minus_cond).invoke(&[
                                    {
                                        // (cdr clause*)
                                        imports::cdr(&[clause_star___0.clone()])
                                    },
                                    env__23.clone(),
                                    tail_p__6.clone(),
                                ])
                            };
                            {
                                // (make-alternative i t e)
                                imports::make_minus_alternative(&[
                                    i__2.clone(),
                                    t__14.clone(),
                                    e__0.clone(),
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
        let exp__2 = args[0].clone();
        let env = args[1].clone();
        {
            // (make-constant exp)
            imports::make_minus_constant(&[exp__2.clone()])
        }
    }
    .into()
}
pub fn astify_minus_definition(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let var_minus_name__1 = args[0].clone();
        let value__4 = args[1].clone();
        let env__24 = args[2].clone();
        {
            // (let ((var (ensure-var! var-name env)) (val (astify value env #f))) (global-add-definition! var val) (make-definition var val))
            {
                let [var__24, val__8] = [
                    {
                        // (ensure-var! var-name env)
                        imports::ensure_minus_var_i(&[var_minus_name__1.clone(), env__24.clone()])
                    },
                    {
                        // (astify value env #f)
                        astify(&[value__4.clone(), env__24.clone(), Scm::False])
                    },
                ];
                {
                    {
                        // (global-add-definition! var val)
                        imports::global_minus_add_minus_definition_i(&[
                            var__24.clone(),
                            val__8.clone(),
                        ])
                    };
                    {
                        // (make-definition var val)
                        imports::make_minus_definition(&[var__24.clone(), val__8.clone()])
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
        let export_minus_spec_star___0 = args[0].clone();
        let env__25 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? export-spec*)
                imports::null_p(&[export_minus_spec_star___0.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else {
                {
                    // (cons (make-export (lookup (car export-spec*) env) (car export-spec*)) (astify-export (cdr export-spec*) env))
                    imports::cons(&[
                        {
                            // (make-export (lookup (car export-spec*) env) (car export-spec*))
                            imports::make_minus_export(&[
                                {
                                    // (lookup (car export-spec*) env)
                                    imports::lookup(&[
                                        {
                                            // (car export-spec*)
                                            imports::car(&[export_minus_spec_star___0.clone()])
                                        },
                                        env__25.clone(),
                                    ])
                                },
                                {
                                    // (car export-spec*)
                                    imports::car(&[export_minus_spec_star___0.clone()])
                                },
                            ])
                        },
                        {
                            // (astify-export (cdr export-spec*) env)
                            Scm::func(astify_minus_export).invoke(&[
                                {
                                    // (cdr export-spec*)
                                    imports::cdr(&[export_minus_spec_star___0.clone()])
                                },
                                env__25.clone(),
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
        let stmt_star___0 = args[0].clone();
        let env__26 = args[1].clone();
        if ({
            // (null? stmt*)
            imports::null_p(&[stmt_star___0.clone()])
        })
        .is_true()
        {
            Scm::Nil
        } else {
            {
                // (let ((libname (importset-libname (car stmt*)))) (cond ((equal? (quote (sunny testing)) libname) (astify-import (cdr stmt*) env)) ((importset-only? (car stmt*)) (cons (astify-import-only libname (importset-only-names (car stmt*)) env) (astify-import (cdr stmt*) env))) (else (cons (astify-import-all libname env) (astify-import (cdr stmt*) env)))))
                {
                    let libname__2 = {
                        // (importset-libname (car stmt*))
                        imports::importset_minus_libname(&[{
                            // (car stmt*)
                            imports::car(&[stmt_star___0.clone()])
                        }])
                    };
                    {
                        // (cond ...)
                        if ({
                            // (equal? (quote (sunny testing)) libname)
                            imports::equal_p(&[
                                Scm::pair(
                                    Scm::symbol("sunny"),
                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                ),
                                libname__2.clone(),
                            ])
                        })
                        .is_true()
                        {
                            {
                                // (astify-import (cdr stmt*) env)
                                Scm::func(astify_minus_import).invoke(&[
                                    {
                                        // (cdr stmt*)
                                        imports::cdr(&[stmt_star___0.clone()])
                                    },
                                    env__26.clone(),
                                ])
                            }
                        } else if ({
                            // (importset-only? (car stmt*))
                            imports::importset_minus_only_p(&[{
                                // (car stmt*)
                                imports::car(&[stmt_star___0.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (cons (astify-import-only libname (importset-only-names (car stmt*)) env) (astify-import (cdr stmt*) env))
                                imports::cons(&[
                                    {
                                        // (astify-import-only libname (importset-only-names (car stmt*)) env)
                                        Scm::func(astify_minus_import_minus_only).invoke(&[
                                            libname__2.clone(),
                                            {
                                                // (importset-only-names (car stmt*))
                                                imports::importset_minus_only_minus_names(&[{
                                                    // (car stmt*)
                                                    imports::car(&[stmt_star___0.clone()])
                                                }])
                                            },
                                            env__26.clone(),
                                        ])
                                    },
                                    {
                                        // (astify-import (cdr stmt*) env)
                                        Scm::func(astify_minus_import).invoke(&[
                                            {
                                                // (cdr stmt*)
                                                imports::cdr(&[stmt_star___0.clone()])
                                            },
                                            env__26.clone(),
                                        ])
                                    },
                                ])
                            }
                        } else {
                            {
                                // (cons (astify-import-all libname env) (astify-import (cdr stmt*) env))
                                imports::cons(&[
                                    {
                                        // (astify-import-all libname env)
                                        Scm::func(astify_minus_import_minus_all)
                                            .invoke(&[libname__2.clone(), env__26.clone()])
                                    },
                                    {
                                        // (astify-import (cdr stmt*) env)
                                        Scm::func(astify_minus_import).invoke(&[
                                            {
                                                // (cdr stmt*)
                                                imports::cdr(&[stmt_star___0.clone()])
                                            },
                                            env__26.clone(),
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
        let libname__3 = args[0].clone();
        let env__27 = args[1].clone();
        {
            {
                // (adjoin-import*! (library-exports (library-decls (get-lib libname))) env)
                imports::adjoin_minus_import_star__i(&[
                    {
                        // (library-exports (library-decls (get-lib libname)))
                        imports::library_minus_exports(&[{
                            // (library-decls (get-lib libname))
                            imports::library_minus_decls(&[{
                                // (get-lib libname)
                                imports::get_minus_lib(&[libname__3.clone()])
                            }])
                        }])
                    },
                    env__27.clone(),
                ])
            };
            {
                // (make-import libname)
                imports::make_minus_import(&[libname__3.clone()])
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
        let libname__4 = args[0].clone();
        let names__2 = args[1].clone();
        let env__28 = args[2].clone();
        {
            {
                // (check-imports names (library-exports (library-decls (get-lib libname))) libname)
                imports::check_minus_imports(&[
                    names__2.clone(),
                    {
                        // (library-exports (library-decls (get-lib libname)))
                        imports::library_minus_exports(&[{
                            // (library-decls (get-lib libname))
                            imports::library_minus_decls(&[{
                                // (get-lib libname)
                                imports::get_minus_lib(&[libname__4.clone()])
                            }])
                        }])
                    },
                    libname__4.clone(),
                ])
            };
            {
                // (adjoin-import*! names env)
                imports::adjoin_minus_import_star__i(&[names__2.clone(), env__28.clone()])
            };
            {
                // (make-import-only libname names)
                imports::make_minus_import_minus_only(&[libname__4.clone(), names__2.clone()])
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
        let exp_star___4 = args[0].clone();
        let env__29 = args[1].clone();
        let tail_p__7 = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? exp*)
                imports::null_p(&[exp_star___4.clone()])
            })
            .is_true()
            {
                {
                    // (error "empty sequence")
                    imports::error(&[Scm::from("empty sequence")])
                }
            } else if ({
                // (null? (cdr exp*))
                imports::null_p(&[{
                    // (cdr exp*)
                    imports::cdr(&[exp_star___4.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (astify (car exp*) env tail?)
                    astify(&[
                        {
                            // (car exp*)
                            imports::car(&[exp_star___4.clone()])
                        },
                        env__29.clone(),
                        tail_p__7.clone(),
                    ])
                }
            } else {
                {
                    // (let* ((first (astify (car exp*) env #f)) (rest (astify-sequence (cdr exp*) env tail?))) (make-sequence first rest))
                    {
                        // (let ((first (astify (car exp*) env #f))) (let ((rest (astify-sequence (cdr exp*) env tail?))) (begin (make-sequence first rest))))
                        {
                            let first__2 = {
                                // (astify (car exp*) env #f)
                                astify(&[
                                    {
                                        // (car exp*)
                                        imports::car(&[exp_star___4.clone()])
                                    },
                                    env__29.clone(),
                                    Scm::False,
                                ])
                            };
                            // (let ((rest (astify-sequence (cdr exp*) env tail?))) (begin (make-sequence first rest)))
                            let rest__0 = {
                                // (astify-sequence (cdr exp*) env tail?)
                                Scm::func(astify_minus_sequence).invoke(&[
                                    {
                                        // (cdr exp*)
                                        imports::cdr(&[exp_star___4.clone()])
                                    },
                                    env__29.clone(),
                                    tail_p__7.clone(),
                                ])
                            };
                            {
                                // (make-sequence first rest)
                                imports::make_minus_sequence(&[first__2.clone(), rest__0.clone()])
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
        let name__40 = args[0].clone();
        let env__30 = args[1].clone();
        {
            // (let ((var (ensure-var! name env))) (if (keyword? var) var (make-reference var)))
            {
                let var__25 = {
                    // (ensure-var! name env)
                    imports::ensure_minus_var_i(&[name__40.clone(), env__30.clone()])
                };
                if ({
                    // (keyword? var)
                    imports::keyword_p(&[var__25.clone()])
                })
                .is_true()
                {
                    var__25.clone()
                } else {
                    {
                        // (make-reference var)
                        imports::make_minus_reference(&[var__25.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn astify_minus_syntactic_minus_closure(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let sc__1 = args[0].clone();
        let env__34 = args[1].clone();
        let tail_p__9 = args[2].clone();
        {
            // ((get-field sc (quote closure)) env tail?)
            {
                // (get-field sc (quote closure))
                imports::get_minus_field(&[sc__1.clone(), Scm::symbol("closure")])
            }
            .invoke(&[env__34.clone(), tail_p__9.clone()])
        }
    }
    .into()
}
pub fn astify_minus_testcase(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let case__0 = args[0].clone();let env__32 = args[1].clone();{
// (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f))))
{
// (let ((given (quote *uninitialized*)) (when (quote *uninitialized*)) (then (quote *uninitialized*)) (dispatch (quote *uninitialized*))) (begin (set! given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (set! when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (set! then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (set! dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))) (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))))
{let [given__0, when__0, then__0, dispatch__0, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let dispatch__0 = dispatch__0.into_boxed();{let then__0 = then__0.into_boxed();{let when__0 = when__0.into_boxed();{let given__0 = given__0.into_boxed();{given__0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt__4 = args[0].clone();let body__16 = args[1].clone();{
// (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
imports::list(&[Scm::symbol("let*"),{
// (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
imports::map(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let assignment__0 = args[0].clone();{
// (list (car assignment) (caddr assignment))
imports::list(&[{
// (car assignment)
imports::car(&[assignment__0.clone()])},{
// (caddr assignment)
imports::caddr(&[assignment__0.clone()])}])}})},{
// (cdr stmt)
imports::cdr(&[stmt__4.clone()])}])},body__16.clone()])}})});Scm::anything();when__0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt__5 = args[0].clone();let body__17 = args[1].clone();{
// (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
{
// (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))) (loop (cdr stmt))))
{let loop__1 = Scm::symbol("*uninitialized*");{let loop__1 = loop__1.into_boxed();{loop__1.set({// Closure
let body__17 = body__17.clone();let loop__1 = loop__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let stmt_star___1 = args[0].clone();{
// (cond ...)
if ({
// (null? stmt*)
imports::null_p(&[stmt_star___1.clone()])}).is_true() {body__17.clone()} else if ({
// (eq? (quote <-) (cadar stmt*))
imports::eq_p(&[Scm::symbol("<-"),{
// (cadar stmt*)
imports::cadar(&[stmt_star___1.clone()])}])}).is_true() {{
// (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))
imports::list(&[Scm::symbol("let"),{
// (list (list (caar stmt*) (caddar stmt*)))
imports::list(&[{
// (list (caar stmt*) (caddar stmt*))
imports::list(&[{
// (caar stmt*)
imports::caar(&[stmt_star___1.clone()])},{
// (caddar stmt*)
imports::caddar(&[stmt_star___1.clone()])}])}])},{
// (loop (cdr stmt*))
loop__1.get().invoke(&[{
// (cdr stmt*)
imports::cdr(&[stmt_star___1.clone()])}])}])}} else {{
// (list (quote begin) (car stmt*) (loop (cdr stmt*)))
imports::list(&[Scm::symbol("begin"),{
// (car stmt*)
imports::car(&[stmt_star___1.clone()])},{
// (loop (cdr stmt*))
loop__1.get().invoke(&[{
// (cdr stmt*)
imports::cdr(&[stmt_star___1.clone()])}])}])}}}})});Scm::anything();{
// (loop (cdr stmt))
loop__1.get().invoke(&[{
// (cdr stmt)
imports::cdr(&[stmt__5.clone()])}])}}}}}}})});Scm::anything();then__0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let stmt__6 = args[0].clone();let body__18 = args[1].clone();{
// (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
imports::cons(&[Scm::symbol("begin"),{
// (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
imports::append(&[{
// (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
imports::map(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let pred__2 = args[0].clone();{
// (list (quote assert) pred)
imports::list(&[Scm::symbol("assert"),pred__2.clone()])}})},{
// (cdr stmt)
imports::cdr(&[stmt__6.clone()])}])},body__18.clone()])}])}})});Scm::anything();dispatch__0.set({// Closure
let given__0 = given__0.clone();let dispatch__0 = dispatch__0.clone();let when__0 = when__0.clone();let then__0 = then__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let section_star___0 = args[0].clone();let body__19 = args[1].clone();{
// (cond ...)
if ({
// (null? section*)
imports::null_p(&[section_star___0.clone()])}).is_true() {body__19.clone()} else if ({
// (eq? (quote given) (caar section*))
imports::eq_p(&[Scm::symbol("given"),{
// (caar section*)
imports::caar(&[section_star___0.clone()])}])}).is_true() {{
// (given (car section*) (dispatch (cdr section*) body))
given__0.get().invoke(&[{
// (car section*)
imports::car(&[section_star___0.clone()])},{
// (dispatch (cdr section*) body)
dispatch__0.get().invoke(&[{
// (cdr section*)
imports::cdr(&[section_star___0.clone()])},body__19.clone()])}])}} else if ({
// (eq? (quote when) (caar section*))
imports::eq_p(&[Scm::symbol("when"),{
// (caar section*)
imports::caar(&[section_star___0.clone()])}])}).is_true() {{
// (when (car section*) (dispatch (cdr section*) body))
when__0.get().invoke(&[{
// (car section*)
imports::car(&[section_star___0.clone()])},{
// (dispatch (cdr section*) body)
dispatch__0.get().invoke(&[{
// (cdr section*)
imports::cdr(&[section_star___0.clone()])},body__19.clone()])}])}} else if ({
// (eq? (quote then) (caar section*))
imports::eq_p(&[Scm::symbol("then"),{
// (caar section*)
imports::caar(&[section_star___0.clone()])}])}).is_true() {{
// (then (car section*) (dispatch (cdr section*) body))
then__0.get().invoke(&[{
// (car section*)
imports::car(&[section_star___0.clone()])},{
// (dispatch (cdr section*) body)
dispatch__0.get().invoke(&[{
// (cdr section*)
imports::cdr(&[section_star___0.clone()])},body__19.clone()])}])}} else {{
// (error "invalid testcase")
imports::error(&[Scm::from("invalid testcase")])}}}})});Scm::anything();{
// (let ((body (dispatch (testcase-body case) (quote ())))) (make-testcase (testcase-description case) (astify body env #f)))
{let body__20 = {
// (dispatch (testcase-body case) (quote ()))
dispatch__0.get().invoke(&[{
// (testcase-body case)
imports::testcase_minus_body(&[case__0.clone()])},Scm::Nil])};{
// (make-testcase (testcase-description case) (astify body env #f))
imports::make_minus_testcase(&[{
// (testcase-description case)
imports::testcase_minus_description(&[case__0.clone()])},{
// (astify body env #f)
astify(&[body__20.clone(),env__32.clone(),Scm::False])}])}}}}}}}}}}}}.into()
}
pub fn astify_minus_testsuite(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let name__41 = args[0].clone();
        let cases__1 = args[1].clone();
        let env__31 = args[2].clone();
        {
            // (make-testsuite name (map (lambda (c) (astify-testcase c env)) cases))
            imports::make_minus_testsuite(&[name__41.clone(), {
                // (map (lambda (c) (astify-testcase c env)) cases)
                imports::map(&[
                    {
                        // Closure
                        let env__31 = env__31.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let c__2 = args[0].clone();
                            {
                                // (astify-testcase c env)
                                Scm::func(astify_minus_testcase)
                                    .invoke(&[c__2.clone(), env__31.clone()])
                            }
                        })
                    },
                    cases__1.clone(),
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
            imports::make_minus_constant(&[Scm::symbol("*UNSPECIFIED*")])
        }
    }
    .into()
}
pub fn filter_minus_syntactic_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let names__3 = args[0].clone();
        let names_minus_env__0 = args[1].clone();
        let else_minus_env__0 = args[2].clone();
        if ({
            // (null? names)
            imports::null_p(&[names__3.clone()])
        })
        .is_true()
        {
            else_minus_env__0.clone()
        } else {
            {
                // (cons (env-find (car names) names-env) (filter-syntactic-env (cdr names) names-env else-env))
                imports::cons(&[
                    {
                        // (env-find (car names) names-env)
                        imports::env_minus_find(&[
                            {
                                // (car names)
                                imports::car(&[names__3.clone()])
                            },
                            names_minus_env__0.clone(),
                        ])
                    },
                    {
                        // (filter-syntactic-env (cdr names) names-env else-env)
                        Scm::func(filter_minus_syntactic_minus_env).invoke(&[
                            {
                                // (cdr names)
                                imports::cdr(&[names__3.clone()])
                            },
                            names_minus_env__0.clone(),
                            else_minus_env__0.clone(),
                        ])
                    },
                ])
            }
        }
    }
    .into()
}
pub fn make_minus_syntactic_minus_closure(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let env__33 = args[0].clone();
        let free_minus_names__0 = args[1].clone();
        let exp__3 = args[2].clone();
        {
            // (let ((sc (clone SyntacticClosure))) (set-field! sc (quote closure) (lambda (free-names-env tail?) (astify exp (filter-syntactic-env free-names free-names-env env) tail?))) sc)
            {
                let sc__0 = {
                    // (clone SyntacticClosure)
                    imports::clone(&[SyntacticClosure.with(|value| value.get())])
                };
                {
                    {
                        // (set-field! sc (quote closure) (lambda (free-names-env tail?) (astify exp (filter-syntactic-env free-names free-names-env env) tail?)))
                        imports::set_minus_field_i(&[sc__0.clone(), Scm::symbol("closure"), {
                            // Closure
                            let exp__3 = exp__3.clone();
                            let free_minus_names__0 = free_minus_names__0.clone();
                            let env__33 = env__33.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let free_minus_names_minus_env__0 = args[0].clone();
                                let tail_p__8 = args[1].clone();
                                {
                                    // (astify exp (filter-syntactic-env free-names free-names-env env) tail?)
                                    astify(&[
                                        exp__3.clone(),
                                        {
                                            // (filter-syntactic-env free-names free-names-env env)
                                            Scm::func(filter_minus_syntactic_minus_env).invoke(&[
                                                free_minus_names__0.clone(),
                                                free_minus_names_minus_env__0.clone(),
                                                env__33.clone(),
                                            ])
                                        },
                                        tail_p__8.clone(),
                                    ])
                                }
                            })
                        }])
                    };
                    sc__0.clone()
                }
            }
        }
    }
    .into()
}
pub fn syntactic_minus_closure_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__17 = args[0].clone();
        {
            // (and (table? obj) (ancestor? obj SyntacticClosure))
            if ({
                // (table? obj)
                imports::table_p(&[obj__17.clone()])
            })
            .is_true()
            {
                {
                    // (ancestor? obj SyntacticClosure)
                    imports::ancestor_p(&[
                        obj__17.clone(),
                        SyntacticClosure.with(|value| value.get()),
                    ])
                }
            } else {
                Scm::False
            }
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
    crate::sunny::table::initialize();
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
        };
        {
            // (define SyntacticClosure (make-table))
            SyntacticClosure.with(|value| {
                value.set({
                    // (make-table)
                    imports::make_minus_table(&[])
                })
            });
            Scm::anything()
        };
        {
            // (set-field! SyntacticClosure (quote __name__) (quote SyntacticClosure))
            imports::set_minus_field_i(&[
                SyntacticClosure.with(|value| value.get()),
                Scm::symbol("__name__"),
                Scm::symbol("SyntacticClosure"),
            ])
        };
        {
            // (define (make-syntactic-closure env free-names exp) ...)
            (/*NOP*/)
        };
        {
            // (define (syntactic-closure? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-syntactic-closure sc env tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (filter-syntactic-env names names-env else-env) ...)
            (/*NOP*/)
        }
    };
}
