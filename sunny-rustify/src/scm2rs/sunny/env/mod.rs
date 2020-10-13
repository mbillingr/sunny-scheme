#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::adjoin_minus_boxed_minus_env;
    pub use super::adjoin_minus_import_i;
    pub use super::adjoin_minus_import_star__i;
    pub use super::adjoin_minus_local_minus_env;
    pub use super::ensure_minus_var_i;
    pub use super::env_minus_find;
    pub use super::env_minus_for_minus_each;
    pub use super::lookup;
    pub use super::lookup_star_;
    pub use super::make_minus_global_minus_env;
    pub use super::map_minus_env;
}

pub fn adjoin_minus_boxed(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__19 = args[0].clone();
        let env__11 = args[1].clone();
        {
            // (cons (new-boxed name) env)
            imports::cons(&[
                {
                    // (new-boxed name)
                    imports::new_minus_boxed(&[name__19.clone()])
                },
                env__11.clone(),
            ])
        }
    }
    .into()
}
pub fn adjoin_minus_boxed_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star___4 = args[0].clone();
        let env__12 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                imports::null_p(&[name_star___4.clone()])
            })
            .is_true()
            {
                env__12.clone()
            } else if ({
                // (pair? name*)
                imports::pair_p(&[name_star___4.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))
                    Scm::func(adjoin_minus_boxed_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            imports::cdr(&[name_star___4.clone()])
                        },
                        {
                            // (adjoin-boxed (car name*) env)
                            adjoin_minus_boxed(&[
                                {
                                    // (car name*)
                                    imports::car(&[name_star___4.clone()])
                                },
                                env__12.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-boxed name* env)
                    adjoin_minus_boxed(&[name_star___4.clone(), env__12.clone()])
                }
            }
        }
    }
    .into()
}
pub fn adjoin_minus_global_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__16 = args[0].clone();
        let env__5 = args[1].clone();
        {
            // (adjoin-global-var! (new-global name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-global name)
                    imports::new_minus_global(&[name__16.clone()])
                },
                env__5.clone(),
            ])
        }
    }
    .into()
}
pub fn adjoin_minus_global_minus_var_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let var__14 = args[0].clone();
        let env__7 = args[1].clone();
        {
            // (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) var)
            {
                let genv__0 = {
                    // (find-globals env)
                    find_minus_globals(&[env__7.clone()])
                };
                {
                    {
                        // (set-cdr! genv (cons var (cdr genv)))
                        imports::set_minus_cdr_i(&[genv__0.clone(), {
                            // (cons var (cdr genv))
                            imports::cons(&[var__14.clone(), {
                                // (cdr genv)
                                imports::cdr(&[genv__0.clone()])
                            }])
                        }])
                    };
                    var__14.clone()
                }
            }
        }
    }
    .into()
}
pub fn adjoin_minus_import_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__17 = args[0].clone();
        let env__6 = args[1].clone();
        {
            // (adjoin-global-var! (new-import name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-import name)
                    imports::new_minus_import(&[name__17.clone()])
                },
                env__6.clone(),
            ])
        }
    }
    .into()
}
pub fn adjoin_minus_import_star__i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star___3 = args[0].clone();
        let env__10 = args[1].clone();
        {
            // (letrec ((loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))))) (loop name* (find-globals env)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv))))) (loop name* (find-globals env))))
                {
                    let loop__0 = Scm::symbol("*uninitialized*");
                    {
                        let loop__0 = loop__0.into_boxed();
                        {
                            loop__0.set({
                                // Closure
                                let loop__0 = loop__0.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let name_star___2 = args[0].clone();
                                    let genv__1 = args[1].clone();
                                    if ({
                                        // (null? name*)
                                        imports::null_p(&[name_star___2.clone()])
                                    })
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else {
                                        {
                                            {
                                                // (set-cdr! genv (cons (new-import (car name*)) (cdr genv)))
                                                imports::set_minus_cdr_i(&[genv__1.clone(), {
                                                    // (cons (new-import (car name*)) (cdr genv))
                                                    imports::cons(&[
                                                        {
                                                            // (new-import (car name*))
                                                            imports::new_minus_import(&[{
                                                                // (car name*)
                                                                imports::car(&[
                                                                    name_star___2.clone()
                                                                ])
                                                            }])
                                                        },
                                                        {
                                                            // (cdr genv)
                                                            imports::cdr(&[genv__1.clone()])
                                                        },
                                                    ])
                                                }])
                                            };
                                            {
                                                // (loop (cdr name*) genv)
                                                loop__0.get().invoke(&[
                                                    {
                                                        // (cdr name*)
                                                        imports::cdr(&[name_star___2.clone()])
                                                    },
                                                    genv__1.clone(),
                                                ])
                                            }
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (loop name* (find-globals env))
                                loop__0.get().invoke(&[name_star___3.clone(), {
                                    // (find-globals env)
                                    find_minus_globals(&[env__10.clone()])
                                }])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn adjoin_minus_local(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__18 = args[0].clone();
        let env__8 = args[1].clone();
        {
            // (cons (new-local name) env)
            imports::cons(&[
                {
                    // (new-local name)
                    imports::new_minus_local(&[name__18.clone()])
                },
                env__8.clone(),
            ])
        }
    }
    .into()
}
pub fn adjoin_minus_local_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star___1 = args[0].clone();
        let env__9 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                imports::null_p(&[name_star___1.clone()])
            })
            .is_true()
            {
                env__9.clone()
            } else if ({
                // (pair? name*)
                imports::pair_p(&[name_star___1.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))
                    Scm::func(adjoin_minus_local_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            imports::cdr(&[name_star___1.clone()])
                        },
                        {
                            // (adjoin-local (car name*) env)
                            adjoin_minus_local(&[
                                {
                                    // (car name*)
                                    imports::car(&[name_star___1.clone()])
                                },
                                env__9.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-local name* env)
                    adjoin_minus_local(&[name_star___1.clone(), env__9.clone()])
                }
            }
        }
    }
    .into()
}
pub fn ensure_minus_var_i(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__12 = args[0].clone();
        let env__0 = args[1].clone();
        {
            // (let ((var (lookup name env))) (if var var (adjoin-global! name env)))
            {
                let var__13 = {
                    // (lookup name env)
                    Scm::func(lookup).invoke(&[name__12.clone(), env__0.clone()])
                };
                if (var__13.clone()).is_true() {
                    var__13.clone()
                } else {
                    {
                        // (adjoin-global! name env)
                        Scm::func(adjoin_minus_global_i).invoke(&[name__12.clone(), env__0.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn env_minus_find(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__15 = args[0].clone();
        let env__3 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? env)
                imports::null_p(&[env__3.clone()])
            })
            .is_true()
            {
                Scm::False
            } else if ({
                // (eq? (quote GLOBAL-MARKER) (car env))
                imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), {
                    // (car env)
                    imports::car(&[env__3.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (env-find name (cdr env))
                    Scm::func(env_minus_find).invoke(&[name__15.clone(), {
                        // (cdr env)
                        imports::cdr(&[env__3.clone()])
                    }])
                }
            } else if ({
                // (same-name? name (variable-name (car env)))
                imports::same_minus_name_p(&[name__15.clone(), {
                    // (variable-name (car env))
                    imports::variable_minus_name(&[{
                        // (car env)
                        imports::car(&[env__3.clone()])
                    }])
                }])
            })
            .is_true()
            {
                {
                    // (car env)
                    imports::car(&[env__3.clone()])
                }
            } else {
                {
                    // (env-find name (cdr env))
                    Scm::func(env_minus_find).invoke(&[name__15.clone(), {
                        // (cdr env)
                        imports::cdr(&[env__3.clone()])
                    }])
                }
            }
        }
    }
    .into()
}
pub fn env_minus_for_minus_each(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let func__4 = args[0].clone();
        let env__14 = args[1].clone();
        {
            // (for-each (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            imports::for_minus_each(&[
                {
                    // Closure
                    let func__4 = func__4.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry__3 = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), entry__3.clone()])
                        })
                        .is_true()
                        {
                            entry__3.clone()
                        } else {
                            {
                                // (func entry)
                                func__4.clone().invoke(&[entry__3.clone()])
                            }
                        }
                    })
                },
                env__14.clone(),
            ])
        }
    }
    .into()
}
pub fn find_minus_globals(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let env__4 = args[0].clone();
        if ({
            // (eq? (quote GLOBAL-MARKER) (car env))
            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), {
                // (car env)
                imports::car(&[env__4.clone()])
            }])
        })
        .is_true()
        {
            env__4.clone()
        } else {
            {
                // (find-globals (cdr env))
                Scm::func(find_minus_globals).invoke(&[{
                    // (cdr env)
                    imports::cdr(&[env__4.clone()])
                }])
            }
        }
    }
    .into()
}
pub fn lookup(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name__14 = args[0].clone();
        let env__2 = args[1].clone();
        {
            // (env-find name env)
            Scm::func(env_minus_find).invoke(&[name__14.clone(), env__2.clone()])
        }
    }
    .into()
}
pub fn lookup_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star___0 = args[0].clone();
        let env__1 = args[1].clone();
        {
            // (map (lambda (name) (lookup name env)) name*)
            imports::map(&[
                {
                    // Closure
                    let env__1 = env__1.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let name__13 = args[0].clone();
                        {
                            // (lookup name env)
                            Scm::func(lookup).invoke(&[name__13.clone(), env__1.clone()])
                        }
                    })
                },
                name_star___0.clone(),
            ])
        }
    }
    .into()
}
pub fn make_minus_global_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (list (quote GLOBAL-MARKER) (new-import (quote assert-eq)) (new-import (quote assert-equal)))
            imports::list(&[
                Scm::symbol("GLOBAL-MARKER"),
                {
                    // (new-import (quote assert-eq))
                    imports::new_minus_import(&[Scm::symbol("assert-eq")])
                },
                {
                    // (new-import (quote assert-equal))
                    imports::new_minus_import(&[Scm::symbol("assert-equal")])
                },
            ])
        }
    }
    .into()
}
pub fn map_minus_env(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let func__3 = args[0].clone();
        let env__13 = args[1].clone();
        {
            // (map (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            imports::map(&[
                {
                    // Closure
                    let func__3 = func__3.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry__2 = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), entry__2.clone()])
                        })
                        .is_true()
                        {
                            entry__2.clone()
                        } else {
                            {
                                // (func entry)
                                func__3.clone().invoke(&[entry__2.clone()])
                            }
                        }
                    })
                },
                env__13.clone(),
            ])
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
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (make-global-env) ...)
            (/*NOP*/)
        };
        {
            // (define (ensure-var! name env) ...)
            (/*NOP*/)
        };
        {
            // (define (lookup* name* env) ...)
            (/*NOP*/)
        };
        {
            // (define (lookup name env) ...)
            (/*NOP*/)
        };
        {
            // (define (env-find name env) ...)
            (/*NOP*/)
        };
        {
            // (define (find-globals env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-global! name env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-import! name env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-global-var! var env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-local name env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-local-env name* env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-import*! name* env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-boxed name env) ...)
            (/*NOP*/)
        };
        {
            // (define (adjoin-boxed-env name* env) ...)
            (/*NOP*/)
        };
        {
            // (define (map-env func env) ...)
            (/*NOP*/)
        };
        {
            // (define (env-for-each func env) ...)
            (/*NOP*/)
        }
    };
}
