#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::adjoin_minus_boxed_minus_env;
    pub use super::adjoin_minus_import_i;
    pub use super::adjoin_minus_import_star__i;
    pub use super::adjoin_minus_local_minus_env;
    pub use super::ensure_minus_var_i;
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (cons (new-boxed name) env)
            Scm::func(imports::cons).invoke(&[
                {
                    // (new-boxed name)
                    Scm::func(imports::new_minus_boxed).invoke(&[name.clone()])
                },
                env.clone(),
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
        let name_star_ = args[0].clone();
        let env = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                Scm::func(imports::null_p).invoke(&[name_star_.clone()])
            })
            .is_true()
            {
                env.clone()
            } else if ({
                // (pair? name*)
                Scm::func(imports::pair_p).invoke(&[name_star_.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))
                    Scm::func(adjoin_minus_boxed_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            Scm::func(imports::cdr).invoke(&[name_star_.clone()])
                        },
                        {
                            // (adjoin-boxed (car name*) env)
                            Scm::func(adjoin_minus_boxed).invoke(&[
                                {
                                    // (car name*)
                                    Scm::func(imports::car).invoke(&[name_star_.clone()])
                                },
                                env.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-boxed name* env)
                    Scm::func(adjoin_minus_boxed).invoke(&[name_star_.clone(), env.clone()])
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (adjoin-global-var! (new-global name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-global name)
                    Scm::func(imports::new_minus_global).invoke(&[name.clone()])
                },
                env.clone(),
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
        let var = args[0].clone();
        let env = args[1].clone();
        {
            // (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) (cdr var))
            {
                let genv = {
                    // (find-globals env)
                    Scm::func(find_minus_globals).invoke(&[env.clone()])
                };
                {
                    {
                        // (set-cdr! genv (cons var (cdr genv)))
                        Scm::func(imports::set_minus_cdr_i).invoke(&[genv.clone(), {
                            // (cons var (cdr genv))
                            Scm::func(imports::cons).invoke(&[var.clone(), {
                                // (cdr genv)
                                Scm::func(imports::cdr).invoke(&[genv.clone()])
                            }])
                        }])
                    };
                    {
                        // (cdr var)
                        Scm::func(imports::cdr).invoke(&[var.clone()])
                    }
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (adjoin-global-var! (new-import name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-import name)
                    Scm::func(imports::new_minus_import).invoke(&[name.clone()])
                },
                env.clone(),
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
        let name_star_ = args[0].clone();
        let env = args[1].clone();
        {
            // (letrec ((loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))))) (loop name* (find-globals env)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv))))) (loop name* (find-globals env))))
                {
                    let loop_ = Scm::symbol("*uninitialized*");
                    {
                        let loop_ = loop_.into_boxed();
                        {
                            loop_.set({
                                // Closure
                                let loop_ = loop_.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let name_star_ = args[0].clone();
                                    let genv = args[1].clone();
                                    if ({
                                        // (null? name*)
                                        Scm::func(imports::null_p).invoke(&[name_star_.clone()])
                                    })
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else {
                                        {
                                            {
                                                // (set-cdr! genv (cons (new-import (car name*)) (cdr genv)))
                                                Scm::func(imports::set_minus_cdr_i).invoke(&[
                                                    genv.clone(),
                                                    {
                                                        // (cons (new-import (car name*)) (cdr genv))
                                                        Scm::func(imports::cons).invoke(&[
                                                            {
                                                                // (new-import (car name*))
                                                                Scm::func(imports::new_minus_import)
                                                                    .invoke(&[{
                                                                        // (car name*)
                                                                        Scm::func(imports::car)
                                                                            .invoke(&[
                                                                                name_star_.clone()
                                                                            ])
                                                                    }])
                                                            },
                                                            {
                                                                // (cdr genv)
                                                                Scm::func(imports::cdr)
                                                                    .invoke(&[genv.clone()])
                                                            },
                                                        ])
                                                    },
                                                ])
                                            };
                                            {
                                                // (loop (cdr name*) genv)
                                                loop_.get().invoke(&[
                                                    {
                                                        // (cdr name*)
                                                        Scm::func(imports::cdr)
                                                            .invoke(&[name_star_.clone()])
                                                    },
                                                    genv.clone(),
                                                ])
                                            }
                                        }
                                    }
                                })
                            });
                            {
                                // (loop name* (find-globals env))
                                loop_.get().invoke(&[name_star_.clone(), {
                                    // (find-globals env)
                                    Scm::func(find_minus_globals).invoke(&[env.clone()])
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (cons (new-local name) env)
            Scm::func(imports::cons).invoke(&[
                {
                    // (new-local name)
                    Scm::func(imports::new_minus_local).invoke(&[name.clone()])
                },
                env.clone(),
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
        let name_star_ = args[0].clone();
        let env = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                Scm::func(imports::null_p).invoke(&[name_star_.clone()])
            })
            .is_true()
            {
                env.clone()
            } else if ({
                // (pair? name*)
                Scm::func(imports::pair_p).invoke(&[name_star_.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))
                    Scm::func(adjoin_minus_local_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            Scm::func(imports::cdr).invoke(&[name_star_.clone()])
                        },
                        {
                            // (adjoin-local (car name*) env)
                            Scm::func(adjoin_minus_local).invoke(&[
                                {
                                    // (car name*)
                                    Scm::func(imports::car).invoke(&[name_star_.clone()])
                                },
                                env.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-local name* env)
                    Scm::func(adjoin_minus_local).invoke(&[name_star_.clone(), env.clone()])
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (let ((var (lookup name env))) (if var var (adjoin-global! name env)))
            {
                let var = {
                    // (lookup name env)
                    Scm::func(lookup).invoke(&[name.clone(), env.clone()])
                };
                if (var.clone()).is_true() {
                    var.clone()
                } else {
                    {
                        // (adjoin-global! name env)
                        Scm::func(adjoin_minus_global_i).invoke(&[name.clone(), env.clone()])
                    }
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
        let func = args[0].clone();
        let env = args[1].clone();
        {
            // (for-each (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            Scm::func(imports::for_minus_each).invoke(&[
                {
                    // Closure
                    let func = func.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            Scm::func(imports::eq_p)
                                .invoke(&[Scm::symbol("GLOBAL-MARKER"), entry.clone()])
                        })
                        .is_true()
                        {
                            entry.clone()
                        } else {
                            {
                                // (func entry)
                                func.clone().invoke(&[entry.clone()])
                            }
                        }
                    })
                },
                env.clone(),
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
        let env = args[0].clone();
        if ({
            // (eq? (quote GLOBAL-MARKER) (car env))
            Scm::func(imports::eq_p).invoke(&[Scm::symbol("GLOBAL-MARKER"), {
                // (car env)
                Scm::func(imports::car).invoke(&[env.clone()])
            }])
        })
        .is_true()
        {
            env.clone()
        } else {
            {
                // (find-globals (cdr env))
                Scm::func(find_minus_globals).invoke(&[{
                    // (cdr env)
                    Scm::func(imports::cdr).invoke(&[env.clone()])
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
        let name = args[0].clone();
        let env = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? env)
                Scm::func(imports::null_p).invoke(&[env.clone()])
            })
            .is_true()
            {
                Scm::False
            } else if ({
                // (eq? (quote GLOBAL-MARKER) (car env))
                Scm::func(imports::eq_p).invoke(&[Scm::symbol("GLOBAL-MARKER"), {
                    // (car env)
                    Scm::func(imports::car).invoke(&[env.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (lookup name (cdr env))
                    Scm::func(lookup).invoke(&[name.clone(), {
                        // (cdr env)
                        Scm::func(imports::cdr).invoke(&[env.clone()])
                    }])
                }
            } else if ({
                // (eq? name (caar env))
                Scm::func(imports::eq_p).invoke(&[name.clone(), {
                    // (caar env)
                    Scm::func(imports::caar).invoke(&[env.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (cdar env)
                    Scm::func(imports::cdar).invoke(&[env.clone()])
                }
            } else {
                {
                    // (lookup name (cdr env))
                    Scm::func(lookup).invoke(&[name.clone(), {
                        // (cdr env)
                        Scm::func(imports::cdr).invoke(&[env.clone()])
                    }])
                }
            }
        }
    }
    .into()
}
pub fn lookup_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star_ = args[0].clone();
        let env = args[1].clone();
        {
            // (map (lambda (name) (lookup name env)) name*)
            Scm::func(imports::map).invoke(&[
                {
                    // Closure
                    let env = env.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        {
                            // (lookup name env)
                            Scm::func(lookup).invoke(&[name.clone(), env.clone()])
                        }
                    })
                },
                name_star_.clone(),
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
            Scm::func(imports::list).invoke(&[
                Scm::symbol("GLOBAL-MARKER"),
                {
                    // (new-import (quote assert-eq))
                    Scm::func(imports::new_minus_import).invoke(&[Scm::symbol("assert-eq")])
                },
                {
                    // (new-import (quote assert-equal))
                    Scm::func(imports::new_minus_import).invoke(&[Scm::symbol("assert-equal")])
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
        let func = args[0].clone();
        let env = args[1].clone();
        {
            // (map (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            Scm::func(imports::map).invoke(&[
                {
                    // Closure
                    let func = func.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            Scm::func(imports::eq_p)
                                .invoke(&[Scm::symbol("GLOBAL-MARKER"), entry.clone()])
                        })
                        .is_true()
                        {
                            entry.clone()
                        } else {
                            {
                                // (func entry)
                                func.clone().invoke(&[entry.clone()])
                            }
                        }
                    })
                },
                env.clone(),
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
