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
        let name__233 = args[0].clone();
        let env__232 = args[1].clone();
        {
            // (cons (new-boxed name) env)
            imports::cons(&[
                {
                    // (new-boxed name)
                    imports::new_minus_boxed(&[name__233.clone()])
                },
                env__232.clone(),
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
        let name_star___235 = args[0].clone();
        let env__234 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                imports::null_p(&[name_star___235.clone()])
            })
            .is_true()
            {
                env__234.clone()
            } else if ({
                // (pair? name*)
                imports::pair_p(&[name_star___235.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))
                    Scm::func(adjoin_minus_boxed_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            imports::cdr(&[name_star___235.clone()])
                        },
                        {
                            // (adjoin-boxed (car name*) env)
                            adjoin_minus_boxed(&[
                                {
                                    // (car name*)
                                    imports::car(&[name_star___235.clone()])
                                },
                                env__234.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-boxed name* env)
                    adjoin_minus_boxed(&[name_star___235.clone(), env__234.clone()])
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
        let name__217 = args[0].clone();
        let env__216 = args[1].clone();
        {
            // (adjoin-global-var! (new-global name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-global name)
                    imports::new_minus_global(&[name__217.clone()])
                },
                env__216.clone(),
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
        let var__221 = args[0].clone();
        let env__222 = args[1].clone();
        {
            // (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) var)
            {
                let genv__220 = {
                    // (find-globals env)
                    find_minus_globals(&[env__222.clone()])
                };
                {
                    {
                        // (set-cdr! genv (cons var (cdr genv)))
                        imports::set_minus_cdr_i(&[genv__220.clone(), {
                            // (cons var (cdr genv))
                            imports::cons(&[var__221.clone(), {
                                // (cdr genv)
                                imports::cdr(&[genv__220.clone()])
                            }])
                        }])
                    };
                    var__221.clone()
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
        let name__219 = args[0].clone();
        let env__218 = args[1].clone();
        {
            // (adjoin-global-var! (new-import name) env)
            Scm::func(adjoin_minus_global_minus_var_i).invoke(&[
                {
                    // (new-import name)
                    imports::new_minus_import(&[name__219.clone()])
                },
                env__218.clone(),
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
        let name_star___231 = args[0].clone();
        let env__230 = args[1].clone();
        {
            // (letrec ((loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))))) (loop name* (find-globals env)))
            {
                // (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv))))) (loop name* (find-globals env))))
                {
                    let loop__227 = Scm::symbol("*uninitialized*");
                    {
                        let loop__227 = loop__227.into_boxed();
                        {
                            loop__227.set({
                                // Closure
                                let loop__227 = loop__227.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let name_star___229 = args[0].clone();
                                    let genv__228 = args[1].clone();
                                    if ({
                                        // (null? name*)
                                        imports::null_p(&[name_star___229.clone()])
                                    })
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else {
                                        {
                                            {
                                                // (set-cdr! genv (cons (new-import (car name*)) (cdr genv)))
                                                imports::set_minus_cdr_i(&[genv__228.clone(), {
                                                    // (cons (new-import (car name*)) (cdr genv))
                                                    imports::cons(&[
                                                        {
                                                            // (new-import (car name*))
                                                            imports::new_minus_import(&[{
                                                                // (car name*)
                                                                imports::car(&[
                                                                    name_star___229.clone()
                                                                ])
                                                            }])
                                                        },
                                                        {
                                                            // (cdr genv)
                                                            imports::cdr(&[genv__228.clone()])
                                                        },
                                                    ])
                                                }])
                                            };
                                            {
                                                // (loop (cdr name*) genv)
                                                loop__227.get().invoke(&[
                                                    {
                                                        // (cdr name*)
                                                        imports::cdr(&[name_star___229.clone()])
                                                    },
                                                    genv__228.clone(),
                                                ])
                                            }
                                        }
                                    }
                                })
                            });
                            Scm::anything();
                            {
                                // (loop name* (find-globals env))
                                loop__227.get().invoke(&[name_star___231.clone(), {
                                    // (find-globals env)
                                    find_minus_globals(&[env__230.clone()])
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
        let name__224 = args[0].clone();
        let env__223 = args[1].clone();
        {
            // (cons (new-local name) env)
            imports::cons(&[
                {
                    // (new-local name)
                    imports::new_minus_local(&[name__224.clone()])
                },
                env__223.clone(),
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
        let name_star___226 = args[0].clone();
        let env__225 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? name*)
                imports::null_p(&[name_star___226.clone()])
            })
            .is_true()
            {
                env__225.clone()
            } else if ({
                // (pair? name*)
                imports::pair_p(&[name_star___226.clone()])
            })
            .is_true()
            {
                {
                    // (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))
                    Scm::func(adjoin_minus_local_minus_env).invoke(&[
                        {
                            // (cdr name*)
                            imports::cdr(&[name_star___226.clone()])
                        },
                        {
                            // (adjoin-local (car name*) env)
                            adjoin_minus_local(&[
                                {
                                    // (car name*)
                                    imports::car(&[name_star___226.clone()])
                                },
                                env__225.clone(),
                            ])
                        },
                    ])
                }
            } else {
                {
                    // (adjoin-local name* env)
                    adjoin_minus_local(&[name_star___226.clone(), env__225.clone()])
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
        let name__206 = args[0].clone();
        let env__205 = args[1].clone();
        {
            // (let ((var (lookup name env))) (if var var (adjoin-global! name env)))
            {
                let var__207 = {
                    // (lookup name env)
                    Scm::func(lookup).invoke(&[name__206.clone(), env__205.clone()])
                };
                if (var__207.clone()).is_true() {
                    var__207.clone()
                } else {
                    {
                        // (adjoin-global! name env)
                        Scm::func(adjoin_minus_global_i)
                            .invoke(&[name__206.clone(), env__205.clone()])
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
        let name__214 = args[0].clone();
        let env__213 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? env)
                imports::null_p(&[env__213.clone()])
            })
            .is_true()
            {
                Scm::False
            } else if ({
                // (eq? (quote GLOBAL-MARKER) (car env))
                imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), {
                    // (car env)
                    imports::car(&[env__213.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (env-find name (cdr env))
                    Scm::func(env_minus_find).invoke(&[name__214.clone(), {
                        // (cdr env)
                        imports::cdr(&[env__213.clone()])
                    }])
                }
            } else if ({
                // (same-name? name (variable-name (car env)))
                imports::same_minus_name_p(&[name__214.clone(), {
                    // (variable-name (car env))
                    imports::variable_minus_name(&[{
                        // (car env)
                        imports::car(&[env__213.clone()])
                    }])
                }])
            })
            .is_true()
            {
                {
                    // (car env)
                    imports::car(&[env__213.clone()])
                }
            } else {
                {
                    // (env-find name (cdr env))
                    Scm::func(env_minus_find).invoke(&[name__214.clone(), {
                        // (cdr env)
                        imports::cdr(&[env__213.clone()])
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
        let func__241 = args[0].clone();
        let env__239 = args[1].clone();
        {
            // (for-each (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            imports::for_minus_each(&[
                {
                    // Closure
                    let func__241 = func__241.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry__240 = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), entry__240.clone()])
                        })
                        .is_true()
                        {
                            entry__240.clone()
                        } else {
                            {
                                // (func entry)
                                func__241.clone().invoke(&[entry__240.clone()])
                            }
                        }
                    })
                },
                env__239.clone(),
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
        let env__215 = args[0].clone();
        if ({
            // (eq? (quote GLOBAL-MARKER) (car env))
            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), {
                // (car env)
                imports::car(&[env__215.clone()])
            }])
        })
        .is_true()
        {
            env__215.clone()
        } else {
            {
                // (find-globals (cdr env))
                Scm::func(find_minus_globals).invoke(&[{
                    // (cdr env)
                    imports::cdr(&[env__215.clone()])
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
        let name__212 = args[0].clone();
        let env__211 = args[1].clone();
        {
            // (env-find name env)
            Scm::func(env_minus_find).invoke(&[name__212.clone(), env__211.clone()])
        }
    }
    .into()
}
pub fn lookup_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_star___208 = args[0].clone();
        let env__209 = args[1].clone();
        {
            // (map (lambda (name) (lookup name env)) name*)
            imports::map(&[
                {
                    // Closure
                    let env__209 = env__209.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let name__210 = args[0].clone();
                        {
                            // (lookup name env)
                            Scm::func(lookup).invoke(&[name__210.clone(), env__209.clone()])
                        }
                    })
                },
                name_star___208.clone(),
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
        let func__238 = args[0].clone();
        let env__236 = args[1].clone();
        {
            // (map (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
            imports::map(&[
                {
                    // Closure
                    let func__238 = func__238.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let entry__237 = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) entry)
                            imports::eq_p(&[Scm::symbol("GLOBAL-MARKER"), entry__237.clone()])
                        })
                        .is_true()
                        {
                            entry__237.clone()
                        } else {
                            {
                                // (func entry)
                                func__238.clone().invoke(&[entry__237.clone()])
                            }
                        }
                    })
                },
                env__236.clone(),
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
