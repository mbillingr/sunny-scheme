#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::adjoin_minus_boxed_minus_env;
    pub use super::globals::adjoin_minus_import_i;
    pub use super::globals::adjoin_minus_import_star__i;
    pub use super::globals::adjoin_minus_local_minus_env;
    pub use super::globals::ensure_minus_var_i;
    pub use super::globals::env_minus_for_minus_each;
    pub use super::globals::lookup;
    pub use super::globals::lookup_star_;
    pub use super::globals::make_minus_global_minus_env;
    pub use super::globals::map_minus_env;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_boxed: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-boxed"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_boxed_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-boxed-env"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_global_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-global!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_global_minus_var_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-global-var!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_import_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-import!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_import_star__i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-import*!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_local: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-local"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_local_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-local-env"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static ensure_minus_var_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ensure-var!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static env_minus_for_minus_each: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL env-for-each"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_globals: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-globals"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static lookup: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL lookup"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static lookup_star_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL lookup*"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_global_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-global-env"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static map_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL map-env"))}
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
            globals::make_minus_global_minus_env.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 0 {
                            panic!("invalid arity")
                        }
                        {
                            // (list (quote GLOBAL-MARKER) (new-import (quote assert-eq)) (new-import (quote assert-equal)))
                            imports::list.with(|value| value.get()).invoke(&[
                                Scm::symbol("GLOBAL-MARKER"),
                                {
                                    // (new-import (quote assert-eq))
                                    imports::new_minus_import
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("assert-eq")])
                                },
                                {
                                    // (new-import (quote assert-equal))
                                    imports::new_minus_import
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("assert-equal")])
                                },
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (ensure-var! name env) ...)
            globals::ensure_minus_var_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
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
                                    globals::lookup
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), env.clone()])
                                };
                                if (var.clone()).is_true() {
                                    var.clone()
                                } else {
                                    {
                                        // (adjoin-global! name env)
                                        globals::adjoin_minus_global_i
                                            .with(|value| value.get())
                                            .invoke(&[name.clone(), env.clone()])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (lookup* name* env) ...)
            globals::lookup_star_.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (map (lambda (name) (lookup name env)) name*)
                            imports::map.with(|value| value.get()).invoke(&[
                                {
                                    let env = env.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let name = args[0].clone();
                                        {
                                            // (lookup name env)
                                            globals::lookup
                                                .with(|value| value.get())
                                                .invoke(&[name.clone(), env.clone()])
                                        }
                                    })
                                },
                                name_star_.clone(),
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (lookup name env) ...)
            globals::lookup.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? env)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()])
                            })
                            .is_true()
                            {
                                Scm::False
                            } else if ({
                                // (eq? (quote GLOBAL-MARKER) (car env))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("GLOBAL-MARKER"),
                                    {
                                        // (car env)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[env.clone()])
                                    },
                                ])
                            })
                            .is_true()
                            {
                                {
                                    // (lookup name (cdr env))
                                    globals::lookup.with(|value| value.get()).invoke(&[
                                        name.clone(),
                                        {
                                            // (cdr env)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[env.clone()])
                                        },
                                    ])
                                }
                            } else if ({
                                // (eq? name (caar env))
                                imports::eq_p
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), {
                                        // (caar env)
                                        imports::caar
                                            .with(|value| value.get())
                                            .invoke(&[env.clone()])
                                    }])
                            })
                            .is_true()
                            {
                                {
                                    // (cdar env)
                                    imports::cdar
                                        .with(|value| value.get())
                                        .invoke(&[env.clone()])
                                }
                            } else {
                                {
                                    // (lookup name (cdr env))
                                    globals::lookup.with(|value| value.get()).invoke(&[
                                        name.clone(),
                                        {
                                            // (cdr env)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[env.clone()])
                                        },
                                    ])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (find-globals env) ...)
            globals::find_minus_globals.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let env = args[0].clone();
                        if ({
                            // (eq? (quote GLOBAL-MARKER) (car env))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("GLOBAL-MARKER"),
                                {
                                    // (car env)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[env.clone()])
                                },
                            ])
                        })
                        .is_true()
                        {
                            env.clone()
                        } else {
                            {
                                // (find-globals (cdr env))
                                globals::find_minus_globals
                                    .with(|value| value.get())
                                    .invoke(&[{
                                        // (cdr env)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[env.clone()])
                                    }])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-global! name env) ...)
            globals::adjoin_minus_global_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (adjoin-global-var! (new-global name) env)
                            globals::adjoin_minus_global_minus_var_i
                                .with(|value| value.get())
                                .invoke(&[
                                    {
                                        // (new-global name)
                                        imports::new_minus_global
                                            .with(|value| value.get())
                                            .invoke(&[name.clone()])
                                    },
                                    env.clone(),
                                ])
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-import! name env) ...)
            globals::adjoin_minus_import_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (adjoin-global-var! (new-import name) env)
                            globals::adjoin_minus_global_minus_var_i
                                .with(|value| value.get())
                                .invoke(&[
                                    {
                                        // (new-import name)
                                        imports::new_minus_import
                                            .with(|value| value.get())
                                            .invoke(&[name.clone()])
                                    },
                                    env.clone(),
                                ])
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-global-var! var env) ...)
            globals::adjoin_minus_global_minus_var_i.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
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
                                    globals::find_minus_globals
                                        .with(|value| value.get())
                                        .invoke(&[env.clone()])
                                };
                                {
                                    {
                                        // (set-cdr! genv (cons var (cdr genv)))
                                        imports::set_minus_cdr_i.with(|value| value.get()).invoke(
                                            &[genv.clone(), {
                                                // (cons var (cdr genv))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    var.clone(),
                                                    {
                                                        // (cdr genv)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[genv.clone()])
                                                    },
                                                ])
                                            }],
                                        )
                                    };
                                    {
                                        // (cdr var)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[var.clone()])
                                    }
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-local name env) ...)
            globals::adjoin_minus_local.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cons (new-local name) env)
                            imports::cons.with(|value| value.get()).invoke(&[
                                {
                                    // (new-local name)
                                    imports::new_minus_local
                                        .with(|value| value.get())
                                        .invoke(&[name.clone()])
                                },
                                env.clone(),
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-local-env name* env) ...)
            globals::adjoin_minus_local_minus_env.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? name*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[name_star_.clone()])
                            })
                            .is_true()
                            {
                                env.clone()
                            } else if ({
                                // (pair? name*)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[name_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))
                                    globals::adjoin_minus_local_minus_env
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (cdr name*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[name_star_.clone()])
                                            },
                                            {
                                                // (adjoin-local (car name*) env)
                                                globals::adjoin_minus_local
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        {
                                                            // (car name*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[name_star_.clone()])
                                                        },
                                                        env.clone(),
                                                    ])
                                            },
                                        ])
                                }
                            } else {
                                {
                                    // (adjoin-local name* env)
                                    globals::adjoin_minus_local
                                        .with(|value| value.get())
                                        .invoke(&[name_star_.clone(), env.clone()])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-import*! name* env) ...)
            globals::adjoin_minus_import_star__i.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name_star_ = args[0].clone();let env = args[1].clone();{
// (letrec ((loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))))) (loop name* (find-globals env)))
{
// (let ((loop (quote *uninitialized*))) (begin (set! loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv))))) (loop name* (find-globals env))))
{let loop_ = Scm::symbol("*uninitialized*");{let loop_ = loop_.into_boxed();{loop_.set({let loop_ = loop_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name_star_ = args[0].clone();let genv = args[1].clone();if ({
// (null? name*)
imports::null_p.with(|value| value.get()).invoke(&[name_star_.clone()])}).is_true() {Scm::Nil} else {{{
// (set-cdr! genv (cons (new-import (car name*)) (cdr genv)))
imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[genv.clone(),{
// (cons (new-import (car name*)) (cdr genv))
imports::cons.with(|value| value.get()).invoke(&[{
// (new-import (car name*))
imports::new_minus_import.with(|value| value.get()).invoke(&[{
// (car name*)
imports::car.with(|value| value.get()).invoke(&[name_star_.clone()])}])},{
// (cdr genv)
imports::cdr.with(|value| value.get()).invoke(&[genv.clone()])}])}])};{
// (loop (cdr name*) genv)
loop_.get().invoke(&[{
// (cdr name*)
imports::cdr.with(|value| value.get()).invoke(&[name_star_.clone()])},genv.clone()])}}}})});{
// (loop name* (find-globals env))
loop_.get().invoke(&[name_star_.clone(),{
// (find-globals env)
globals::find_minus_globals.with(|value| value.get()).invoke(&[env.clone()])}])}}}}}}})}))
        };
        {
            // (define (adjoin-boxed name env) ...)
            globals::adjoin_minus_boxed.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cons (new-boxed name) env)
                            imports::cons.with(|value| value.get()).invoke(&[
                                {
                                    // (new-boxed name)
                                    imports::new_minus_boxed
                                        .with(|value| value.get())
                                        .invoke(&[name.clone()])
                                },
                                env.clone(),
                            ])
                        }
                    })
                })
            })
        };
        {
            // (define (adjoin-boxed-env name* env) ...)
            globals::adjoin_minus_boxed_minus_env.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let name_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? name*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[name_star_.clone()])
                            })
                            .is_true()
                            {
                                env.clone()
                            } else if ({
                                // (pair? name*)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[name_star_.clone()])
                            })
                            .is_true()
                            {
                                {
                                    // (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))
                                    globals::adjoin_minus_boxed_minus_env
                                        .with(|value| value.get())
                                        .invoke(&[
                                            {
                                                // (cdr name*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[name_star_.clone()])
                                            },
                                            {
                                                // (adjoin-boxed (car name*) env)
                                                globals::adjoin_minus_boxed
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        {
                                                            // (car name*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[name_star_.clone()])
                                                        },
                                                        env.clone(),
                                                    ])
                                            },
                                        ])
                                }
                            } else {
                                {
                                    // (adjoin-boxed name* env)
                                    globals::adjoin_minus_boxed
                                        .with(|value| value.get())
                                        .invoke(&[name_star_.clone(), env.clone()])
                                }
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (map-env func env) ...)
            globals::map_minus_env.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let func = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (map (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
                            imports::map.with(|value| value.get()).invoke(&[
                                {
                                    let func = func.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let entry = args[0].clone();
                                        if ({
                                            // (eq? (quote GLOBAL-MARKER) entry)
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("GLOBAL-MARKER"),
                                                entry.clone(),
                                            ])
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
                    })
                })
            })
        };
        {
            // (define (env-for-each func env) ...)
            globals::env_minus_for_minus_each.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let func = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (for-each (lambda (entry) (if (eq? (quote GLOBAL-MARKER) entry) entry (func entry))) env)
                            imports::for_minus_each.with(|value| value.get()).invoke(&[
                                {
                                    let func = func.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let entry = args[0].clone();
                                        if ({
                                            // (eq? (quote GLOBAL-MARKER) entry)
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("GLOBAL-MARKER"),
                                                entry.clone(),
                                            ])
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
                    })
                })
            })
        }
    };
}
