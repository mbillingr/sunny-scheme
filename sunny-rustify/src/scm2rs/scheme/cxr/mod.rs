#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::{caar, cadr, car, cdar, cddr, cdr};
}

pub mod exports {
    pub use super::globals::caaaar;
    pub use super::globals::caaadr;
    pub use super::globals::caaar;
    pub use super::globals::caadar;
    pub use super::globals::caaddr;
    pub use super::globals::caadr;
    pub use super::globals::cadaar;
    pub use super::globals::cadadr;
    pub use super::globals::cadar;
    pub use super::globals::caddar;
    pub use super::globals::cadddr;
    pub use super::globals::caddr;
    pub use super::globals::cdaaar;
    pub use super::globals::cdaadr;
    pub use super::globals::cdaar;
    pub use super::globals::cdadar;
    pub use super::globals::cdaddr;
    pub use super::globals::cdadr;
    pub use super::globals::cddaar;
    pub use super::globals::cddadr;
    pub use super::globals::cddar;
    pub use super::globals::cdddar;
    pub use super::globals::cddddr;
    pub use super::globals::cdddr;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static cddddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cddadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cddaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddaar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaaar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cadddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cadadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cadaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadaar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaaar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cdaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadar"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static caaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaar"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    {
        (/*NOP*/);
        {
            // (define (caaar x) ...)
            globals::caaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (car (caar x))
                            imports::car.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caadr x) ...)
            globals::caadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (car (cadr x))
                            imports::car.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cadar x) ...)
            globals::cadar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (car (cdar x))
                            imports::car.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caddr x) ...)
            globals::caddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (car (cddr x))
                            imports::car.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdaar x) ...)
            globals::cdaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdr (caar x))
                            imports::cdr.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdadr x) ...)
            globals::cdadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdr (cadr x))
                            imports::cdr.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cddar x) ...)
            globals::cddar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdr (cdar x))
                            imports::cdr.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdddr x) ...)
            globals::cdddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdr (cddr x))
                            imports::cdr.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caaaar x) ...)
            globals::caaaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (caar (caar x))
                            imports::caar.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caaadr x) ...)
            globals::caaadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (caar (cadr x))
                            imports::caar.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caadar x) ...)
            globals::caadar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (caar (cdar x))
                            imports::caar.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caaddr x) ...)
            globals::caaddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (caar (cddr x))
                            imports::caar.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cadaar x) ...)
            globals::cadaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cadr (caar x))
                            imports::cadr.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cadadr x) ...)
            globals::cadadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cadr (cadr x))
                            imports::cadr.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (caddar x) ...)
            globals::caddar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cadr (cdar x))
                            imports::cadr.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cadddr x) ...)
            globals::cadddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cadr (cddr x))
                            imports::cadr.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdaaar x) ...)
            globals::cdaaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdar (caar x))
                            imports::cdar.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdaadr x) ...)
            globals::cdaadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdar (cadr x))
                            imports::cdar.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdadar x) ...)
            globals::cdadar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdar (cdar x))
                            imports::cdar.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdaddr x) ...)
            globals::cdaddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cdar (cddr x))
                            imports::cdar.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cddaar x) ...)
            globals::cddaar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cddr (caar x))
                            imports::cddr.with(|value| value.get()).invoke(&[{
                                // (caar x)
                                imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cddadr x) ...)
            globals::cddadr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cddr (cadr x))
                            imports::cddr.with(|value| value.get()).invoke(&[{
                                // (cadr x)
                                imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cdddar x) ...)
            globals::cdddar.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cddr (cdar x))
                            imports::cddr.with(|value| value.get()).invoke(&[{
                                // (cdar x)
                                imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        };
        {
            // (define (cddddr x) ...)
            globals::cddddr.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let x = args[0].clone();
                        {
                            // (cddr (cddr x))
                            imports::cddr.with(|value| value.get()).invoke(&[{
                                // (cddr x)
                                imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                            }])
                        }
                    })
                })
            })
        }
    };
}
