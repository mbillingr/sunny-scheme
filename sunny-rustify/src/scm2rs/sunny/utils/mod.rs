#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::any;
    pub use super::globals::atom_p;
    pub use super::globals::bor;
    pub use super::globals::dotted_minus_list_p;
    pub use super::globals::filter;
    pub use super::globals::last_minus_cdr;
    pub use super::globals::proper_minus_list_minus_part;
    pub use super::globals::reduce;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static bor: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL bor"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static any: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL any"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static reduce: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL reduce"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static filter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL filter"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static proper_minus_list_minus_part: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL proper-list-part"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static dotted_minus_list_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL dotted-list?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static last_minus_cdr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL last-cdr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static atom_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL atom?"))}
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
        // (define (atom? x) ...)
        globals::atom_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let x = args[0].clone();
                    if (
                        // (pair? x)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[x.clone()])
                    )
                    .is_true()
                    {
                        Scm::False
                    } else {
                        Scm::True
                    }
                })
            })
        });
        // (define (dotted-list? seq) ...)
        globals::dotted_minus_list_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (not (null? (last-cdr seq)))
                    imports::not.with(|value| value.get()).invoke(&[
                        // (null? (last-cdr seq))
                        imports::null_p.with(|value| value.get()).invoke(&[
                            // (last-cdr seq)
                            globals::last_minus_cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()]),
                        ]),
                    ])
                })
            })
        });
        // (define (last-cdr seq) ...)
        globals::last_minus_cdr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    if (
                        // (pair? seq)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    )
                    .is_true()
                    {
                        // (last-cdr (cdr seq))
                        globals::last_minus_cdr.with(|value| value.get()).invoke(&[
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()]),
                        ])
                    } else {
                        seq.clone()
                    }
                })
            })
        });
        // (define (proper-list-part seq) ...)
        globals::proper_minus_list_minus_part.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    if (
                        // (pair? seq)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    )
                    .is_true()
                    {
                        // (cons (car seq) (proper-list-part (cdr seq)))
                        imports::cons.with(|value| value.get()).invoke(&[
                            // (car seq)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[seq.clone()]),
                            // (proper-list-part (cdr seq))
                            globals::proper_minus_list_minus_part
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ]),
                        ])
                    } else {
                        Scm::Nil
                    }
                })
            })
        });
        // (define (filter f seq) ...)
        globals::filter.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let seq = args[1].clone();
                    if (
                        // (pair? seq)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    )
                    .is_true()
                    {
                        if (
                            // (f (car seq))
                            f.clone().invoke(&[
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cons (car seq) (filter f (cdr seq)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                                // (filter f (cdr seq))
                                globals::filter.with(|value| value.get()).invoke(&[
                                    f.clone(),
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ]),
                            ])
                        } else {
                            // (filter f (cdr seq))
                            globals::filter.with(|value| value.get()).invoke(&[
                                f.clone(),
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ])
                        }
                    } else {
                        Scm::Nil
                    }
                })
            })
        });
        // (define (reduce f init seq) ...)
        globals::reduce.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let init = args[1].clone();
                    let seq = args[2].clone();
                    if (
                        // (pair? seq)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    )
                    .is_true()
                    {
                        // (reduce f (f init (car seq)) (cdr seq))
                        globals::reduce.with(|value| value.get()).invoke(&[
                            f.clone(),
                            // (f init (car seq))
                            f.clone().invoke(&[
                                init.clone(),
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ]),
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()]),
                        ])
                    } else {
                        init.clone()
                    }
                })
            })
        });
        // (define (any f seq) ...)
        globals::any.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let seq = args[1].clone();
                    if (
                        // (pair? seq)
                        imports::pair_p
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    )
                    .is_true()
                    {
                        if (
                            // (f (car seq))
                            f.clone().invoke(&[
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            Scm::True
                        } else {
                            // (any f (cdr seq))
                            globals::any.with(|value| value.get()).invoke(&[
                                f.clone(),
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ])
                        }
                    } else {
                        Scm::False
                    }
                })
            })
        });
        // (define (bor first . args) ...)
        globals::bor.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() < 1 {
                        panic!("not enough args")
                    }
                    let first = args[0].clone();
                    let args_ = Scm::list(&args[1..]);
                    if (first.clone()).is_true() {
                        first.clone()
                    } else if (
                        // (null? args)
                        imports::null_p
                            .with(|value| value.get())
                            .invoke(&[args_.clone()])
                    )
                    .is_true()
                    {
                        Scm::False
                    } else {
                        // (apply bor args)
                        imports::apply
                            .with(|value| value.get())
                            .invoke(&[globals::bor.with(|value| value.get()), args_.clone()])
                    }
                })
            })
        })
    };
}
