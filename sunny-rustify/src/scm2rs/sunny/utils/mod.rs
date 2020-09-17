#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::dotted_minus_list_p;
    pub use super::globals::filter;
    pub use super::globals::last_minus_cdr;
    pub use super::globals::proper_minus_list_minus_part;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static filter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL filter"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static proper_minus_list_minus_part: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL proper-list-part"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static last_minus_cdr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL last-cdr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static dotted_minus_list_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL dotted-list?"))}
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
        // (define (dotted-list? seq) (not (null? (last-cdr seq))))
        globals::dotted_minus_list_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (letrec () (not (null? (last-cdr seq))))
                    {
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
                    }
                })
            })
        });
        // (define (last-cdr seq) (if (pair? seq) (last-cdr (cdr seq)) seq))
        globals::last_minus_cdr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (letrec () (if (pair? seq) (last-cdr (cdr seq)) seq))
                    {
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
                    }
                })
            })
        });
        // (define (proper-list-part seq) (if (pair? seq) (cons (car seq) (proper-list-part (cdr seq))) (quote ())))
        globals::proper_minus_list_minus_part.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (letrec () (if (pair? seq) (cons (car seq) (proper-list-part (cdr seq))) (quote ())))
                    {
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
                    }
                })
            })
        });
        // (define (filter f seq) (if (pair? seq) (if (f (car seq)) (cons (car seq) (filter f (cdr seq))) (filter f (cdr seq))) (quote ())))
        globals::filter.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let seq = args[1].clone();
                    // (letrec () (if (pair? seq) (if (f (car seq)) (cons (car seq) (filter f (cdr seq))) (filter f (cdr seq))) (quote ())))
                    {
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
                    }
                })
            })
        })
    };
}
