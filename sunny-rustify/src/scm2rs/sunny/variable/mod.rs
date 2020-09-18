#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
}

pub mod exports {
    pub use super::globals::global_minus_imported_p;
    pub use super::globals::global_minus_regular_p;
    pub use super::globals::variable;
    pub use super::globals::variable_minus_getter;
    pub use super::globals::variable_minus_mut_p;
    pub use super::globals::variable_minus_set_minus_getter_i;
    pub use super::globals::variable_minus_set_minus_mutable_i;
    pub use super::globals::variable_minus_set_minus_setter_i;
    pub use super::globals::variable_minus_setter;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_regular_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-regular?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_imported_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-imported?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_setter_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-setter!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_getter_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-getter!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_mutable_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-mutable!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_mut_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-mut?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_setter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-setter"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_getter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-getter"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    {
        (/*NOP*/);
        // (define (variable getter setter mut?) (list getter setter mut?))
        globals::variable.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let getter = args[0].clone();
                    let setter = args[1].clone();
                    let mut_p = args[2].clone();
                    // (letrec () (list getter setter mut?))
                    {
                        // (list getter setter mut?)
                        imports::list.with(|value| value.get()).invoke(&[
                            getter.clone(),
                            setter.clone(),
                            mut_p.clone(),
                        ])
                    }
                })
            })
        });
        // (define (variable-getter var) (car var))
        globals::variable_minus_getter.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (car var))
                    {
                        // (car var)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[var.clone()])
                    }
                })
            })
        });
        // (define (variable-setter var) (cadr var))
        globals::variable_minus_setter.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (cadr var))
                    {
                        // (cadr var)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[var.clone()])
                    }
                })
            })
        });
        // (define (variable-mut? var) (caddr var))
        globals::variable_minus_mut_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (caddr var))
                    {
                        // (caddr var)
                        imports::caddr
                            .with(|value| value.get())
                            .invoke(&[var.clone()])
                    }
                })
            })
        });
        // (define (variable-set-mutable! var) (set-car! (cddr var) #t))
        globals::variable_minus_set_minus_mutable_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (set-car! (cddr var) #t))
                    {
                        // (set-car! (cddr var) #t)
                        imports::set_minus_car_i.with(|value| value.get()).invoke(&[
                            // (cddr var)
                            imports::cddr
                                .with(|value| value.get())
                                .invoke(&[var.clone()]),
                            Scm::True,
                        ])
                    }
                })
            })
        });
        // (define (variable-set-getter! var getter) (set-car! var getter))
        globals::variable_minus_set_minus_getter_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    let getter = args[1].clone();
                    // (letrec () (set-car! var getter))
                    {
                        // (set-car! var getter)
                        imports::set_minus_car_i
                            .with(|value| value.get())
                            .invoke(&[var.clone(), getter.clone()])
                    }
                })
            })
        });
        // (define (variable-set-setter! var setter) (set-car! (cdr var) setter))
        globals::variable_minus_set_minus_setter_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    let setter = args[1].clone();
                    // (letrec () (set-car! (cdr var) setter))
                    {
                        // (set-car! (cdr var) setter)
                        imports::set_minus_car_i.with(|value| value.get()).invoke(&[
                            // (cdr var)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[var.clone()]),
                            setter.clone(),
                        ])
                    }
                })
            })
        });
        // (define (global-imported? var) (eq? (quote IMPORT-REF) (car var)))
        globals::global_minus_imported_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (eq? (quote IMPORT-REF) (car var)))
                    {
                        // (eq? (quote IMPORT-REF) (car var))
                        imports::eq_p.with(|value| value.get()).invoke(&[
                            Scm::symbol("IMPORT-REF"),
                            // (car var)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[var.clone()]),
                        ])
                    }
                })
            })
        });
        // (define (global-regular? var) (eq? (quote GLOBAL-REF) (car var)))
        globals::global_minus_regular_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (eq? (quote GLOBAL-REF) (car var)))
                    {
                        // (eq? (quote GLOBAL-REF) (car var))
                        imports::eq_p.with(|value| value.get()).invoke(&[
                            Scm::symbol("GLOBAL-REF"),
                            // (car var)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[var.clone()]),
                        ])
                    }
                })
            })
        })
    };
}
