#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::table::exports::*;
}

pub mod exports {
    pub use super::globals::boxed_minus_variable_p;
    pub use super::globals::global_minus_add_minus_definition_i;
    pub use super::globals::global_minus_variable_p;
    pub use super::globals::import_minus_variable_p;
    pub use super::globals::local_minus_boxify_i;
    pub use super::globals::local_minus_variable_p;
    pub use super::globals::new_minus_boxed;
    pub use super::globals::new_minus_global;
    pub use super::globals::new_minus_import;
    pub use super::globals::new_minus_local;
    pub use super::globals::variable_minus_mutable_p;
    pub use super::globals::variable_minus_set_minus_mutable_i;
    pub use super::globals::variable_p;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_boxed: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-boxed"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_local: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-local"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_global: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-global"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static local_minus_boxify_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL local-boxify!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_add_minus_definition_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-add-definition!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_mutable_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-mutable!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_mutable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-mutable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxed_minus_variable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxed-variable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static local_minus_variable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL local-variable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_variable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-variable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_variable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-variable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static variable_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static BoxedVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL BoxedVariable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static LocalVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL LocalVariable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static ImportedVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ImportedVariable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static GlobalVariable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL GlobalVariable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static Variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL Variable"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::sunny::table::initialize();
    {
        (/*NOP*/);
        // (define Variable (make-table))
        globals::Variable.with(|value| {
            value.set(
                // (make-table)
                imports::make_minus_table
                    .with(|value| value.get())
                    .invoke(&[]),
            )
        });
        // (set-field! Variable (quote mut) #f)
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::Variable.with(|value| value.get()),
                Scm::symbol("mut"),
                Scm::False,
            ]);
        // (set-field! Variable (quote mutable?) (lambda (self) (get-field self (quote mut))))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::Variable.with(|value| value.get()),
                Scm::symbol("mutable?"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        // (letrec () (get-field self (quote mut)))
                        {
                            // (get-field self (quote mut))
                            imports::get_minus_field
                                .with(|value| value.get())
                                .invoke(&[self_.clone(), Scm::symbol("mut")])
                        }
                    })
                },
            ]);
        // (set-field! Variable (quote set-mutable!) (lambda (self) (set-field! self (quote mut) #t)))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::Variable.with(|value| value.get()),
                Scm::symbol("set-mutable!"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        // (letrec () (set-field! self (quote mut) #t))
                        {
                            // (set-field! self (quote mut) #t)
                            imports::set_minus_field_i
                                .with(|value| value.get())
                                .invoke(&[self_.clone(), Scm::symbol("mut"), Scm::True])
                        }
                    })
                },
            ]);
        // (define GlobalVariable (clone Variable))
        globals::GlobalVariable.with(|value| {
            value.set(
                // (clone Variable)
                imports::clone
                    .with(|value| value.get())
                    .invoke(&[globals::Variable.with(|value| value.get())]),
            )
        });
        // (set-field! GlobalVariable (quote status) (quote UNDEFINED))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::GlobalVariable.with(|value| value.get()),
                Scm::symbol("status"),
                Scm::symbol("UNDEFINED"),
            ]);
        // (set-field! GlobalVariable (quote mutable?) (lambda (self) (eq? (quote MUTABLE) (get-field self (quote status)))))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::GlobalVariable.with(|value| value.get()),
                Scm::symbol("mutable?"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        // (letrec () (eq? (quote MUTABLE) (get-field self (quote status))))
                        {
                            // (eq? (quote MUTABLE) (get-field self (quote status)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("MUTABLE"),
                                // (get-field self (quote status))
                                imports::get_minus_field
                                    .with(|value| value.get())
                                    .invoke(&[self_.clone(), Scm::symbol("status")]),
                            ])
                        }
                    })
                },
            ]);
        // (set-field! GlobalVariable (quote set-mutable!) (lambda (self) (set-field! self (quote status) (quote MUTABLE))))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::GlobalVariable.with(|value| value.get()),
                Scm::symbol("set-mutable!"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        // (letrec () (set-field! self (quote status) (quote MUTABLE)))
                        {
                            // (set-field! self (quote status) (quote MUTABLE))
                            imports::set_minus_field_i
                                .with(|value| value.get())
                                .invoke(&[
                                    self_.clone(),
                                    Scm::symbol("status"),
                                    Scm::symbol("MUTABLE"),
                                ])
                        }
                    })
                },
            ]);
        // (set-field! GlobalVariable (quote add-definition!) (lambda (self proc?) (if (eq? (quote UNDEFINED) (get-field self (quote status))) (set-field! self (quote status) (if proc? (quote PROC) (quote VAL))) (set-field! self (quote status) (quote MUTABLE)))))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::GlobalVariable.with(|value| value.get()),
                Scm::symbol("add-definition!"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        let proc_p = args[1].clone();
                        // (letrec () (if (eq? (quote UNDEFINED) (get-field self (quote status))) (set-field! self (quote status) (if proc? (quote PROC) (quote VAL))) (set-field! self (quote status) (quote MUTABLE))))
                        {
                            if (
                                // (eq? (quote UNDEFINED) (get-field self (quote status)))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("UNDEFINED"),
                                    // (get-field self (quote status))
                                    imports::get_minus_field
                                        .with(|value| value.get())
                                        .invoke(&[self_.clone(), Scm::symbol("status")]),
                                ])
                            )
                            .is_true()
                            {
                                // (set-field! self (quote status) (if proc? (quote PROC) (quote VAL)))
                                imports::set_minus_field_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        self_.clone(),
                                        Scm::symbol("status"),
                                        if (proc_p.clone()).is_true() {
                                            Scm::symbol("PROC")
                                        } else {
                                            Scm::symbol("VAL")
                                        },
                                    ])
                            } else {
                                // (set-field! self (quote status) (quote MUTABLE))
                                imports::set_minus_field_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        self_.clone(),
                                        Scm::symbol("status"),
                                        Scm::symbol("MUTABLE"),
                                    ])
                            }
                        }
                    })
                },
            ]);
        // (define ImportedVariable (clone Variable))
        globals::ImportedVariable.with(|value| {
            value.set(
                // (clone Variable)
                imports::clone
                    .with(|value| value.get())
                    .invoke(&[globals::Variable.with(|value| value.get())]),
            )
        });
        // (define LocalVariable (clone Variable))
        globals::LocalVariable.with(|value| {
            value.set(
                // (clone Variable)
                imports::clone
                    .with(|value| value.get())
                    .invoke(&[globals::Variable.with(|value| value.get())]),
            )
        });
        // (set-field! LocalVariable (quote into-boxed!) (lambda (self) (set-parent! self BoxedVariable)))
        imports::set_minus_field_i
            .with(|value| value.get())
            .invoke(&[
                globals::LocalVariable.with(|value| value.get()),
                Scm::symbol("into-boxed!"),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let self_ = args[0].clone();
                        // (letrec () (set-parent! self BoxedVariable))
                        {
                            // (set-parent! self BoxedVariable)
                            imports::set_minus_parent_i
                                .with(|value| value.get())
                                .invoke(&[
                                    self_.clone(),
                                    globals::BoxedVariable.with(|value| value.get()),
                                ])
                        }
                    })
                },
            ]);
        // (define BoxedVariable (clone LocalVariable))
        globals::BoxedVariable.with(|value| {
            value.set(
                // (clone LocalVariable)
                imports::clone
                    .with(|value| value.get())
                    .invoke(&[globals::LocalVariable.with(|value| value.get())]),
            )
        });
        // (define (variable? obj) (ancestor? obj Variable))
        globals::variable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (ancestor? obj Variable))
                    {
                        // (ancestor? obj Variable)
                        imports::ancestor_p
                            .with(|value| value.get())
                            .invoke(&[obj.clone(), globals::Variable.with(|value| value.get())])
                    }
                })
            })
        });
        // (define (global-variable? obj) (ancestor? obj GlobalVariable))
        globals::global_minus_variable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (ancestor? obj GlobalVariable))
                    {
                        // (ancestor? obj GlobalVariable)
                        imports::ancestor_p.with(|value| value.get()).invoke(&[
                            obj.clone(),
                            globals::GlobalVariable.with(|value| value.get()),
                        ])
                    }
                })
            })
        });
        // (define (import-variable? obj) (ancestor? obj ImportedVariable))
        globals::import_minus_variable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (ancestor? obj ImportedVariable))
                    {
                        // (ancestor? obj ImportedVariable)
                        imports::ancestor_p.with(|value| value.get()).invoke(&[
                            obj.clone(),
                            globals::ImportedVariable.with(|value| value.get()),
                        ])
                    }
                })
            })
        });
        // (define (local-variable? obj) (ancestor? obj LocalVariable))
        globals::local_minus_variable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (ancestor? obj LocalVariable))
                    {
                        // (ancestor? obj LocalVariable)
                        imports::ancestor_p.with(|value| value.get()).invoke(&[
                            obj.clone(),
                            globals::LocalVariable.with(|value| value.get()),
                        ])
                    }
                })
            })
        });
        // (define (boxed-variable? obj) (ancestor? obj BoxedVariable))
        globals::boxed_minus_variable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (ancestor? obj BoxedVariable))
                    {
                        // (ancestor? obj BoxedVariable)
                        imports::ancestor_p.with(|value| value.get()).invoke(&[
                            obj.clone(),
                            globals::BoxedVariable.with(|value| value.get()),
                        ])
                    }
                })
            })
        });
        // (define (variable-mutable? var) (call-method var (quote mutable?)))
        globals::variable_minus_mutable_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (call-method var (quote mutable?)))
                    {
                        // (call-method var (quote mutable?))
                        imports::call_minus_method
                            .with(|value| value.get())
                            .invoke(&[var.clone(), Scm::symbol("mutable?")])
                    }
                })
            })
        });
        // (define (variable-set-mutable! var) (call-method var (quote set-mutable!)))
        globals::variable_minus_set_minus_mutable_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (call-method var (quote set-mutable!)))
                    {
                        // (call-method var (quote set-mutable!))
                        imports::call_minus_method
                            .with(|value| value.get())
                            .invoke(&[var.clone(), Scm::symbol("set-mutable!")])
                    }
                })
            })
        });
        // (define (global-add-definition! var lambda?) (call-method var (quote add-definition!) lambda?))
        globals::global_minus_add_minus_definition_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    let lambda_p = args[1].clone();
                    // (letrec () (call-method var (quote add-definition!) lambda?))
                    {
                        // (call-method var (quote add-definition!) lambda?)
                        imports::call_minus_method
                            .with(|value| value.get())
                            .invoke(&[
                                var.clone(),
                                Scm::symbol("add-definition!"),
                                lambda_p.clone(),
                            ])
                    }
                })
            })
        });
        // (define (local-boxify! var) (call-method var (quote into-boxed!)))
        globals::local_minus_boxify_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    // (letrec () (call-method var (quote into-boxed!)))
                    {
                        // (call-method var (quote into-boxed!))
                        imports::call_minus_method
                            .with(|value| value.get())
                            .invoke(&[var.clone(), Scm::symbol("into-boxed!")])
                    }
                })
            })
        });
        // (define (new-import name) (cons name (clone ImportedVariable)))
        globals::new_minus_import.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (clone ImportedVariable)))
                    {
                        // (cons name (clone ImportedVariable))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (clone ImportedVariable)
                            imports::clone
                                .with(|value| value.get())
                                .invoke(&[globals::ImportedVariable.with(|value| value.get())]),
                        ])
                    }
                })
            })
        });
        // (define (new-global name) (cons name (clone GlobalVariable)))
        globals::new_minus_global.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (clone GlobalVariable)))
                    {
                        // (cons name (clone GlobalVariable))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (clone GlobalVariable)
                            imports::clone
                                .with(|value| value.get())
                                .invoke(&[globals::GlobalVariable.with(|value| value.get())]),
                        ])
                    }
                })
            })
        });
        // (define (new-local name) (cons name (clone LocalVariable)))
        globals::new_minus_local.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (clone LocalVariable)))
                    {
                        // (cons name (clone LocalVariable))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (clone LocalVariable)
                            imports::clone
                                .with(|value| value.get())
                                .invoke(&[globals::LocalVariable.with(|value| value.get())]),
                        ])
                    }
                })
            })
        });
        // (define (new-boxed name) (cons name (clone BoxedVariable)))
        globals::new_minus_boxed.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (clone BoxedVariable)))
                    {
                        // (cons name (clone BoxedVariable))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (clone BoxedVariable)
                            imports::clone
                                .with(|value| value.get())
                                .invoke(&[globals::BoxedVariable.with(|value| value.get())]),
                        ])
                    }
                })
            })
        })
    };
}
