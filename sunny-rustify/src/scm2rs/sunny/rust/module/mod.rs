#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::chibi::filesystem::exports::*;
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
}

pub mod exports {
    pub use super::globals::close_minus_module;
    pub use super::globals::module_minus_path;
    pub use super::globals::module_minus_port;
    pub use super::globals::module_p;
    pub use super::globals::open_minus_module;
    pub use super::globals::open_minus_submodule;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_port: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-port"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static close_minus_module: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL close-module"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_path: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-path"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static open_minus_submodule: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL open-submodule"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static open_minus_module: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL open-module"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static module_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module?"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::file::initialize();
    crate::chibi::filesystem::initialize();
    crate::sunny::rust::rustify::initialize();
    {
        (/*NOP*/);
        // (define (module? obj) (and (pair? obj) (eq? (quote module) (car obj))))
        globals::module_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    // (letrec () (and (pair? obj) (eq? (quote module) (car obj))))
                    {
                        // (and (pair? obj) (eq? (quote module) (car obj)))
                        if (
                            // (pair? obj)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[obj.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (quote module) (car obj))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("module"),
                                // (car obj)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[obj.clone()]),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (open-module name base-path) (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path)))
        globals::open_minus_module.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let base_minus_path = args[1].clone();
                    // (letrec () (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path)))
                    {
                        // (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path))
                        {
                            let [path] = [
                                // (string-append base-path "/" (rustify-libname name))
                                imports::string_minus_append
                                    .with(|value| value.get())
                                    .invoke(&[
                                        base_minus_path.clone(),
                                        Scm::from("/"),
                                        // (rustify-libname name)
                                        imports::rustify_minus_libname
                                            .with(|value| value.get())
                                            .invoke(&[name.clone()]),
                                    ]),
                            ];
                            {
                                // (create-directory* path)
                                imports::create_minus_directory_star_
                                    .with(|value| value.get())
                                    .invoke(&[path.clone()]);
                                // (list (quote module) (open-output-file (string-append path "/mod.rs")) path)
                                imports::list.with(|value| value.get()).invoke(&[
                                    Scm::symbol("module"),
                                    // (open-output-file (string-append path "/mod.rs"))
                                    imports::open_minus_output_minus_file
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (string-append path "/mod.rs")
                                            imports::string_minus_append
                                                .with(|value| value.get())
                                                .invoke(&[path.clone(), Scm::from("/mod.rs")]),
                                        ]),
                                    path.clone(),
                                ])
                            }
                        }
                    }
                })
            })
        });
        // (define (open-submodule name module) (open-module name (module-path module)))
        globals::open_minus_submodule.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let module = args[1].clone();
                    // (letrec () (open-module name (module-path module)))
                    {
                        // (open-module name (module-path module))
                        globals::open_minus_module
                            .with(|value| value.get())
                            .invoke(&[
                                name.clone(),
                                // (module-path module)
                                globals::module_minus_path
                                    .with(|value| value.get())
                                    .invoke(&[module.clone()]),
                            ])
                    }
                })
            })
        });
        // (define (close-module module) (close-port (module-port module)))
        globals::close_minus_module.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let module = args[0].clone();
                    // (letrec () (close-port (module-port module)))
                    {
                        // (close-port (module-port module))
                        imports::close_minus_port
                            .with(|value| value.get())
                            .invoke(&[
                                // (module-port module)
                                globals::module_minus_port
                                    .with(|value| value.get())
                                    .invoke(&[module.clone()]),
                            ])
                    }
                })
            })
        });
        // (define (module-port module) (cadr module))
        globals::module_minus_port.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let module = args[0].clone();
                    // (letrec () (cadr module))
                    {
                        // (cadr module)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[module.clone()])
                    }
                })
            })
        });
        // (define (module-path module) (caddr module))
        globals::module_minus_path.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let module = args[0].clone();
                    // (letrec () (caddr module))
                    {
                        // (caddr module)
                        imports::caddr
                            .with(|value| value.get())
                            .invoke(&[module.clone()])
                    }
                })
            })
        })
    };
}
