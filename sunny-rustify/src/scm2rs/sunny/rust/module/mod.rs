#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::chibi::filesystem::exports::*;
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
}

pub mod exports {
    pub use super::globals::close_minus_module;
    pub use super::globals::module_minus_path;
    pub use super::globals::module_minus_port;
    pub use super::globals::module_p;
    pub use super::globals::open_minus_module;
    pub use super::globals::open_minus_submodule;
    pub use super::globals::print;
    pub use super::globals::println;
    pub use super::globals::rust_minus_block;
    pub use super::globals::show;
    pub use super::globals::showln;
}

mod globals {
    use sunny_core::{Mut, Scm};
    pub fn as_minus_port(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let port_minus_or_minus_module = args[0].clone();
            if ({
                // (module? port-or-module)
                globals::module_p
                    .with(|value| value.get())
                    .invoke(&[port_minus_or_minus_module.clone()])
            })
            .is_true()
            {
                {
                    // (module-port port-or-module)
                    globals::module_minus_port
                        .with(|value| value.get())
                        .invoke(&[port_minus_or_minus_module.clone()])
                }
            } else {
                port_minus_or_minus_module.clone()
            }
        }
    }
    pub fn close_minus_module(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let module = args[0].clone();
            {
                // (close-port (module-port module))
                imports::close_minus_port
                    .with(|value| value.get())
                    .invoke(&[{
                        // (module-port module)
                        globals::module_minus_port
                            .with(|value| value.get())
                            .invoke(&[module.clone()])
                    }])
            }
        }
    }
    pub fn module_minus_path(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let module = args[0].clone();
            {
                // (caddr module)
                imports::caddr
                    .with(|value| value.get())
                    .invoke(&[module.clone()])
            }
        }
    }
    pub fn module_minus_port(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let module = args[0].clone();
            {
                // (cadr module)
                imports::cadr
                    .with(|value| value.get())
                    .invoke(&[module.clone()])
            }
        }
    }
    pub fn module_p(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let obj = args[0].clone();
            {
                // (and (pair? obj) (eq? (quote module) (car obj)))
                if ({
                    // (pair? obj)
                    imports::pair_p
                        .with(|value| value.get())
                        .invoke(&[obj.clone()])
                })
                .is_true()
                {
                    {
                        // (eq? (quote module) (car obj))
                        imports::eq_p
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("module"), {
                                // (car obj)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[obj.clone()])
                            }])
                    }
                } else {
                    Scm::False
                }
            }
        }
    }
    pub fn open_minus_module(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let name = args[0].clone();
            let base_minus_path = args[1].clone();
            {
                // (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path))
                {
                    let path = {
                        // (string-append base-path "/" (rustify-libname name))
                        imports::string_minus_append
                            .with(|value| value.get())
                            .invoke(&[base_minus_path.clone(), Scm::from("/"), {
                                // (rustify-libname name)
                                imports::rustify_minus_libname
                                    .with(|value| value.get())
                                    .invoke(&[name.clone()])
                            }])
                    };
                    {
                        {
                            // (create-directory* path)
                            imports::create_minus_directory_star_
                                .with(|value| value.get())
                                .invoke(&[path.clone()])
                        };
                        {
                            // (list (quote module) (open-output-file (string-append path "/mod.rs")) path)
                            imports::list.with(|value| value.get()).invoke(&[
                                Scm::symbol("module"),
                                {
                                    // (open-output-file (string-append path "/mod.rs"))
                                    imports::open_minus_output_minus_file
                                        .with(|value| value.get())
                                        .invoke(&[{
                                            // (string-append path "/mod.rs")
                                            imports::string_minus_append
                                                .with(|value| value.get())
                                                .invoke(&[path.clone(), Scm::from("/mod.rs")])
                                        }])
                                },
                                path.clone(),
                            ])
                        }
                    }
                }
            }
        }
    }
    pub fn open_minus_submodule(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let name = args[0].clone();
            let module = args[1].clone();
            {
                // (open-module name (module-path module))
                globals::open_minus_module
                    .with(|value| value.get())
                    .invoke(&[name.clone(), {
                        // (module-path module)
                        globals::module_minus_path
                            .with(|value| value.get())
                            .invoke(&[module.clone()])
                    }])
            }
        }
    }
    pub fn print(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let f = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (for-each (lambda (a) (display a (as-port f))) args)
                imports::for_minus_each.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        let f = f.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a = args[0].clone();
                            {
                                // (display a (as-port f))
                                imports::display
                                    .with(|value| value.get())
                                    .invoke(&[a.clone(), {
                                        // (as-port f)
                                        globals::as_minus_port
                                            .with(|value| value.get())
                                            .invoke(&[f.clone()])
                                    }])
                            }
                        })
                    },
                    args_.clone(),
                ])
            }
        }
    }
    pub fn println(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let f = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                {
                    // (for-each (lambda (a) (display a (as-port f))) args)
                    imports::for_minus_each.with(|value| value.get()).invoke(&[
                        {
                            // Closure
                            let f = f.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let a = args[0].clone();
                                {
                                    // (display a (as-port f))
                                    imports::display.with(|value| value.get()).invoke(&[
                                        a.clone(),
                                        {
                                            // (as-port f)
                                            globals::as_minus_port
                                                .with(|value| value.get())
                                                .invoke(&[f.clone()])
                                        },
                                    ])
                                }
                            })
                        },
                        args_.clone(),
                    ])
                };
                {
                    // (newline (as-port f))
                    imports::newline.with(|value| value.get()).invoke(&[{
                        // (as-port f)
                        globals::as_minus_port
                            .with(|value| value.get())
                            .invoke(&[f.clone()])
                    }])
                }
            }
        }
    }
    pub fn rust_minus_block(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let module = args[0].clone();
            let code = args[1].clone();
            {
                {
                    // (print module "{")
                    globals::print
                        .with(|value| value.get())
                        .invoke(&[module.clone(), Scm::from("{")])
                };
                {
                    // (code)
                    code.clone().invoke(&[])
                };
                {
                    // (print module "}")
                    globals::print
                        .with(|value| value.get())
                        .invoke(&[module.clone(), Scm::from("}")])
                }
            }
        }
    }
    pub fn show(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let f = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (for-each (lambda (a) (write a (as-port f))) args)
                imports::for_minus_each.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        let f = f.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a = args[0].clone();
                            {
                                // (write a (as-port f))
                                imports::write
                                    .with(|value| value.get())
                                    .invoke(&[a.clone(), {
                                        // (as-port f)
                                        globals::as_minus_port
                                            .with(|value| value.get())
                                            .invoke(&[f.clone()])
                                    }])
                            }
                        })
                    },
                    args_.clone(),
                ])
            }
        }
    }
    pub fn showln(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let f = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                {
                    // (for-each (lambda (a) (write a (as-port f))) args)
                    imports::for_minus_each.with(|value| value.get()).invoke(&[
                        {
                            // Closure
                            let f = f.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let a = args[0].clone();
                                {
                                    // (write a (as-port f))
                                    imports::write
                                        .with(|value| value.get())
                                        .invoke(&[a.clone(), {
                                            // (as-port f)
                                            globals::as_minus_port
                                                .with(|value| value.get())
                                                .invoke(&[f.clone()])
                                        }])
                                }
                            })
                        },
                        args_.clone(),
                    ])
                };
                {
                    // (newline (as-port f))
                    imports::newline.with(|value| value.get()).invoke(&[{
                        // (as-port f)
                        globals::as_minus_port
                            .with(|value| value.get())
                            .invoke(&[f.clone()])
                    }])
                }
            }
        }
    }
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
    crate::scheme::write::initialize();
    crate::chibi::filesystem::initialize();
    crate::sunny::rust::rustify::initialize();
    {
        (/*NOP*/);
        {
            // (define (module? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (open-module name base-path) ...)
            (/*NOP*/)
        };
        {
            // (define (open-submodule name module) ...)
            (/*NOP*/)
        };
        {
            // (define (close-module module) ...)
            (/*NOP*/)
        };
        {
            // (define (module-port module) ...)
            (/*NOP*/)
        };
        {
            // (define (module-path module) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-block module code) ...)
            (/*NOP*/)
        };
        {
            // (define (println f . args) ...)
            (/*NOP*/)
        };
        {
            // (define (print f . args) ...)
            (/*NOP*/)
        };
        {
            // (define (showln f . args) ...)
            (/*NOP*/)
        };
        {
            // (define (show f . args) ...)
            (/*NOP*/)
        };
        {
            // (define (as-port port-or-module) ...)
            (/*NOP*/)
        }
    };
}
