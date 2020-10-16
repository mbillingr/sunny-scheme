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
    pub use super::close_minus_module;
    pub use super::module_minus_path;
    pub use super::module_minus_port;
    pub use super::module_p;
    pub use super::open_minus_module;
    pub use super::open_minus_submodule;
    pub use super::print;
    pub use super::println;
    pub use super::rust_minus_block;
    pub use super::show;
    pub use super::showln;
}

pub fn as_minus_port(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let port_minus_or_minus_module_0 = args[0].clone();
        if ({
            // (module? port-or-module)
            module_p(&[port_minus_or_minus_module_0.clone()])
        })
        .is_true()
        {
            {
                // (module-port port-or-module)
                module_minus_port(&[port_minus_or_minus_module_0.clone()])
            }
        } else {
            port_minus_or_minus_module_0.clone()
        }
    }
    .into()
}
pub fn close_minus_module(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module_1 = args[0].clone();
        {
            // (close-port (module-port module))
            imports::close_minus_port(&[{
                // (module-port module)
                Scm::func(module_minus_port).invoke(&[module_1.clone()])
            }])
        }
    }
    .into()
}
pub fn module_minus_path(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module_3 = args[0].clone();
        {
            // (caddr module)
            imports::caddr(&[module_3.clone()])
        }
    }
    .into()
}
pub fn module_minus_port(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module_2 = args[0].clone();
        {
            // (cadr module)
            imports::cadr(&[module_2.clone()])
        }
    }
    .into()
}
pub fn module_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_14 = args[0].clone();
        {
            // (and (pair? obj) (eq? (quote module) (car obj)))
            if ({
                // (pair? obj)
                imports::pair_p(&[obj_14.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote module) (car obj))
                    imports::eq_p(&[Scm::symbol("module"), {
                        // (car obj)
                        imports::car(&[obj_14.clone()])
                    }])
                }
            } else {
                Scm::False
            }
        }
    }
    .into()
}
pub fn open_minus_module(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_24 = args[0].clone();
        let base_minus_path_0 = args[1].clone();
        {
            // (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path))
            {
                let path_0 = {
                    // (string-append base-path "/" (rustify-libname name))
                    imports::string_minus_append(&[base_minus_path_0.clone(), Scm::from("/"), {
                        // (rustify-libname name)
                        imports::rustify_minus_libname(&[name_24.clone()])
                    }])
                };
                {
                    {
                        // (create-directory* path)
                        imports::create_minus_directory_star_(&[path_0.clone()])
                    };
                    {
                        // (list (quote module) (open-output-file (string-append path "/mod.rs")) path)
                        imports::list(&[
                            Scm::symbol("module"),
                            {
                                // (open-output-file (string-append path "/mod.rs"))
                                imports::open_minus_output_minus_file(&[{
                                    // (string-append path "/mod.rs")
                                    imports::string_minus_append(&[
                                        path_0.clone(),
                                        Scm::from("/mod.rs"),
                                    ])
                                }])
                            },
                            path_0.clone(),
                        ])
                    }
                }
            }
        }
    }
    .into()
}
pub fn open_minus_submodule(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name_25 = args[0].clone();
        let module_0 = args[1].clone();
        {
            // (open-module name (module-path module))
            open_minus_module(&[name_25.clone(), {
                // (module-path module)
                Scm::func(module_minus_path).invoke(&[module_0.clone()])
            }])
        }
    }
    .into()
}
pub fn print(args: &[Scm]) -> Scm {
    {
        if args.len() < 1 {
            panic!("not enough args")
        }
        let f_5 = args[0].clone();
        let args__6 = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (display a (as-port f))) args)
            imports::for_minus_each(&[
                {
                    // Closure
                    let f_5 = f_5.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let a_2 = args[0].clone();
                        {
                            // (display a (as-port f))
                            imports::display(&[a_2.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f_5.clone()])
                            }])
                        }
                    })
                },
                args__6.clone(),
            ])
        }
    }
    .into()
}
pub fn println(args: &[Scm]) -> Scm {
    {
        if args.len() < 1 {
            panic!("not enough args")
        }
        let f_4 = args[0].clone();
        let args__5 = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (display a (as-port f))) args)
                imports::for_minus_each(&[
                    {
                        // Closure
                        let f_4 = f_4.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a_1 = args[0].clone();
                            {
                                // (display a (as-port f))
                                imports::display(&[a_1.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f_4.clone()])
                                }])
                            }
                        })
                    },
                    args__5.clone(),
                ])
            };
            {
                // (newline (as-port f))
                imports::newline(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f_4.clone()])
                }])
            }
        }
    }
    .into()
}
pub fn rust_minus_block(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let module_4 = args[0].clone();
        let code_0 = args[1].clone();
        {
            {
                // (print module "{")
                Scm::func(print).invoke(&[module_4.clone(), Scm::from("{")])
            };
            {
                // (code)
                code_0.clone().invoke(&[])
            };
            {
                // (print module "}")
                Scm::func(print).invoke(&[module_4.clone(), Scm::from("}")])
            }
        }
    }
    .into()
}
pub fn show(args: &[Scm]) -> Scm {
    {
        if args.len() < 1 {
            panic!("not enough args")
        }
        let f_7 = args[0].clone();
        let args__8 = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (write a (as-port f))) args)
            imports::for_minus_each(&[
                {
                    // Closure
                    let f_7 = f_7.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let a_4 = args[0].clone();
                        {
                            // (write a (as-port f))
                            imports::write(&[a_4.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f_7.clone()])
                            }])
                        }
                    })
                },
                args__8.clone(),
            ])
        }
    }
    .into()
}
pub fn showln(args: &[Scm]) -> Scm {
    {
        if args.len() < 1 {
            panic!("not enough args")
        }
        let f_6 = args[0].clone();
        let args__7 = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (write a (as-port f))) args)
                imports::for_minus_each(&[
                    {
                        // Closure
                        let f_6 = f_6.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a_3 = args[0].clone();
                            {
                                // (write a (as-port f))
                                imports::write(&[a_3.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f_6.clone()])
                                }])
                            }
                        })
                    },
                    args__7.clone(),
                ])
            };
            {
                // (newline (as-port f))
                imports::newline(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f_6.clone()])
                }])
            }
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
