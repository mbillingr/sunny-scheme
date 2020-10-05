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
        let port_minus_or_minus_module = args[0].clone();
        if ({
            // (module? port-or-module)
            Scm::func(module_p).invoke(&[port_minus_or_minus_module.clone()])
        })
        .is_true()
        {
            {
                // (module-port port-or-module)
                Scm::func(module_minus_port).invoke(&[port_minus_or_minus_module.clone()])
            }
        } else {
            port_minus_or_minus_module.clone()
        }
    }
    .into()
}
pub fn close_minus_module(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module = args[0].clone();
        {
            // (close-port (module-port module))
            Scm::func(imports::close_minus_port).invoke(&[{
                // (module-port module)
                Scm::func(module_minus_port).invoke(&[module.clone()])
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
        let module = args[0].clone();
        {
            // (caddr module)
            Scm::func(imports::caddr).invoke(&[module.clone()])
        }
    }
    .into()
}
pub fn module_minus_port(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module = args[0].clone();
        {
            // (cadr module)
            Scm::func(imports::cadr).invoke(&[module.clone()])
        }
    }
    .into()
}
pub fn module_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        {
            // (and (pair? obj) (eq? (quote module) (car obj)))
            if ({
                // (pair? obj)
                Scm::func(imports::pair_p).invoke(&[obj.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote module) (car obj))
                    Scm::func(imports::eq_p).invoke(&[Scm::symbol("module"), {
                        // (car obj)
                        Scm::func(imports::car).invoke(&[obj.clone()])
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
        let name = args[0].clone();
        let base_minus_path = args[1].clone();
        {
            // (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path))
            {
                let path = {
                    // (string-append base-path "/" (rustify-libname name))
                    Scm::func(imports::string_minus_append).invoke(&[
                        base_minus_path.clone(),
                        Scm::from("/"),
                        {
                            // (rustify-libname name)
                            Scm::func(imports::rustify_minus_libname).invoke(&[name.clone()])
                        },
                    ])
                };
                {
                    {
                        // (create-directory* path)
                        Scm::func(imports::create_minus_directory_star_).invoke(&[path.clone()])
                    };
                    {
                        // (list (quote module) (open-output-file (string-append path "/mod.rs")) path)
                        Scm::func(imports::list).invoke(&[
                            Scm::symbol("module"),
                            {
                                // (open-output-file (string-append path "/mod.rs"))
                                Scm::func(imports::open_minus_output_minus_file).invoke(&[{
                                    // (string-append path "/mod.rs")
                                    Scm::func(imports::string_minus_append)
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
    .into()
}
pub fn open_minus_submodule(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        let module = args[1].clone();
        {
            // (open-module name (module-path module))
            Scm::func(open_minus_module).invoke(&[name.clone(), {
                // (module-path module)
                Scm::func(module_minus_path).invoke(&[module.clone()])
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
        let f = args[0].clone();
        let args_ = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (display a (as-port f))) args)
            Scm::func(imports::for_minus_each).invoke(&[
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
                            Scm::func(imports::display).invoke(&[a.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f.clone()])
                            }])
                        }
                    })
                },
                args_.clone(),
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
        let f = args[0].clone();
        let args_ = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (display a (as-port f))) args)
                Scm::func(imports::for_minus_each).invoke(&[
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
                                Scm::func(imports::display).invoke(&[a.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f.clone()])
                                }])
                            }
                        })
                    },
                    args_.clone(),
                ])
            };
            {
                // (newline (as-port f))
                Scm::func(imports::newline).invoke(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f.clone()])
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
        let module = args[0].clone();
        let code = args[1].clone();
        {
            {
                // (print module "{")
                Scm::func(print).invoke(&[module.clone(), Scm::from("{")])
            };
            {
                // (code)
                code.clone().invoke(&[])
            };
            {
                // (print module "}")
                Scm::func(print).invoke(&[module.clone(), Scm::from("}")])
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
        let f = args[0].clone();
        let args_ = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (write a (as-port f))) args)
            Scm::func(imports::for_minus_each).invoke(&[
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
                            Scm::func(imports::write).invoke(&[a.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f.clone()])
                            }])
                        }
                    })
                },
                args_.clone(),
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
        let f = args[0].clone();
        let args_ = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (write a (as-port f))) args)
                Scm::func(imports::for_minus_each).invoke(&[
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
                                Scm::func(imports::write).invoke(&[a.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f.clone()])
                                }])
                            }
                        })
                    },
                    args_.clone(),
                ])
            };
            {
                // (newline (as-port f))
                Scm::func(imports::newline).invoke(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f.clone()])
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
