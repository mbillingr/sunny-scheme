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
        let port_minus_or_minus_module__296 = args[0].clone();
        if ({
            // (module? port-or-module)
            module_p(&[port_minus_or_minus_module__296.clone()])
        })
        .is_true()
        {
            {
                // (module-port port-or-module)
                module_minus_port(&[port_minus_or_minus_module__296.clone()])
            }
        } else {
            port_minus_or_minus_module__296.clone()
        }
    }
    .into()
}
pub fn close_minus_module(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module__279 = args[0].clone();
        {
            // (close-port (module-port module))
            imports::close_minus_port(&[{
                // (module-port module)
                Scm::func(module_minus_port).invoke(&[module__279.clone()])
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
        let module__281 = args[0].clone();
        {
            // (caddr module)
            imports::caddr(&[module__281.clone()])
        }
    }
    .into()
}
pub fn module_minus_port(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let module__280 = args[0].clone();
        {
            // (cadr module)
            imports::cadr(&[module__280.clone()])
        }
    }
    .into()
}
pub fn module_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__273 = args[0].clone();
        {
            // (and (pair? obj) (eq? (quote module) (car obj)))
            if ({
                // (pair? obj)
                imports::pair_p(&[obj__273.clone()])
            })
            .is_true()
            {
                {
                    // (eq? (quote module) (car obj))
                    imports::eq_p(&[Scm::symbol("module"), {
                        // (car obj)
                        imports::car(&[obj__273.clone()])
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
        let name__275 = args[0].clone();
        let base_minus_path__274 = args[1].clone();
        {
            // (let ((path (string-append base-path "/" (rustify-libname name)))) (create-directory* path) (list (quote module) (open-output-file (string-append path "/mod.rs")) path))
            {
                let path__276 = {
                    // (string-append base-path "/" (rustify-libname name))
                    imports::string_minus_append(&[base_minus_path__274.clone(), Scm::from("/"), {
                        // (rustify-libname name)
                        imports::rustify_minus_libname(&[name__275.clone()])
                    }])
                };
                {
                    {
                        // (create-directory* path)
                        imports::create_minus_directory_star_(&[path__276.clone()])
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
                                        path__276.clone(),
                                        Scm::from("/mod.rs"),
                                    ])
                                }])
                            },
                            path__276.clone(),
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
        let name__277 = args[0].clone();
        let module__278 = args[1].clone();
        {
            // (open-module name (module-path module))
            open_minus_module(&[name__277.clone(), {
                // (module-path module)
                Scm::func(module_minus_path).invoke(&[module__278.clone()])
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
        let f__288 = args[0].clone();
        let args__289 = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (display a (as-port f))) args)
            imports::for_minus_each(&[
                {
                    // Closure
                    let f__288 = f__288.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let a__287 = args[0].clone();
                        {
                            // (display a (as-port f))
                            imports::display(&[a__287.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f__288.clone()])
                            }])
                        }
                    })
                },
                args__289.clone(),
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
        let f__285 = args[0].clone();
        let args__286 = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (display a (as-port f))) args)
                imports::for_minus_each(&[
                    {
                        // Closure
                        let f__285 = f__285.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a__284 = args[0].clone();
                            {
                                // (display a (as-port f))
                                imports::display(&[a__284.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f__285.clone()])
                                }])
                            }
                        })
                    },
                    args__286.clone(),
                ])
            };
            {
                // (newline (as-port f))
                imports::newline(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f__285.clone()])
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
        let module__282 = args[0].clone();
        let code__283 = args[1].clone();
        {
            {
                // (print module "{")
                Scm::func(print).invoke(&[module__282.clone(), Scm::from("{")])
            };
            {
                // (code)
                code__283.clone().invoke(&[])
            };
            {
                // (print module "}")
                Scm::func(print).invoke(&[module__282.clone(), Scm::from("}")])
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
        let f__294 = args[0].clone();
        let args__295 = Scm::list(&args[1..]);
        {
            // (for-each (lambda (a) (write a (as-port f))) args)
            imports::for_minus_each(&[
                {
                    // Closure
                    let f__294 = f__294.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let a__293 = args[0].clone();
                        {
                            // (write a (as-port f))
                            imports::write(&[a__293.clone(), {
                                // (as-port f)
                                Scm::func(as_minus_port).invoke(&[f__294.clone()])
                            }])
                        }
                    })
                },
                args__295.clone(),
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
        let f__291 = args[0].clone();
        let args__292 = Scm::list(&args[1..]);
        {
            {
                // (for-each (lambda (a) (write a (as-port f))) args)
                imports::for_minus_each(&[
                    {
                        // Closure
                        let f__291 = f__291.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let a__290 = args[0].clone();
                            {
                                // (write a (as-port f))
                                imports::write(&[a__290.clone(), {
                                    // (as-port f)
                                    Scm::func(as_minus_port).invoke(&[f__291.clone()])
                                }])
                            }
                        })
                    },
                    args__292.clone(),
                ])
            };
            {
                // (newline (as-port f))
                imports::newline(&[{
                    // (as-port f)
                    Scm::func(as_minus_port).invoke(&[f__291.clone()])
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
