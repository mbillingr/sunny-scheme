#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::check_minus_imports;
    pub use super::get_minus_lib;
    pub use super::library_minus_decls;
    pub use super::library_minus_exports;
    pub use super::library_minus_name;
}

pub fn check_minus_imports(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let imports = args[0].clone();
        let exports = args[1].clone();
        let lib = args[2].clone();
        if ({
            // (null? imports)
            Scm::func(imports::null_p).invoke(&[imports.clone()])
        })
        .is_true()
        {
            Scm::True
        } else if ({
            // (memq (car imports) exports)
            Scm::func(imports::memq).invoke(&[
                {
                    // (car imports)
                    Scm::func(imports::car).invoke(&[imports.clone()])
                },
                exports.clone(),
            ])
        })
        .is_true()
        {
            {
                // (check-imports (cdr imports) exports lib)
                Scm::func(check_minus_imports).invoke(&[
                    {
                        // (cdr imports)
                        Scm::func(imports::cdr).invoke(&[imports.clone()])
                    },
                    exports.clone(),
                    lib.clone(),
                ])
            }
        } else {
            {
                // (error "Invalid import" (car imports) lib)
                Scm::func(imports::error).invoke(&[
                    Scm::from("Invalid import"),
                    {
                        // (car imports)
                        Scm::func(imports::car).invoke(&[imports.clone()])
                    },
                    lib.clone(),
                ])
            }
        }
    }
    .into()
}
pub fn find_minus_library(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let base_minus_path_star_ = args[0].clone();
        let relative_minus_path = args[1].clone();
        let extension_star_ = args[2].clone();
        if ({
            // (null? base-path*)
            Scm::func(imports::null_p).invoke(&[base_minus_path_star_.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (let* ((path (string-append (car base-path*) relative-path)) (full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))
                {
                    // (let ((path (string-append (car base-path*) relative-path))) (let ((full-path (find-library-ext path extension*))) (begin (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))))
                    {
                        let path = {
                            // (string-append (car base-path*) relative-path)
                            Scm::func(imports::string_minus_append).invoke(&[
                                {
                                    // (car base-path*)
                                    Scm::func(imports::car).invoke(&[base_minus_path_star_.clone()])
                                },
                                relative_minus_path.clone(),
                            ])
                        };
                        // (let ((full-path (find-library-ext path extension*))) (begin (if full-path full-path (find-library (cdr base-path*) relative-path extension*))))
                        let full_minus_path = {
                            // (find-library-ext path extension*)
                            Scm::func(find_minus_library_minus_ext)
                                .invoke(&[path.clone(), extension_star_.clone()])
                        };
                        if (full_minus_path.clone()).is_true() {
                            full_minus_path.clone()
                        } else {
                            {
                                // (find-library (cdr base-path*) relative-path extension*)
                                Scm::func(find_minus_library).invoke(&[
                                    {
                                        // (cdr base-path*)
                                        Scm::func(imports::cdr)
                                            .invoke(&[base_minus_path_star_.clone()])
                                    },
                                    relative_minus_path.clone(),
                                    extension_star_.clone(),
                                ])
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn find_minus_library_minus_ext(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let path = args[0].clone();
        let extension_star_ = args[1].clone();
        if ({
            // (null? extension*)
            Scm::func(imports::null_p).invoke(&[extension_star_.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))
                {
                    let full_minus_path = {
                        // (string-append path (car extension*))
                        Scm::func(imports::string_minus_append).invoke(&[path.clone(), {
                            // (car extension*)
                            Scm::func(imports::car).invoke(&[extension_star_.clone()])
                        }])
                    };
                    if ({
                        // (file-exists? full-path)
                        Scm::func(imports::file_minus_exists_p).invoke(&[full_minus_path.clone()])
                    })
                    .is_true()
                    {
                        full_minus_path.clone()
                    } else {
                        {
                            // (find-library-ext path (cdr extension*))
                            Scm::func(find_minus_library_minus_ext).invoke(&[path.clone(), {
                                // (cdr extension*)
                                Scm::func(imports::cdr).invoke(&[extension_star_.clone()])
                            }])
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn get_minus_lib(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let lib = args[0].clone();
        {
            // (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib)))
            {
                let full_minus_path = {
                    // (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx")))
                    Scm::func(find_minus_library).invoke(&[
                        Scm::pair(
                            Scm::from("."),
                            Scm::pair(
                                Scm::from("./lib"),
                                Scm::pair(
                                    Scm::from("./scheme/lib"),
                                    Scm::pair(
                                        Scm::from("scm-libs"),
                                        Scm::pair(
                                            Scm::from("../scheme/lib"),
                                            Scm::pair(
                                                Scm::from("../scm-libs"),
                                                Scm::pair(Scm::from("../../scm-libs"), Scm::Nil),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        {
                            // (library-path lib)
                            Scm::func(library_minus_path).invoke(&[lib.clone()])
                        },
                        Scm::pair(Scm::from(".sld"), Scm::pair(Scm::from(".slx"), Scm::Nil)),
                    ])
                };
                if (full_minus_path.clone()).is_true() {
                    {
                        // (read (open-input-file full-path))
                        Scm::func(imports::read).invoke(&[{
                            // (open-input-file full-path)
                            Scm::func(imports::open_minus_input_minus_file)
                                .invoke(&[full_minus_path.clone()])
                        }])
                    }
                } else {
                    {
                        // (error "Unknown library" lib)
                        Scm::func(imports::error)
                            .invoke(&[Scm::from("Unknown library"), lib.clone()])
                    }
                }
            }
        }
    }
    .into()
}
pub fn library_minus_decls(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cddr expr)
            Scm::func(imports::cddr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn library_minus_exports(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let lib_minus_decl_star_ = args[0].clone();
        {
            // (cond ...)
            if ({
                // (null? lib-decl*)
                Scm::func(imports::null_p).invoke(&[lib_minus_decl_star_.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else if ({
                // (eq? (quote export) (caar lib-decl*))
                Scm::func(imports::eq_p).invoke(&[Scm::symbol("export"), {
                    // (caar lib-decl*)
                    Scm::func(imports::caar).invoke(&[lib_minus_decl_star_.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))
                    Scm::func(imports::append).invoke(&[
                        {
                            // (cdar lib-decl*)
                            Scm::func(imports::cdar).invoke(&[lib_minus_decl_star_.clone()])
                        },
                        {
                            // (library-exports (cdr lib-decl*))
                            Scm::func(library_minus_exports).invoke(&[{
                                // (cdr lib-decl*)
                                Scm::func(imports::cdr).invoke(&[lib_minus_decl_star_.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (library-exports (cdr lib-decl*))
                    Scm::func(library_minus_exports).invoke(&[{
                        // (cdr lib-decl*)
                        Scm::func(imports::cdr).invoke(&[lib_minus_decl_star_.clone()])
                    }])
                }
            }
        }
    }
    .into()
}
pub fn library_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let expr = args[0].clone();
        {
            // (cadr expr)
            Scm::func(imports::cadr).invoke(&[expr.clone()])
        }
    }
    .into()
}
pub fn library_minus_path(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let lib = args[0].clone();
        {
            // (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib))
            Scm::func(imports::reduce).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let left = args[0].clone();
                        let right = args[1].clone();
                        {
                            // (string-append left (string-append "/" right))
                            Scm::func(imports::string_minus_append).invoke(&[left.clone(), {
                                // (string-append "/" right)
                                Scm::func(imports::string_minus_append)
                                    .invoke(&[Scm::from("/"), right.clone()])
                            }])
                        }
                    })
                },
                Scm::from(""),
                {
                    // (map symbol->string lib)
                    Scm::func(imports::map)
                        .invoke(&[Scm::func(imports::symbol_minus__g_string), lib.clone()])
                },
            ])
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
    crate::scheme::file::initialize();
    crate::scheme::read::initialize();
    crate::scheme::write::initialize();
    crate::sunny::env::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (library-name expr) ...)
            (/*NOP*/)
        };
        {
            // (define (library-decls expr) ...)
            (/*NOP*/)
        };
        {
            // (define (library-exports lib-decl*) ...)
            (/*NOP*/)
        };
        {
            // (define (get-lib lib) ...)
            (/*NOP*/)
        };
        {
            // (define (find-library base-path* relative-path extension*) ...)
            (/*NOP*/)
        };
        {
            // (define (find-library-ext path extension*) ...)
            (/*NOP*/)
        };
        {
            // (define (library-path lib) ...)
            (/*NOP*/)
        };
        {
            // (define (check-imports imports exports lib) ...)
            (/*NOP*/)
        }
    };
}
