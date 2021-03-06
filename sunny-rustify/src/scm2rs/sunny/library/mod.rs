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
        let imports_3 = args[0].clone();
        let exports_3 = args[1].clone();
        let lib_10 = args[2].clone();
        if ({
            // (null? imports)
            imports::null_p(&[imports_3.clone()])
        })
        .is_true()
        {
            Scm::True
        } else if ({
            // (memq (car imports) exports)
            imports::memq(&[
                {
                    // (car imports)
                    imports::car(&[imports_3.clone()])
                },
                exports_3.clone(),
            ])
        })
        .is_true()
        {
            {
                // (check-imports (cdr imports) exports lib)
                Scm::func(check_minus_imports).invoke(&[
                    {
                        // (cdr imports)
                        imports::cdr(&[imports_3.clone()])
                    },
                    exports_3.clone(),
                    lib_10.clone(),
                ])
            }
        } else {
            {
                // (error "Invalid import" (car imports) lib)
                imports::error(&[
                    Scm::from("Invalid import"),
                    {
                        // (car imports)
                        imports::car(&[imports_3.clone()])
                    },
                    lib_10.clone(),
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
        let base_minus_path_star__0 = args[0].clone();
        let relative_minus_path_0 = args[1].clone();
        let extension_star__0 = args[2].clone();
        if ({
            // (null? base-path*)
            imports::null_p(&[base_minus_path_star__0.clone()])
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
                        let path_1 = {
                            // (string-append (car base-path*) relative-path)
                            imports::string_minus_append(&[
                                {
                                    // (car base-path*)
                                    imports::car(&[base_minus_path_star__0.clone()])
                                },
                                relative_minus_path_0.clone(),
                            ])
                        };
                        // (let ((full-path (find-library-ext path extension*))) (begin (if full-path full-path (find-library (cdr base-path*) relative-path extension*))))
                        let full_minus_path_1 = {
                            // (find-library-ext path extension*)
                            Scm::func(find_minus_library_minus_ext)
                                .invoke(&[path_1.clone(), extension_star__0.clone()])
                        };
                        if (full_minus_path_1.clone()).is_true() {
                            full_minus_path_1.clone()
                        } else {
                            {
                                // (find-library (cdr base-path*) relative-path extension*)
                                Scm::func(find_minus_library).invoke(&[
                                    {
                                        // (cdr base-path*)
                                        imports::cdr(&[base_minus_path_star__0.clone()])
                                    },
                                    relative_minus_path_0.clone(),
                                    extension_star__0.clone(),
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
        let path_2 = args[0].clone();
        let extension_star__1 = args[1].clone();
        if ({
            // (null? extension*)
            imports::null_p(&[extension_star__1.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))
                {
                    let full_minus_path_2 = {
                        // (string-append path (car extension*))
                        imports::string_minus_append(&[path_2.clone(), {
                            // (car extension*)
                            imports::car(&[extension_star__1.clone()])
                        }])
                    };
                    if ({
                        // (file-exists? full-path)
                        imports::file_minus_exists_p(&[full_minus_path_2.clone()])
                    })
                    .is_true()
                    {
                        full_minus_path_2.clone()
                    } else {
                        {
                            // (find-library-ext path (cdr extension*))
                            Scm::func(find_minus_library_minus_ext).invoke(&[path_2.clone(), {
                                // (cdr extension*)
                                imports::cdr(&[extension_star__1.clone()])
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
        let lib_8 = args[0].clone();
        {
            // (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib)))
            {
                let full_minus_path_0 = {
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
                            Scm::func(library_minus_path).invoke(&[lib_8.clone()])
                        },
                        Scm::pair(Scm::from(".sld"), Scm::pair(Scm::from(".slx"), Scm::Nil)),
                    ])
                };
                if (full_minus_path_0.clone()).is_true() {
                    {
                        // (read (open-input-file full-path))
                        imports::read(&[{
                            // (open-input-file full-path)
                            imports::open_minus_input_minus_file(&[full_minus_path_0.clone()])
                        }])
                    }
                } else {
                    {
                        // (error "Unknown library" lib)
                        imports::error(&[Scm::from("Unknown library"), lib_8.clone()])
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
        let expr_1 = args[0].clone();
        {
            // (cddr expr)
            imports::cddr(&[expr_1.clone()])
        }
    }
    .into()
}
pub fn library_minus_exports(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let lib_minus_decl_star__0 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (null? lib-decl*)
                imports::null_p(&[lib_minus_decl_star__0.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else if ({
                // (eq? (quote export) (caar lib-decl*))
                imports::eq_p(&[Scm::symbol("export"), {
                    // (caar lib-decl*)
                    imports::caar(&[lib_minus_decl_star__0.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))
                    imports::append(&[
                        {
                            // (cdar lib-decl*)
                            imports::cdar(&[lib_minus_decl_star__0.clone()])
                        },
                        {
                            // (library-exports (cdr lib-decl*))
                            Scm::func(library_minus_exports).invoke(&[{
                                // (cdr lib-decl*)
                                imports::cdr(&[lib_minus_decl_star__0.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (library-exports (cdr lib-decl*))
                    Scm::func(library_minus_exports).invoke(&[{
                        // (cdr lib-decl*)
                        imports::cdr(&[lib_minus_decl_star__0.clone()])
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
        let expr_0 = args[0].clone();
        {
            // (cadr expr)
            imports::cadr(&[expr_0.clone()])
        }
    }
    .into()
}
pub fn library_minus_path(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let lib_9 = args[0].clone();
        {
            // (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib))
            imports::reduce(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let left_0 = args[0].clone();
                        let right_0 = args[1].clone();
                        {
                            // (string-append left (string-append "/" right))
                            imports::string_minus_append(&[left_0.clone(), {
                                // (string-append "/" right)
                                imports::string_minus_append(&[Scm::from("/"), right_0.clone()])
                            }])
                        }
                    })
                },
                Scm::from(""),
                {
                    // (map symbol->string lib)
                    imports::map(&[Scm::func(imports::symbol_minus__g_string), lib_9.clone()])
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
