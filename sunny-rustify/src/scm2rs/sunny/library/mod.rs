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
    pub use super::globals::check_minus_imports;
    pub use super::globals::get_minus_lib;
    pub use super::globals::library_minus_decls;
    pub use super::globals::library_minus_exports;
    pub use super::globals::library_minus_name;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static check_minus_imports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL check-imports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library_minus_ext: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library-ext"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static get_minus_lib: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL get-lib"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_path: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-path"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_exports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-exports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-name"))}
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
        // (define (library-name expr) ...)
        globals::library_minus_name.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (cadr expr)
                    imports::cadr
                        .with(|value| value.get())
                        .invoke(&[expr.clone()])
                })
            })
        });
        // (define (library-decls expr) ...)
        globals::library_minus_decls.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (cddr expr)
                    imports::cddr
                        .with(|value| value.get())
                        .invoke(&[expr.clone()])
                })
            })
        });
        // (define (library-exports lib-decl*) ...)
        globals::library_minus_exports.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib_minus_decl_star_ = args[0].clone();
                    // (cond ...)
                    if (
                        // (null? lib-decl*)
                        imports::null_p
                            .with(|value| value.get())
                            .invoke(&[lib_minus_decl_star_.clone()])
                    )
                    .is_true()
                    {
                        Scm::Nil
                    } else if (
                        // (eq? (quote export) (caar lib-decl*))
                        imports::eq_p.with(|value| value.get()).invoke(&[
                            Scm::symbol("export"),
                            // (caar lib-decl*)
                            imports::caar
                                .with(|value| value.get())
                                .invoke(&[lib_minus_decl_star_.clone()]),
                        ])
                    )
                    .is_true()
                    {
                        // (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))
                        globals::append.with(|value| value.get()).invoke(&[
                            // (cdar lib-decl*)
                            imports::cdar
                                .with(|value| value.get())
                                .invoke(&[lib_minus_decl_star_.clone()]),
                            // (library-exports (cdr lib-decl*))
                            globals::library_minus_exports
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr lib-decl*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[lib_minus_decl_star_.clone()]),
                                ]),
                        ])
                    } else {
                        // (library-exports (cdr lib-decl*))
                        globals::library_minus_exports
                            .with(|value| value.get())
                            .invoke(&[
                                // (cdr lib-decl*)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[lib_minus_decl_star_.clone()]),
                            ])
                    }
                })
            })
        });
        // (define (get-lib lib) ...)
        globals::get_minus_lib.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    // (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib)))
                    {
                        let [full_minus_path] = [
                            // (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx")))
                            globals::find_minus_library
                                .with(|value| value.get())
                                .invoke(&[
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
                                                            Scm::pair(
                                                                Scm::from("../../scm-libs"),
                                                                Scm::Nil,
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                    // (library-path lib)
                                    globals::library_minus_path
                                        .with(|value| value.get())
                                        .invoke(&[lib.clone()]),
                                    Scm::pair(
                                        Scm::from(".sld"),
                                        Scm::pair(Scm::from(".slx"), Scm::Nil),
                                    ),
                                ]),
                        ];
                        if (full_minus_path.clone()).is_true() {
                            // (read (open-input-file full-path))
                            imports::read.with(|value| value.get()).invoke(&[
                                // (open-input-file full-path)
                                imports::open_minus_input_minus_file
                                    .with(|value| value.get())
                                    .invoke(&[full_minus_path.clone()]),
                            ])
                        } else {
                            // (error "Unknown library" lib)
                            imports::error
                                .with(|value| value.get())
                                .invoke(&[Scm::from("Unknown library"), lib.clone()])
                        }
                    }
                })
            })
        });
        // (define (find-library base-path* relative-path extension*) ...)
        globals::find_minus_library.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let base_minus_path_star_ = args[0].clone();
                    let relative_minus_path = args[1].clone();
                    let extension_star_ = args[2].clone();
                    if (
                        // (null? base-path*)
                        imports::null_p
                            .with(|value| value.get())
                            .invoke(&[base_minus_path_star_.clone()])
                    )
                    .is_true()
                    {
                        Scm::False
                    } else {
                        // (let* ((path (string-append (car base-path*) relative-path)) (full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))

                        // (let ((path (string-append (car base-path*) relative-path))) (let ((full-path (find-library-ext path extension*))) (begin (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))))
                        {
                            let [path] = [
                                // (string-append (car base-path*) relative-path)
                                imports::string_minus_append
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car base-path*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[base_minus_path_star_.clone()]),
                                        relative_minus_path.clone(),
                                    ]),
                            ];
                            // (let ((full-path (find-library-ext path extension*))) (begin (if full-path full-path (find-library (cdr base-path*) relative-path extension*))))
                            {
                                let [full_minus_path] = [
                                    // (find-library-ext path extension*)
                                    globals::find_minus_library_minus_ext
                                        .with(|value| value.get())
                                        .invoke(&[path.clone(), extension_star_.clone()]),
                                ];
                                if (full_minus_path.clone()).is_true() {
                                    full_minus_path.clone()
                                } else {
                                    // (find-library (cdr base-path*) relative-path extension*)
                                    globals::find_minus_library
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (cdr base-path*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[base_minus_path_star_.clone()]),
                                            relative_minus_path.clone(),
                                            extension_star_.clone(),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (find-library-ext path extension*) ...)
        globals::find_minus_library_minus_ext.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let path = args[0].clone();
                    let extension_star_ = args[1].clone();
                    if (
                        // (null? extension*)
                        imports::null_p
                            .with(|value| value.get())
                            .invoke(&[extension_star_.clone()])
                    )
                    .is_true()
                    {
                        Scm::False
                    } else {
                        // (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))
                        {
                            let [full_minus_path] = [
                                // (string-append path (car extension*))
                                imports::string_minus_append
                                    .with(|value| value.get())
                                    .invoke(&[
                                        path.clone(),
                                        // (car extension*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[extension_star_.clone()]),
                                    ]),
                            ];
                            if (
                                // (file-exists? full-path)
                                imports::file_minus_exists_p
                                    .with(|value| value.get())
                                    .invoke(&[full_minus_path.clone()])
                            )
                            .is_true()
                            {
                                full_minus_path.clone()
                            } else {
                                // (find-library-ext path (cdr extension*))
                                globals::find_minus_library_minus_ext
                                    .with(|value| value.get())
                                    .invoke(&[
                                        path.clone(),
                                        // (cdr extension*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[extension_star_.clone()]),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (library-path lib) ...)
        globals::library_minus_path.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    // (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib))
                    imports::reduce.with(|value| value.get()).invoke(&[
                        {
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let left = args[0].clone();
                                let right = args[1].clone();
                                // (string-append left (string-append "/" right))
                                imports::string_minus_append
                                    .with(|value| value.get())
                                    .invoke(&[
                                        left.clone(),
                                        // (string-append "/" right)
                                        imports::string_minus_append
                                            .with(|value| value.get())
                                            .invoke(&[Scm::from("/"), right.clone()]),
                                    ])
                            })
                        },
                        Scm::from(""),
                        // (map symbol->string lib)
                        imports::map.with(|value| value.get()).invoke(&[
                            imports::symbol_minus__g_string.with(|value| value.get()),
                            lib.clone(),
                        ]),
                    ])
                })
            })
        });
        // (define (check-imports imports exports lib) ...)
        globals::check_minus_imports.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let imports = args[0].clone();
                    let exports = args[1].clone();
                    let lib = args[2].clone();
                    if (
                        // (null? imports)
                        imports::null_p
                            .with(|value| value.get())
                            .invoke(&[imports.clone()])
                    )
                    .is_true()
                    {
                        Scm::True
                    } else if (
                        // (memq (car imports) exports)
                        imports::memq.with(|value| value.get()).invoke(&[
                            // (car imports)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[imports.clone()]),
                            exports.clone(),
                        ])
                    )
                    .is_true()
                    {
                        // (check-imports (cdr imports) exports lib)
                        globals::check_minus_imports
                            .with(|value| value.get())
                            .invoke(&[
                                // (cdr imports)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[imports.clone()]),
                                exports.clone(),
                                lib.clone(),
                            ])
                    } else {
                        // (error "Invalid import" (car imports) lib)
                        imports::error.with(|value| value.get()).invoke(&[
                            Scm::from("Invalid import"),
                            // (car imports)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[imports.clone()]),
                            lib.clone(),
                        ])
                    }
                })
            })
        })
    };
}
