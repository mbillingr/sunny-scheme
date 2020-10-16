#[allow(unused_imports)]
use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::process_context::exports::command_minus_line;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::ast_transforms::boxify::exports::*;
    pub use crate::sunny::ast_transforms::close_procedures::exports::*;
    pub use crate::sunny::ast_transforms::extract_definitions::exports::*;
    pub use crate::sunny::ast_transforms::rename_vars::exports::*;
    pub use crate::sunny::astify_toplevel::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::table::exports::*;
    pub use crate::sunny::variable::exports::*;
    pub use crate::testsuite::exports::*;
}

thread_local! {#[allow(non_upper_case_globals)] pub static UNIQUE_minus_COUNTS: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE UNIQUE_minus_COUNTS"))}
thread_local! {#[allow(non_upper_case_globals)] pub static args_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE args_"))}
thread_local! {#[allow(non_upper_case_globals)] pub static ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE ast"))}
thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE input_minus_file"))}
thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE input_minus_file_minus_name"))}
pub fn load_minus_sexpr(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr))))
            {
                let expr_27 = {
                    // (read input-file)
                    imports::read(&[input_minus_file.with(|value| value.get())])
                };
                if ({
                    // (eof-object? expr)
                    imports::eof_minus_object_p(&[expr_27.clone()])
                })
                .is_true()
                {
                    Scm::Nil
                } else {
                    {
                        // (cons expr (load-sexpr))
                        imports::cons(&[expr_27.clone(), {
                            // (load-sexpr)
                            Scm::func(load_minus_sexpr).invoke(&[])
                        }])
                    }
                }
            }
        }
    }
    .into()
}
thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_dir: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE output_minus_dir"))}
thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_module_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE output_minus_module_minus_name"))}
thread_local! {#[allow(non_upper_case_globals)] pub static program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE program"))}
pub fn rust_minus_pipeline(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let scheme_minus_ast_0 = args[0].clone();
        {
            // (extract-definitions (boxify (rename-vars (lambda (name var) (let* ((str-name (if (string? name) name (symbol->string name))) (rust-name (rustify-identifier str-name))) (if (local-variable? var) (unique-name rust-name) rust-name))) (close-procedures scheme-ast))))
            imports::extract_minus_definitions(&[{
                // (boxify (rename-vars (lambda (name var) (let* ((str-name (if (string? name) name (symbol->string name))) (rust-name (rustify-identifier str-name))) (if (local-variable? var) (unique-name rust-name) rust-name))) (close-procedures scheme-ast)))
                imports::boxify(&[{
                    // (rename-vars (lambda (name var) (let* ((str-name (if (string? name) name (symbol->string name))) (rust-name (rustify-identifier str-name))) (if (local-variable? var) (unique-name rust-name) rust-name))) (close-procedures scheme-ast))
                    imports::rename_minus_vars(&[
                        {
                            // Closure
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let name_46 = args[0].clone();
                                let var_26 = args[1].clone();
                                {
                                    // (let* ((str-name (if (string? name) name (symbol->string name))) (rust-name (rustify-identifier str-name))) (if (local-variable? var) (unique-name rust-name) rust-name))
                                    {
                                        // (let ((str-name (if (string? name) name (symbol->string name)))) (let ((rust-name (rustify-identifier str-name))) (begin (if (local-variable? var) (unique-name rust-name) rust-name))))
                                        {
                                            let str_minus_name_0 = if ({
                                                // (string? name)
                                                imports::string_p(&[name_46.clone()])
                                            })
                                            .is_true()
                                            {
                                                name_46.clone()
                                            } else {
                                                {
                                                    // (symbol->string name)
                                                    imports::symbol_minus__g_string(&[
                                                        name_46.clone()
                                                    ])
                                                }
                                            };
                                            // (let ((rust-name (rustify-identifier str-name))) (begin (if (local-variable? var) (unique-name rust-name) rust-name)))
                                            let rust_minus_name_0 = {
                                                // (rustify-identifier str-name)
                                                imports::rustify_minus_identifier(&[
                                                    str_minus_name_0.clone(),
                                                ])
                                            };
                                            if ({
                                                // (local-variable? var)
                                                imports::local_minus_variable_p(&[var_26.clone()])
                                            })
                                            .is_true()
                                            {
                                                {
                                                    // (unique-name rust-name)
                                                    unique_minus_name(&[rust_minus_name_0.clone()])
                                                }
                                            } else {
                                                rust_minus_name_0.clone()
                                            }
                                        }
                                    }
                                }
                            })
                        },
                        {
                            // (close-procedures scheme-ast)
                            imports::close_minus_procedures(&[scheme_minus_ast_0.clone()])
                        },
                    ])
                }])
            }])
        }
    }
    .into()
}
pub fn unique_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_45 = args[0].clone();
        {
            // (let* ((entry (assoc name UNIQUE-COUNTS)) (nr (cond (entry (set-cdr! entry (+ 1 (cdr entry))) (cdr entry)) (else (set! UNIQUE-COUNTS (cons (cons name 0) UNIQUE-COUNTS)) 0)))) (string-append name "_" (number->string nr)))
            {
                // (let ((entry (assoc name UNIQUE-COUNTS))) (let ((nr (cond (entry (set-cdr! entry (+ 1 (cdr entry))) (cdr entry)) (else (set! UNIQUE-COUNTS (cons (cons name 0) UNIQUE-COUNTS)) 0)))) (begin (string-append name "_" (number->string nr)))))
                {
                    let entry_4 = {
                        // (assoc name UNIQUE-COUNTS)
                        imports::assoc(&[
                            name_45.clone(),
                            UNIQUE_minus_COUNTS.with(|value| value.get()),
                        ])
                    };
                    // (let ((nr (cond (entry (set-cdr! entry (+ 1 (cdr entry))) (cdr entry)) (else (set! UNIQUE-COUNTS (cons (cons name 0) UNIQUE-COUNTS)) 0)))) (begin (string-append name "_" (number->string nr))))
                    let nr_0 = {
                        // (cond ...)
                        if (entry_4.clone()).is_true() {
                            {
                                {
                                    // (set-cdr! entry (+ 1 (cdr entry)))
                                    imports::set_minus_cdr_i(&[entry_4.clone(), {
                                        // (+ 1 (cdr entry))
                                        imports::_plus_(&[Scm::from(1), {
                                            // (cdr entry)
                                            imports::cdr(&[entry_4.clone()])
                                        }])
                                    }])
                                };
                                {
                                    // (cdr entry)
                                    imports::cdr(&[entry_4.clone()])
                                }
                            }
                        } else {
                            {
                                UNIQUE_minus_COUNTS.with(|value| {
                                    value.set({
                                        // (cons (cons name 0) UNIQUE-COUNTS)
                                        imports::cons(&[
                                            {
                                                // (cons name 0)
                                                imports::cons(&[name_45.clone(), Scm::from(0)])
                                            },
                                            UNIQUE_minus_COUNTS.with(|value| value.get()),
                                        ])
                                    })
                                });
                                Scm::anything();
                                Scm::from(0)
                            }
                        }
                    };
                    {
                        // (string-append name "_" (number->string nr))
                        imports::string_minus_append(&[name_45.clone(), Scm::from("_"), {
                            // (number->string nr)
                            imports::number_minus__g_string(&[nr_0.clone()])
                        }])
                    }
                }
            }
        }
    }
    .into()
}

pub fn main() {
    eprintln!("built with");
    eprintln!("    '{}' memory model", MEMORY_MODEL_KIND);

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::file::initialize();
    crate::scheme::read::initialize();
    crate::scheme::write::initialize();
    crate::scheme::process_context::initialize();
    crate::sunny::ast_transforms::boxify::initialize();
    crate::sunny::ast_transforms::close_procedures::initialize();
    crate::sunny::ast_transforms::extract_definitions::initialize();
    crate::sunny::ast_transforms::rename_vars::initialize();
    crate::sunny::astify_toplevel::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::table::initialize();
    crate::sunny::variable::initialize();
    crate::testsuite::initialize();
    {
        {
            // (define UNIQUE-COUNTS (quote ()))
            UNIQUE_minus_COUNTS.with(|value| value.set(Scm::Nil));
            Scm::anything()
        };
        {
            // (define (unique-name name) ...)
            (/*NOP*/)
        };
        {
            // (define (rust-pipeline scheme-ast) ...)
            (/*NOP*/)
        };
        {
            // (define args (command-line))
            args_.with(|value| {
                value.set({
                    // (command-line)
                    imports::command_minus_line(&[])
                })
            });
            Scm::anything()
        };
        {
            // (define input-file-name (cadr args))
            input_minus_file_minus_name.with(|value| {
                value.set({
                    // (cadr args)
                    imports::cadr(&[args_.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (define output-module-name (caddr args))
            output_minus_module_minus_name.with(|value| {
                value.set({
                    // (caddr args)
                    imports::caddr(&[args_.with(|value| value.get())])
                })
            });
            Scm::anything()
        };
        {
            // (define output-dir (if (pair? (cdddr args)) (cadddr args) "."))
            output_minus_dir.with(|value| {
                value.set(
                    if ({
                        // (pair? (cdddr args))
                        imports::pair_p(&[{
                            // (cdddr args)
                            imports::cdddr(&[args_.with(|value| value.get())])
                        }])
                    })
                    .is_true()
                    {
                        {
                            // (cadddr args)
                            imports::cadddr(&[args_.with(|value| value.get())])
                        }
                    } else {
                        Scm::from(".")
                    },
                )
            });
            Scm::anything()
        };
        {
            // (newline)
            imports::newline(&[])
        };
        {
            // (display input-file-name)
            imports::display(&[input_minus_file_minus_name.with(|value| value.get())])
        };
        {
            // (display " --> ")
            imports::display(&[Scm::from(" --> ")])
        };
        {
            // (display output-dir)
            imports::display(&[output_minus_dir.with(|value| value.get())])
        };
        {
            // (display "/")
            imports::display(&[Scm::from("/")])
        };
        {
            // (display output-module-name)
            imports::display(&[output_minus_module_minus_name.with(|value| value.get())])
        };
        {
            // (newline)
            imports::newline(&[])
        };
        {
            // (newline)
            imports::newline(&[])
        };
        {
            // (define input-file (open-input-file input-file-name))
            input_minus_file.with(|value| {
                value.set({
                    // (open-input-file input-file-name)
                    imports::open_minus_input_minus_file(&[
                        input_minus_file_minus_name.with(|value| value.get())
                    ])
                })
            });
            Scm::anything()
        };
        {
            // (define (load-sexpr) ...)
            (/*NOP*/)
        };
        {
            // (define program (load-sexpr))
            program.with(|value| {
                value.set({
                    // (load-sexpr)
                    load_minus_sexpr(&[])
                })
            });
            Scm::anything()
        };
        {
            // (define ast (astify-toplevel program rust-pipeline))
            ast.with(|value| {
                value.set({
                    // (astify-toplevel program rust-pipeline)
                    imports::astify_minus_toplevel(&[
                        program.with(|value| value.get()),
                        Scm::func(rust_minus_pipeline),
                    ])
                })
            });
            Scm::anything()
        };
        {
            // (rust-gen-in-module output-module-name output-dir (lambda (module) (ast (quote gen-rust) module)))
            imports::rust_minus_gen_minus_in_minus_module(&[
                output_minus_module_minus_name.with(|value| value.get()),
                output_minus_dir.with(|value| value.get()),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let module_46 = args[0].clone();
                        {
                            // (ast (quote gen-rust) module)
                            ast.with(|value| value.get())
                                .invoke(&[Scm::symbol("gen-rust"), module_46.clone()])
                        }
                    })
                },
            ])
        }
    };
}
pub mod chibi;
pub mod scheme;
pub mod sunny;
pub mod testsuite;
