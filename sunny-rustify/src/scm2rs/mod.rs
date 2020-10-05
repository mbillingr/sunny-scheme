#[allow(unused_imports)]
use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::process_context::exports::command_minus_line;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::astify_toplevel::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::table::exports::*;
    pub use crate::testsuite::exports::*;
}

thread_local! {#[allow(non_upper_case_globals)] pub static args_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE args"))}
thread_local! {#[allow(non_upper_case_globals)] pub static ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE ast"))}
thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE input-file"))}
thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE input-file-name"))}
pub fn load_minus_sexpr(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr))))
            {
                let expr = {
                    // (read input-file)
                    Scm::func(imports::read).invoke(&[input_minus_file.with(|value| value.get())])
                };
                if ({
                    // (eof-object? expr)
                    Scm::func(imports::eof_minus_object_p).invoke(&[expr.clone()])
                })
                .is_true()
                {
                    Scm::Nil
                } else {
                    {
                        // (cons expr (load-sexpr))
                        Scm::func(imports::cons).invoke(&[expr.clone(), {
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
thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_dir: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE output-dir"))}
thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_module_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE output-module-name"))}
thread_local! {#[allow(non_upper_case_globals)] pub static program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL VARIABLE program"))}

pub fn main() {
    eprintln!("built with");
    eprintln!("    '{}' memory model", MEMORY_MODEL_KIND);

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::file::initialize();
    crate::scheme::read::initialize();
    crate::scheme::write::initialize();
    crate::scheme::process_context::initialize();
    crate::sunny::astify_toplevel::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::table::initialize();
    crate::testsuite::initialize();
    {
        {
            // (define args (command-line))
            args_.with(|value| {
                value.set({
                    // (command-line)
                    Scm::func(imports::command_minus_line).invoke(&[])
                })
            })
        };
        {
            // (define input-file-name (cadr args))
            input_minus_file_minus_name.with(|value| {
                value.set({
                    // (cadr args)
                    Scm::func(imports::cadr).invoke(&[args_.with(|value| value.get())])
                })
            })
        };
        {
            // (define output-module-name (caddr args))
            output_minus_module_minus_name.with(|value| {
                value.set({
                    // (caddr args)
                    Scm::func(imports::caddr).invoke(&[args_.with(|value| value.get())])
                })
            })
        };
        {
            // (define output-dir (if (pair? (cdddr args)) (cadddr args) "."))
            output_minus_dir.with(|value| {
                value.set(
                    if ({
                        // (pair? (cdddr args))
                        Scm::func(imports::pair_p).invoke(&[{
                            // (cdddr args)
                            Scm::func(imports::cdddr).invoke(&[args_.with(|value| value.get())])
                        }])
                    })
                    .is_true()
                    {
                        {
                            // (cadddr args)
                            Scm::func(imports::cadddr).invoke(&[args_.with(|value| value.get())])
                        }
                    } else {
                        Scm::from(".")
                    },
                )
            })
        };
        {
            // (newline)
            Scm::func(imports::newline).invoke(&[])
        };
        {
            // (display input-file-name)
            Scm::func(imports::display)
                .invoke(&[input_minus_file_minus_name.with(|value| value.get())])
        };
        {
            // (display " --> ")
            Scm::func(imports::display).invoke(&[Scm::from(" --> ")])
        };
        {
            // (display output-dir)
            Scm::func(imports::display).invoke(&[output_minus_dir.with(|value| value.get())])
        };
        {
            // (display "/")
            Scm::func(imports::display).invoke(&[Scm::from("/")])
        };
        {
            // (display output-module-name)
            Scm::func(imports::display)
                .invoke(&[output_minus_module_minus_name.with(|value| value.get())])
        };
        {
            // (newline)
            Scm::func(imports::newline).invoke(&[])
        };
        {
            // (newline)
            Scm::func(imports::newline).invoke(&[])
        };
        {
            // (define input-file (open-input-file input-file-name))
            input_minus_file.with(|value| {
                value.set({
                    // (open-input-file input-file-name)
                    Scm::func(imports::open_minus_input_minus_file)
                        .invoke(&[input_minus_file_minus_name.with(|value| value.get())])
                })
            })
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
                    Scm::func(load_minus_sexpr).invoke(&[])
                })
            })
        };
        {
            // (define ast (astify-toplevel program))
            ast.with(|value| {
                value.set({
                    // (astify-toplevel program)
                    Scm::func(imports::astify_minus_toplevel)
                        .invoke(&[program.with(|value| value.get())])
                })
            })
        };
        {
            // (rust-gen-in-module output-module-name output-dir (lambda (module) (ast (quote gen-rust) module)))
            Scm::func(imports::rust_minus_gen_minus_in_minus_module).invoke(&[
                output_minus_module_minus_name.with(|value| value.get()),
                output_minus_dir.with(|value| value.get()),
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let module = args[0].clone();
                        {
                            // (ast (quote gen-rust) module)
                            ast.with(|value| value.get())
                                .invoke(&[Scm::symbol("gen-rust"), module.clone()])
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
