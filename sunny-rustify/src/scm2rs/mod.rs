#[allow(unused_imports)]
use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::process_context::exports::command_minus_line;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::table::exports::*;
    pub use crate::sunny::translate::exports::*;
    pub use crate::testsuite::exports::*;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static args_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static load_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL load-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_dir: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-dir"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_module_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-module-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL program"))}
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
    crate::sunny::translate::initialize();
    crate::sunny::table::initialize();
    crate::testsuite::initialize();
    {
        // (define args (command-line))
        globals::args_.with(|value| {
            value.set(
                // (command-line)
                imports::command_minus_line
                    .with(|value| value.get())
                    .invoke(&[]),
            )
        });
        // (define input-file-name (cadr args))
        globals::input_minus_file_minus_name.with(|value| {
            value.set(
                // (cadr args)
                imports::cadr
                    .with(|value| value.get())
                    .invoke(&[globals::args_.with(|value| value.get())]),
            )
        });
        // (define output-module-name (caddr args))
        globals::output_minus_module_minus_name.with(|value| {
            value.set(
                // (caddr args)
                imports::caddr
                    .with(|value| value.get())
                    .invoke(&[globals::args_.with(|value| value.get())]),
            )
        });
        // (define output-dir (if (pair? (cdddr args)) (cadddr args) "."))
        globals::output_minus_dir.with(|value| {
            value.set(
                if (
                    // (pair? (cdddr args))
                    imports::pair_p.with(|value| value.get()).invoke(&[
                        // (cdddr args)
                        imports::cdddr
                            .with(|value| value.get())
                            .invoke(&[globals::args_.with(|value| value.get())]),
                    ])
                )
                .is_true()
                {
                    // (cadddr args)
                    imports::cadddr
                        .with(|value| value.get())
                        .invoke(&[globals::args_.with(|value| value.get())])
                } else {
                    Scm::from(".")
                },
            )
        });
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (display input-file-name)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::input_minus_file_minus_name.with(|value| value.get())]);
        // (display " --> ")
        imports::display
            .with(|value| value.get())
            .invoke(&[Scm::from(" --> ")]);
        // (display output-dir)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::output_minus_dir.with(|value| value.get())]);
        // (display "/")
        imports::display
            .with(|value| value.get())
            .invoke(&[Scm::from("/")]);
        // (display output-module-name)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::output_minus_module_minus_name.with(|value| value.get())]);
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (define input-file (open-input-file input-file-name))
        globals::input_minus_file.with(|value| {
            value.set(
                // (open-input-file input-file-name)
                imports::open_minus_input_minus_file
                    .with(|value| value.get())
                    .invoke(&[globals::input_minus_file_minus_name.with(|value| value.get())]),
            )
        });
        // (define (load-sexpr) ...)
        globals::load_minus_sexpr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr))))
                    {
                        let [expr] = [
                            // (read input-file)
                            imports::read
                                .with(|value| value.get())
                                .invoke(&[globals::input_minus_file.with(|value| value.get())]),
                        ];
                        if (
                            // (eof-object? expr)
                            imports::eof_minus_object_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else {
                            // (cons expr (load-sexpr))
                            imports::cons.with(|value| value.get()).invoke(&[
                                expr.clone(),
                                // (load-sexpr)
                                globals::load_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[]),
                            ])
                        }
                    }
                })
            })
        });
        // (define program (load-sexpr))
        globals::program.with(|value| {
            value.set(
                // (load-sexpr)
                globals::load_minus_sexpr
                    .with(|value| value.get())
                    .invoke(&[]),
            )
        });
        // (define ast (scm->ast program))
        globals::ast.with(|value| {
            value.set(
                // (scm->ast program)
                imports::scm_minus__g_ast
                    .with(|value| value.get())
                    .invoke(&[globals::program.with(|value| value.get())]),
            )
        });
        // (rust-gen-in-module output-module-name output-dir (lambda (module) (ast (quote gen-rust) module)))
        imports::rust_minus_gen_minus_in_minus_module
            .with(|value| value.get())
            .invoke(&[
                globals::output_minus_module_minus_name.with(|value| value.get()),
                globals::output_minus_dir.with(|value| value.get()),
                {
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let module = args[0].clone();
                        // (ast (quote gen-rust) module)
                        globals::ast
                            .with(|value| value.get())
                            .invoke(&[Scm::symbol("gen-rust"), module.clone()])
                    })
                },
            ])
    };
}
pub mod chibi;
pub mod scheme;
pub mod sunny;
pub mod testsuite;
