#[allow(unused_imports)]
use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::process_context::exports::command_minus_line;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::translate::exports::*;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static load_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL load-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-file"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-file-name"))}
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
    {
        // (define input-file-name (cadr (command-line)))
        globals::input_minus_file_minus_name.with(|value| {
            value.set(
                // (cadr (command-line))
                imports::cadr.with(|value| value.get()).invoke(&[
                    // (command-line)
                    imports::command_minus_line
                        .with(|value| value.get())
                        .invoke(&[]),
                ]),
            )
        });
        // (define output-file-name (caddr (command-line)))
        globals::output_minus_file_minus_name.with(|value| {
            value.set(
                // (caddr (command-line))
                imports::caddr.with(|value| value.get()).invoke(&[
                    // (command-line)
                    imports::command_minus_line
                        .with(|value| value.get())
                        .invoke(&[]),
                ]),
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
        // (display output-file-name)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::output_minus_file_minus_name.with(|value| value.get())]);
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
        // (define (load-sexpr) (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr)))))
        globals::load_minus_sexpr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr)))))
                    {
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
        // (define output-file (open-output-file output-file-name))
        globals::output_minus_file.with(|value| {
            value.set(
                // (open-output-file output-file-name)
                imports::open_minus_output_minus_file
                    .with(|value| value.get())
                    .invoke(&[globals::output_minus_file_minus_name.with(|value| value.get())]),
            )
        });
        // (ast (quote gen-rust) output-file)
        globals::ast.with(|value| value.get()).invoke(&[
            Scm::symbol("gen-rust"),
            globals::output_minus_file.with(|value| value.get()),
        ]);
        // (close-port output-file)
        imports::close_minus_port
            .with(|value| value.get())
            .invoke(&[globals::output_minus_file.with(|value| value.get())])
    };
}
pub mod scheme {
    pub mod base {}
    pub mod cxr {}
    pub mod file {}
    pub mod read {}
    pub mod write {}
    pub mod process_context {}
}
pub mod chibi {
    pub mod filesystem {}
}
pub mod sunny {
    pub mod utils {}
    pub mod translate {}
}
