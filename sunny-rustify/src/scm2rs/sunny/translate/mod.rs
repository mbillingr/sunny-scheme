#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::chibi::filesystem::exports::*;
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::{
        file_minus_exists_p, open_minus_input_minus_file, open_minus_output_minus_file,
    };
    pub use crate::scheme::read::exports::read;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::ast_transforms::boxify::exports::*;
    pub use crate::sunny::ast_transforms::close_procedures::exports::*;
    pub use crate::sunny::astify::exports::*;
    pub use crate::sunny::astify_toplevel::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::module_tree::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::sexpr_ast::exports::*;
    pub use crate::sunny::syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::scm_minus__g_ast;
    pub use super::imports::rust_minus_gen_minus_in_minus_module;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static assoc: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assoc"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scm_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scm->ast"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::read::initialize();
    crate::scheme::file::initialize();
    crate::chibi::filesystem::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::ast_transforms::boxify::initialize();
    crate::sunny::ast_transforms::close_procedures::initialize();
    crate::sunny::astify::initialize();
    crate::sunny::astify_toplevel::initialize();
    crate::sunny::env::initialize();
    crate::sunny::library::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::rust::module::initialize();
    crate::sunny::rust::module_tree::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::sexpr_ast::initialize();
    crate::sunny::syntax::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (scm->ast exp*) ...)
            globals::scm_minus__g_ast.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let exp_star_ = args[0].clone();
                        if ({
                            // (library? (car exp*))
                            imports::library_p.with(|value| value.get()).invoke(&[{
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (astify-library (library-name (car exp*)) (library-decls (car exp*)) (list (quote ())))
                                imports::astify_minus_library
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (library-name (car exp*))
                                            imports::library_minus_name
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (car exp*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                        },
                                        {
                                            // (library-decls (car exp*))
                                            imports::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (car exp*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                        },
                                        {
                                            // (list (quote ()))
                                            imports::list
                                                .with(|value| value.get())
                                                .invoke(&[Scm::Nil])
                                        },
                                    ])
                            }
                        } else {
                            {
                                // (astify-program exp*)
                                imports::astify_minus_program
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (assoc obj seq) ...)
            globals::assoc.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let obj = args[0].clone();
                        let seq = args[1].clone();
                        if ({
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        })
                        .is_true()
                        {
                            if ({
                                // (equal? obj (caar seq))
                                imports::equal_p
                                    .with(|value| value.get())
                                    .invoke(&[obj.clone(), {
                                        // (caar seq)
                                        imports::caar
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()])
                                    }])
                            })
                            .is_true()
                            {
                                {
                                    // (car seq)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                }
                            } else {
                                {
                                    // (assoc obj (cdr seq))
                                    globals::assoc.with(|value| value.get()).invoke(&[
                                        obj.clone(),
                                        {
                                            // (cdr seq)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()])
                                        },
                                    ])
                                }
                            }
                        } else {
                            Scm::False
                        }
                    })
                })
            })
        }
    };
}
