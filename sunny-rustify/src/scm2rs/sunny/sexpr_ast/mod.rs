#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::sexpr_minus__g_export;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::write::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::env::initialize();
    crate::sunny::library::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (sexpr->export export-spec* env) ...)
            globals::sexpr_minus__g_export.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let export_minus_spec_star_ = args[0].clone();
                        let env = args[1].clone();
                        {
                            // (cond ...)
                            if ({
                                // (null? export-spec*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[export_minus_spec_star_.clone()])
                            })
                            .is_true()
                            {
                                Scm::Nil
                            } else {
                                {
                                    // (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (make-export env (car export-spec*) (car export-spec*))
                                            imports::make_minus_export
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    env.clone(),
                                                    {
                                                        // (car export-spec*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                    {
                                                        // (car export-spec*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                ])
                                        },
                                        {
                                            // (sexpr->export (cdr export-spec*) env)
                                            globals::sexpr_minus__g_export
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    {
                                                        // (cdr export-spec*)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                export_minus_spec_star_.clone()
                                                            ])
                                                    },
                                                    env.clone(),
                                                ])
                                        },
                                    ])
                                }
                            }
                        }
                    })
                })
            })
        }
    };
}
