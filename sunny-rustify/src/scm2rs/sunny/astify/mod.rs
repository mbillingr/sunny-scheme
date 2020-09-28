#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::sexpr_ast::exports::*;
}

pub mod exports {
    pub use super::globals::astify_minus_alternative;
    pub use super::globals::astify_minus_constant;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL astify"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::sexpr_ast::initialize();
    {
        (/*NOP*/);
        // (define astify sexpr->ast)
        globals::astify
            .with(|value| value.set(imports::sexpr_minus__g_ast.with(|value| value.get())));
        // (define (astify-alternative condition consequent alternative env tail?) (make-alternative (astify condition env #f) (astify consequent env tail?) (astify alternative env tail?)))
        globals::astify_minus_alternative.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 5 {
                        panic!("invalid arity")
                    }
                    let condition = args[0].clone();
                    let consequent = args[1].clone();
                    let alternative = args[2].clone();
                    let env = args[3].clone();
                    let tail_p = args[4].clone();
                    // (letrec () (make-alternative (astify condition env #f) (astify consequent env tail?) (astify alternative env tail?)))
                    {
                        // (make-alternative (astify condition env #f) (astify consequent env tail?) (astify alternative env tail?))
                        imports::make_minus_alternative
                            .with(|value| value.get())
                            .invoke(&[
                                // (astify condition env #f)
                                globals::astify.with(|value| value.get()).invoke(&[
                                    condition.clone(),
                                    env.clone(),
                                    Scm::False,
                                ]),
                                // (astify consequent env tail?)
                                globals::astify.with(|value| value.get()).invoke(&[
                                    consequent.clone(),
                                    env.clone(),
                                    tail_p.clone(),
                                ]),
                                // (astify alternative env tail?)
                                globals::astify.with(|value| value.get()).invoke(&[
                                    alternative.clone(),
                                    env.clone(),
                                    tail_p.clone(),
                                ]),
                            ])
                    }
                })
            })
        });
        // (define (astify-constant exp env) (make-constant exp))
        globals::astify_minus_constant.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (make-constant exp))
                    {
                        // (make-constant exp)
                        imports::make_minus_constant
                            .with(|value| value.get())
                            .invoke(&[exp.clone()])
                    }
                })
            })
        })
    };
}
