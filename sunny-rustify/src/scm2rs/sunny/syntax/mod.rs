#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::astify::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::make_minus_core_minus_env;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static expand_minus_and: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL expand-and"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static expand_minus_if: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL expand-if"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static expand_minus_quote: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL expand-quote"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_core_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-core-env"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::write::initialize();
    crate::sunny::astify::initialize();
    crate::sunny::env::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        // (define (make-core-env) (list (quote GLOBAL-MARKER) (new-keyword (quote and) expand-and) (new-keyword (quote if) expand-if) (new-keyword (quote quote) expand-quote) (new-import (quote assert-eq)) (new-import (quote assert-equal))))
        globals::make_minus_core_minus_env.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (list (quote GLOBAL-MARKER) (new-keyword (quote and) expand-and) (new-keyword (quote if) expand-if) (new-keyword (quote quote) expand-quote) (new-import (quote assert-eq)) (new-import (quote assert-equal))))
                    {
                        // (list (quote GLOBAL-MARKER) (new-keyword (quote and) expand-and) (new-keyword (quote if) expand-if) (new-keyword (quote quote) expand-quote) (new-import (quote assert-eq)) (new-import (quote assert-equal)))
                        imports::list.with(|value| value.get()).invoke(&[
                            Scm::symbol("GLOBAL-MARKER"),
                            // (new-keyword (quote and) expand-and)
                            imports::new_minus_keyword
                                .with(|value| value.get())
                                .invoke(&[
                                    Scm::symbol("and"),
                                    globals::expand_minus_and.with(|value| value.get()),
                                ]),
                            // (new-keyword (quote if) expand-if)
                            imports::new_minus_keyword
                                .with(|value| value.get())
                                .invoke(&[
                                    Scm::symbol("if"),
                                    globals::expand_minus_if.with(|value| value.get()),
                                ]),
                            // (new-keyword (quote quote) expand-quote)
                            imports::new_minus_keyword
                                .with(|value| value.get())
                                .invoke(&[
                                    Scm::symbol("quote"),
                                    globals::expand_minus_quote.with(|value| value.get()),
                                ]),
                            // (new-import (quote assert-eq))
                            imports::new_minus_import
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("assert-eq")]),
                            // (new-import (quote assert-equal))
                            imports::new_minus_import
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("assert-equal")]),
                        ])
                    }
                })
            })
        });
        // (define (expand-and exp env tail?) (astify-comment exp (astify-and (and-args exp) env tail?)))
        globals::expand_minus_and.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (astify-comment exp (astify-and (and-args exp) env tail?)))
                    {
                        // (astify-comment exp (astify-and (and-args exp) env tail?))
                        imports::astify_minus_comment
                            .with(|value| value.get())
                            .invoke(&[
                                exp.clone(),
                                // (astify-and (and-args exp) env tail?)
                                imports::astify_minus_and
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (and-args exp)
                                        imports::and_minus_args
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ]),
                            ])
                    }
                })
            })
        });
        // (define (expand-if exp env tail?) (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?))
        globals::expand_minus_if.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?))
                    {
                        // (astify-alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
                        imports::astify_minus_alternative
                            .with(|value| value.get())
                            .invoke(&[
                                // (if-condition exp)
                                imports::if_minus_condition
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()]),
                                // (if-consequence exp)
                                imports::if_minus_consequence
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()]),
                                // (if-alternative exp)
                                imports::if_minus_alternative
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()]),
                                env.clone(),
                                tail_p.clone(),
                            ])
                    }
                })
            })
        });
        // (define (expand-quote exp env tail?) (astify-constant (cadr exp) env))
        globals::expand_minus_quote.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (astify-constant (cadr exp) env))
                    {
                        // (astify-constant (cadr exp) env)
                        imports::astify_minus_constant
                            .with(|value| value.get())
                            .invoke(&[
                                // (cadr exp)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()]),
                                env.clone(),
                            ])
                    }
                })
            })
        })
    };
}
