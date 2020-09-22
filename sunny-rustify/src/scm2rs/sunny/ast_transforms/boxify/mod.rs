#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::boxify;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        // (define (boxify node) (define (transform node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))) (node (quote transform) transform))
        globals::boxify.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec ((transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))))) (node (quote transform) transform))
                    {
                        let transform = Scm::uninitialized().into_boxed();
                        transform.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node = args[0].clone();
                                let ignore = args[1].clone();
                                // (letrec () (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore))))
                                {
                                    // (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))
                                    if (
                                        // (eq? (node (quote kind)) (quote ABSTRACTION))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            // (node (quote kind))
                                            node.clone().invoke(&[Scm::symbol("kind")]),
                                            Scm::symbol("ABSTRACTION"),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))
                                        globals::boxify_minus_abstraction
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-body))
                                                node.clone().invoke(&[Scm::symbol("get-body")]),
                                            ])
                                    } else if (
                                        // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            // (node (quote kind))
                                            node.clone().invoke(&[Scm::symbol("kind")]),
                                            Scm::symbol("VARARG-ABSTRACTION"),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))
                                        globals::boxify_minus_vararg_minus_abstraction
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vararg))
                                                node.clone().invoke(&[Scm::symbol("get-vararg")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-varvar))
                                                node.clone().invoke(&[Scm::symbol("get-varvar")]),
                                                // (cons (node (quote get-vararg)) (node (quote get-params)))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    // (node (quote get-vararg))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-vararg")]),
                                                    // (node (quote get-params))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-params")]),
                                                ]),
                                                // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    // (node (quote get-varvar))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-varvar")]),
                                                    // (node (quote get-vars))
                                                    node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                ]),
                                                // (node (quote get-body))
                                                node.clone().invoke(&[Scm::symbol("get-body")]),
                                            ])
                                    } else {
                                        // (ignore)
                                        ignore.clone().invoke(&[])
                                    }
                                }
                            })
                        });

                        // (node (quote transform) transform)
                        node.clone()
                            .invoke(&[Scm::symbol("transform"), transform.get()])
                    }
                })
            })
        });
        // (define (boxify-abstraction params vars param* var* body) (if (null? var*) (make-abstraction params vars body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-abstraction params vars (cdr param*) (cdr var*) body))))
        globals::boxify_minus_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 5 {
                        panic!("invalid arity")
                    }
                    let params = args[0].clone();
                    let vars = args[1].clone();
                    let param_star_ = args[2].clone();
                    let var_star_ = args[3].clone();
                    let body = args[4].clone();
                    // (letrec () (if (null? var*) (make-abstraction params vars body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-abstraction params vars (cdr param*) (cdr var*) body))))
                    {
                        if (
                            // (null? var*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[var_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-abstraction params vars body)
                            imports::make_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[params.clone(), vars.clone(), body.clone()])
                        } else if (
                            // (variable-mut? (car var*))
                            imports::variable_minus_mut_p
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car var*)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                ])
                        )
                        .is_true()
                        {
                            {
                                // (variable-set-setter! (car var*) (quote BOXED-SET))
                                imports::variable_minus_set_minus_setter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-SET"),
                                    ]);
                                // (variable-set-getter! (car var*) (quote BOXED-REF))
                                imports::variable_minus_set_minus_getter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-REF"),
                                    ]);
                                // (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))
                                globals::boxify_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        params.clone(),
                                        vars.clone(),
                                        // (cdr param*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (cdr var*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        // (make-boxify (car param*) body)
                                        imports::make_minus_boxify
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car param*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[param_star_.clone()]),
                                                body.clone(),
                                            ]),
                                    ])
                            }
                        } else {
                            // (boxify-abstraction params vars (cdr param*) (cdr var*) body)
                            globals::boxify_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vars.clone(),
                                    // (cdr param*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[param_star_.clone()]),
                                    // (cdr var*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                    body.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (boxify-vararg-abstraction params vararg vars varvar param* var* body) (if (null? var*) (make-vararg-abstraction params vararg vars varvar body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body))))
        globals::boxify_minus_vararg_minus_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 7 {
                        panic!("invalid arity")
                    }
                    let params = args[0].clone();
                    let vararg = args[1].clone();
                    let vars = args[2].clone();
                    let varvar = args[3].clone();
                    let param_star_ = args[4].clone();
                    let var_star_ = args[5].clone();
                    let body = args[6].clone();
                    // (letrec () (if (null? var*) (make-vararg-abstraction params vararg vars varvar body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body))))
                    {
                        if (
                            // (null? var*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[var_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-vararg-abstraction params vararg vars varvar body)
                            imports::make_minus_vararg_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vararg.clone(),
                                    vars.clone(),
                                    varvar.clone(),
                                    body.clone(),
                                ])
                        } else if (
                            // (variable-mut? (car var*))
                            imports::variable_minus_mut_p
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car var*)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                ])
                        )
                        .is_true()
                        {
                            {
                                // (variable-set-setter! (car var*) (quote BOXED-SET))
                                imports::variable_minus_set_minus_setter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-SET"),
                                    ]);
                                // (variable-set-getter! (car var*) (quote BOXED-REF))
                                imports::variable_minus_set_minus_getter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-REF"),
                                    ]);
                                // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))
                                globals::boxify_minus_vararg_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        params.clone(),
                                        vararg.clone(),
                                        vars.clone(),
                                        varvar.clone(),
                                        // (cdr param*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (cdr var*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        // (make-boxify (car param*) body)
                                        imports::make_minus_boxify
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car param*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[param_star_.clone()]),
                                                body.clone(),
                                            ]),
                                    ])
                            }
                        } else {
                            // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body)
                            globals::boxify_minus_vararg_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vararg.clone(),
                                    vars.clone(),
                                    varvar.clone(),
                                    // (cdr param*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[param_star_.clone()]),
                                    // (cdr var*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                    body.clone(),
                                ])
                        }
                    }
                })
            })
        })
    };
}
