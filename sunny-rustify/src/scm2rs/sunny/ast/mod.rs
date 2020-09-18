#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::sets::exports::*;
}

pub mod exports {
    pub use super::globals::make_minus_comment;
    pub use super::globals::make_minus_constant;
    pub use super::globals::make_minus_nop;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_nop: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-nop"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_comment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-comment"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::rust::module::initialize();
    {
        (/*NOP*/);
        // (define (make-comment comment node) (define (repr) (cons (quote COMMENT) (cons comment (node (quote repr))))) (define (transform func) (func self (lambda () (make-comment comment (node (quote transform) func))))) (define (free-vars) (node (quote free-vars))) (define (gen-rust module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))) self)
        globals::make_minus_comment.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let comment = args[0].clone();
                    let node = args[1].clone();
                    // (letrec ((repr (lambda () (cons (quote COMMENT) (cons comment (node (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let comment = comment.clone();
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote COMMENT) (cons comment (node (quote repr)))))
                                {
                                    // (cons (quote COMMENT) (cons comment (node (quote repr))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("COMMENT"),
                                        // (cons comment (node (quote repr)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            comment.clone(),
                                            // (node (quote repr))
                                            node.clone().invoke(&[Scm::symbol("repr")]),
                                        ]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let comment = comment.clone();
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-comment comment (node (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-comment comment (node (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let comment = comment.clone();
                                        let node = node.clone();
                                        let func = func.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-comment comment (node (quote transform) func)))
                                            {
                                                // (make-comment comment (node (quote transform) func))
                                                globals::make_minus_comment
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        comment.clone(),
                                                        // (node (quote transform) func)
                                                        node.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            func.clone(),
                                                        ]),
                                                    ])
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        free_minus_vars.set({
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (node (quote free-vars)))
                                {
                                    // (node (quote free-vars))
                                    node.clone().invoke(&[Scm::symbol("free-vars")])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let comment = comment.clone();
                            let node = node.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))
                                {
                                    {
                                        // (println module)
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone()]);
                                        // (print module "// ")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("// ")]);
                                        // (showln module comment)
                                        imports::showln
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), comment.clone()]);
                                        // (node (quote gen-rust) module)
                                        node.clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()])
                                    }
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let free_minus_vars = free_minus_vars.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))
                                    if (
                                        // (eq? (quote repr) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("repr"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print)
                                        imports::print.with(|value| value.get()).invoke(&[])
                                    } else if (
                                        // (eq? (quote transform) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("transform"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (transform (car args))
                                        transform.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else if (
                                        // (eq? (quote free-vars) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("free-vars"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (free-vars)
                                        free_minus_vars.get().invoke(&[])
                                    } else if (
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("COMMENT")
                                    } else if (
                                        // (eq? (quote gen-rust) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("gen-rust"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (gen-rust (car args))
                                        gen_minus_rust.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else {
                                        // (error "Unknown message COMMENT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message COMMENT"),
                                            msg.clone(),
                                        ])
                                    }
                                }
                            })
                        });
                        self_.get()
                    }
                })
            })
        });
        // (define (make-nop) (define (repr) (quote (NOP))) (define (transform func) (func self (lambda () self))) (define (free-vars) (make-set)) (define (gen-rust module) (print module "(/*NOP*/)")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))) self)
        globals::make_minus_nop.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec ((repr (lambda () (quote (NOP)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module "(/*NOP*/)"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (quote (NOP)))
                                {
                                    Scm::pair(Scm::symbol("NOP"), Scm::Nil)
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () self)))
                                {
                                    // (func self (lambda () self))
                                    func.clone().invoke(&[self_.get(), {
                                        let self_ = self_.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () self)
                                            {
                                                self_.get()
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        free_minus_vars.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (make-set))
                                {
                                    // (make-set)
                                    imports::make_minus_set
                                        .with(|value| value.get())
                                        .invoke(&[])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "(/*NOP*/)"))
                                {
                                    // (print module "(/*NOP*/)")
                                    imports::print
                                        .with(|value| value.get())
                                        .invoke(&[module.clone(), Scm::from("(/*NOP*/)")])
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let free_minus_vars = free_minus_vars.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))
                                    if (
                                        // (eq? (quote repr) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("repr"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print)
                                        imports::print.with(|value| value.get()).invoke(&[])
                                    } else if (
                                        // (eq? (quote transform) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("transform"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (transform (car args))
                                        transform.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else if (
                                        // (eq? (quote free-vars) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("free-vars"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (free-vars)
                                        free_minus_vars.get().invoke(&[])
                                    } else if (
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("NOP")
                                    } else if (
                                        // (eq? (quote gen-rust) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("gen-rust"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (gen-rust (car args))
                                        gen_minus_rust.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else {
                                        // (error "Unknown message NOP" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message NOP"),
                                            msg.clone(),
                                        ])
                                    }
                                }
                            })
                        });
                        self_.get()
                    }
                })
            })
        });
        // (define (make-constant val) (define (repr) (cons (quote CONSTANT) val)) (define (transform func) (func self (lambda () self))) (define (free-vars) (make-set)) (define (gen-constant module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char('\\'')")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")")))) (define (gen-rust module) (gen-constant module val)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))) self)
        globals::make_minus_constant.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let val = args[0].clone();
                    // (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char('\\'')")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_constant = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let val = val.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote CONSTANT) val))
                                {
                                    // (cons (quote CONSTANT) val)
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("CONSTANT"), val.clone()])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () self)))
                                {
                                    // (func self (lambda () self))
                                    func.clone().invoke(&[self_.get(), {
                                        let self_ = self_.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () self)
                                            {
                                                self_.get()
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        free_minus_vars.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (make-set))
                                {
                                    // (make-set)
                                    imports::make_minus_set
                                        .with(|value| value.get())
                                        .invoke(&[])
                                }
                            })
                        });
                        gen_minus_constant.set({
                            let gen_minus_constant = gen_minus_constant.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                let val = args[1].clone();
                                // (letrec () (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char('\\'')")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))
                                {
                                    // (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char('\\'')")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")")))
                                    if (
                                        // (null? val)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::Nil")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("Scm::Nil")])
                                    } else if (
                                        // (eq? val #t)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone(), Scm::True])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::True")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("Scm::True")])
                                    } else if (
                                        // (eq? val #f)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone(), Scm::False])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::False")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("Scm::False")])
                                    } else if (
                                        // (symbol? val)
                                        imports::symbol_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::symbol(\"" val "\")")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("Scm::symbol(\""),
                                            val.clone(),
                                            Scm::from("\")"),
                                        ])
                                    } else if (
                                        // (eq? val #\')
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone(), Scm::char('\'')])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::char('\\'')")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("Scm::char('\\'')"),
                                        ])
                                    } else if (
                                        // (char? val)
                                        imports::char_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print module "Scm::char('" val "')")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("Scm::char('"),
                                            val.clone(),
                                            Scm::from("')"),
                                        ])
                                    } else if (
                                        // (pair? val)
                                        imports::pair_p
                                            .with(|value| value.get())
                                            .invoke(&[val.clone()])
                                    )
                                    .is_true()
                                    {
                                        {
                                            // (print module "Scm::pair(")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from("Scm::pair(")]);
                                            // (gen-constant module (car val))
                                            gen_minus_constant.get().invoke(&[
                                                module.clone(),
                                                // (car val)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[val.clone()]),
                                            ]);
                                            // (print module ", ")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from(", ")]);
                                            // (gen-constant module (cdr val))
                                            gen_minus_constant.get().invoke(&[
                                                module.clone(),
                                                // (cdr val)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[val.clone()]),
                                            ]);
                                            // (print module ")")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from(")")])
                                        }
                                    } else {
                                        {
                                            // (print module "Scm::from(")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from("Scm::from(")]);
                                            // (show module val)
                                            imports::show
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), val.clone()]);
                                            // (print module ")")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from(")")])
                                        }
                                    }
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let gen_minus_constant = gen_minus_constant.clone();
                            let val = val.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (gen-constant module val))
                                {
                                    // (gen-constant module val)
                                    gen_minus_constant
                                        .get()
                                        .invoke(&[module.clone(), val.clone()])
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let free_minus_vars = free_minus_vars.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))
                                    if (
                                        // (eq? (quote repr) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("repr"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print)
                                        imports::print.with(|value| value.get()).invoke(&[])
                                    } else if (
                                        // (eq? (quote transform) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("transform"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (transform (car args))
                                        transform.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else if (
                                        // (eq? (quote free-vars) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("free-vars"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (free-vars)
                                        free_minus_vars.get().invoke(&[])
                                    } else if (
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("CONSTANT")
                                    } else if (
                                        // (eq? (quote gen-rust) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("gen-rust"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (gen-rust (car args))
                                        gen_minus_rust.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else {
                                        // (error "Unknown message CONSTANT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message CONSTANT"),
                                            msg.clone(),
                                        ])
                                    }
                                }
                            })
                        });
                        self_.get()
                    }
                })
            })
        })
    };
}
