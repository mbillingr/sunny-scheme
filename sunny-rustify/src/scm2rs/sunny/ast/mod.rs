#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::make_minus_alternative;
    pub use super::globals::make_minus_application;
    pub use super::globals::make_minus_args;
    pub use super::globals::make_minus_assignment;
    pub use super::globals::make_minus_comment;
    pub use super::globals::make_minus_constant;
    pub use super::globals::make_minus_nop;
    pub use super::globals::make_minus_null_minus_arg;
    pub use super::globals::make_minus_reference;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_null_minus_arg: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-null-arg"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-assignment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-reference"))}
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
    crate::sunny::rust::rustify::initialize();
    crate::sunny::variable::initialize();
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
        });
        // (define (make-reference name var) (define (global?) (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var)))) (define (repr) (list (variable-getter var) name)) (define (transform func) (func self (lambda () self))) (define (free-vars) (if (global?) (make-set) (set-add (make-set) name))) (define (gen-rust module) (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))) self)
        globals::make_minus_reference.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let var = args[1].clone();
                    // (letrec ((global? (lambda () (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var))))) (repr (lambda () (list (variable-getter var) name))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (global?) (make-set) (set-add (make-set) name)))) (gen-rust (lambda (module) (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))))) self)
                    {
                        let global_p = Scm::uninitialized().into_boxed();
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        global_p.set({
                            let var = var.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var))))
                                {
                                    if (
                                        // (eq? (quote GLOBAL-REF) (variable-getter var))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("GLOBAL-REF"),
                                            // (variable-getter var)
                                            imports::variable_minus_getter
                                                .with(|value| value.get())
                                                .invoke(&[var.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        Scm::True
                                    } else {
                                        // (eq? (quote IMPORT-REF) (variable-getter var))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("IMPORT-REF"),
                                            // (variable-getter var)
                                            imports::variable_minus_getter
                                                .with(|value| value.get())
                                                .invoke(&[var.clone()]),
                                        ])
                                    }
                                }
                            })
                        });
                        repr.set({
                            let var = var.clone();
                            let name = name.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (variable-getter var) name))
                                {
                                    // (list (variable-getter var) name)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (variable-getter var)
                                        imports::variable_minus_getter
                                            .with(|value| value.get())
                                            .invoke(&[var.clone()]),
                                        name.clone(),
                                    ])
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
                            let global_p = global_p.clone();
                            let name = name.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (if (global?) (make-set) (set-add (make-set) name)))
                                {
                                    if (
                                        // (global?)
                                        global_p.get().invoke(&[])
                                    )
                                    .is_true()
                                    {
                                        // (make-set)
                                        imports::make_minus_set
                                            .with(|value| value.get())
                                            .invoke(&[])
                                    } else {
                                        // (set-add (make-set) name)
                                        imports::set_minus_add.with(|value| value.get()).invoke(&[
                                            // (make-set)
                                            imports::make_minus_set
                                                .with(|value| value.get())
                                                .invoke(&[]),
                                            name.clone(),
                                        ])
                                    }
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let name = name.clone();
                            let var = var.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))
                                {
                                    // (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))
                                    {
                                        let [getter] = [
                                            // (variable-getter var)
                                            imports::variable_minus_getter
                                                .with(|value| value.get())
                                                .invoke(&[var.clone()]),
                                        ];
                                        // (cond ((eq? (quote GLOBAL-REF) getter) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))
                                        if (
                                            // (eq? (quote GLOBAL-REF) getter)
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("GLOBAL-REF"),
                                                getter.clone(),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            // (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                Scm::from("globals::"),
                                                // (rustify-identifier name)
                                                imports::rustify_minus_identifier
                                                    .with(|value| value.get())
                                                    .invoke(&[name.clone()]),
                                                Scm::from(".with(|value| value.get())"),
                                            ])
                                        } else if (
                                            // (eq? (quote IMPORT-REF) getter)
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("IMPORT-REF"),
                                                getter.clone(),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            // (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                Scm::from("imports::"),
                                                // (rustify-identifier name)
                                                imports::rustify_minus_identifier
                                                    .with(|value| value.get())
                                                    .invoke(&[name.clone()]),
                                                Scm::from(".with(|value| value.get())"),
                                            ])
                                        } else if (
                                            // (eq? (quote BOXED-REF) getter)
                                            imports::eq_p
                                                .with(|value| value.get())
                                                .invoke(&[Scm::symbol("BOXED-REF"), getter.clone()])
                                        )
                                        .is_true()
                                        {
                                            // (print module (rustify-identifier name) ".get()")
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                // (rustify-identifier name)
                                                imports::rustify_minus_identifier
                                                    .with(|value| value.get())
                                                    .invoke(&[name.clone()]),
                                                Scm::from(".get()"),
                                            ])
                                        } else {
                                            // (print module (rustify-identifier name) ".clone()")
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                // (rustify-identifier name)
                                                imports::rustify_minus_identifier
                                                    .with(|value| value.get())
                                                    .invoke(&[name.clone()]),
                                                Scm::from(".clone()"),
                                            ])
                                        }
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))
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
                                        Scm::symbol("REFERENCE")
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
                                        // (error "Unknown message REFERENCE" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message REFERENCE"),
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
        // (define (make-assignment name var val) (define (repr) (list (variable-setter var) name (val (quote repr)))) (define (transform func) (func self (lambda () (make-assignment name var (val (quote transform) func))))) (define (free-vars) (set-add (val (quote free-vars)) name)) (define (gen-rust module) (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((eq? (quote BOXED-SET) setter) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable"))))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))) self)
        globals::make_minus_assignment.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let var = args[1].clone();
                    let val = args[2].clone();
                    // (letrec ((repr (lambda () (list (variable-setter var) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (module) (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((eq? (quote BOXED-SET) setter) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let var = var.clone();
                            let name = name.clone();
                            let val = val.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (variable-setter var) name (val (quote repr))))
                                {
                                    // (list (variable-setter var) name (val (quote repr)))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (variable-setter var)
                                        imports::variable_minus_setter
                                            .with(|value| value.get())
                                            .invoke(&[var.clone()]),
                                        name.clone(),
                                        // (val (quote repr))
                                        val.clone().invoke(&[Scm::symbol("repr")]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let name = name.clone();
                            let var = var.clone();
                            let val = val.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-assignment name var (val (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-assignment name var (val (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let name = name.clone();
                                        let var = var.clone();
                                        let val = val.clone();
                                        let func = func.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-assignment name var (val (quote transform) func)))
                                            {
                                                // (make-assignment name var (val (quote transform) func))
                                                globals::make_minus_assignment
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        name.clone(),
                                                        var.clone(),
                                                        // (val (quote transform) func)
                                                        val.clone().invoke(&[
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
                            let val = val.clone();
                            let name = name.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-add (val (quote free-vars)) name))
                                {
                                    // (set-add (val (quote free-vars)) name)
                                    imports::set_minus_add.with(|value| value.get()).invoke(&[
                                        // (val (quote free-vars))
                                        val.clone().invoke(&[Scm::symbol("free-vars")]),
                                        name.clone(),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let name = name.clone();
                            let val = val.clone();
                            let var = var.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((eq? (quote BOXED-SET) setter) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable")))))
                                {
                                    // (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((eq? (quote BOXED-SET) setter) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable"))))
                                    {
                                        let [setter] = [
                                            // (variable-setter var)
                                            imports::variable_minus_setter
                                                .with(|value| value.get())
                                                .invoke(&[var.clone()]),
                                        ];
                                        // (cond ((eq? (quote GLOBAL-SET) setter) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((eq? (quote BOXED-SET) setter) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable")))
                                        if (
                                            // (eq? (quote GLOBAL-SET) setter)
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("GLOBAL-SET"),
                                                setter.clone(),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            {
                                                // (print module "globals::" (rustify-identifier name) ".with(|value| value.set(")
                                                imports::print.with(|value| value.get()).invoke(&[
                                                    module.clone(),
                                                    Scm::from("globals::"),
                                                    // (rustify-identifier name)
                                                    imports::rustify_minus_identifier
                                                        .with(|value| value.get())
                                                        .invoke(&[name.clone()]),
                                                    Scm::from(".with(|value| value.set("),
                                                ]);
                                                // (val (quote gen-rust) module)
                                                val.clone().invoke(&[
                                                    Scm::symbol("gen-rust"),
                                                    module.clone(),
                                                ]);
                                                // (print module "))")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("))")])
                                            }
                                        } else if (
                                            // (eq? (quote BOXED-SET) setter)
                                            imports::eq_p
                                                .with(|value| value.get())
                                                .invoke(&[Scm::symbol("BOXED-SET"), setter.clone()])
                                        )
                                        .is_true()
                                        {
                                            {
                                                // (print module (rustify-identifier name) ".set(")
                                                imports::print.with(|value| value.get()).invoke(&[
                                                    module.clone(),
                                                    // (rustify-identifier name)
                                                    imports::rustify_minus_identifier
                                                        .with(|value| value.get())
                                                        .invoke(&[name.clone()]),
                                                    Scm::from(".set("),
                                                ]);
                                                // (val (quote gen-rust) module)
                                                val.clone().invoke(&[
                                                    Scm::symbol("gen-rust"),
                                                    module.clone(),
                                                ]);
                                                // (print module ")")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from(")")])
                                            }
                                        } else {
                                            // (error "set! on unboxed variable")
                                            imports::error
                                                .with(|value| value.get())
                                                .invoke(&[Scm::from("set! on unboxed variable")])
                                        }
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))
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
                                        Scm::symbol("ASSIGNMENT")
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
                                        // (error "Unknown message ASSIGNMENT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message ASSIGNMENT"),
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
        // (define (make-alternative condition consequent alternative) (define (repr) (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))) (define (transform func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))) (define (free-vars) (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))) (define (gen-rust module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}")))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))) self)
        globals::make_minus_alternative.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let condition = args[0].clone();
                    let consequent = args[1].clone();
                    let alternative = args[2].clone();
                    // (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let condition = condition.clone();
                            let consequent = consequent.clone();
                            let alternative = alternative.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))
                                {
                                    // (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("IF"),
                                        // (condition (quote repr))
                                        condition.clone().invoke(&[Scm::symbol("repr")]),
                                        // (consequent (quote repr))
                                        consequent.clone().invoke(&[Scm::symbol("repr")]),
                                        // (alternative (quote repr))
                                        alternative.clone().invoke(&[Scm::symbol("repr")]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let condition = condition.clone();
                            let consequent = consequent.clone();
                            let alternative = alternative.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let condition = condition.clone();
                                        let func = func.clone();
                                        let consequent = consequent.clone();
                                        let alternative = alternative.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))
                                            {
                                                // (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
                                                globals::make_minus_alternative
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (condition (quote transform) func)
                                                        condition.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            func.clone(),
                                                        ]),
                                                        // (consequent (quote transform) func)
                                                        consequent.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            func.clone(),
                                                        ]),
                                                        // (alternative (quote transform) func)
                                                        alternative.clone().invoke(&[
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
                            let condition = condition.clone();
                            let consequent = consequent.clone();
                            let alternative = alternative.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))
                                {
                                    // (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
                                    imports::set_minus_union.with(|value| value.get()).invoke(&[
                                        // (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
                                        imports::set_minus_union.with(|value| value.get()).invoke(
                                            &[
                                                // (condition (quote free-vars))
                                                condition
                                                    .clone()
                                                    .invoke(&[Scm::symbol("free-vars")]),
                                                // (consequent (quote free-vars))
                                                consequent
                                                    .clone()
                                                    .invoke(&[Scm::symbol("free-vars")]),
                                            ],
                                        ),
                                        // (alternative (quote free-vars))
                                        alternative.clone().invoke(&[Scm::symbol("free-vars")]),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let condition = condition.clone();
                            let consequent = consequent.clone();
                            let alternative = alternative.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))
                                {
                                    {
                                        // (print module "if (")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("if (")]);
                                        // (condition (quote gen-rust) module)
                                        condition
                                            .clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module ").is_true() {")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from(").is_true() {")]);
                                        // (consequent (quote gen-rust) module)
                                        consequent
                                            .clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module "} else ")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("} else ")]);
                                        if (
                                            // (eq? (alternative (quote kind)) (quote ALTERNATIVE))
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                // (alternative (quote kind))
                                                alternative.clone().invoke(&[Scm::symbol("kind")]),
                                                Scm::symbol("ALTERNATIVE"),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            // (alternative (quote gen-rust) module)
                                            alternative
                                                .clone()
                                                .invoke(&[Scm::symbol("gen-rust"), module.clone()])
                                        } else {
                                            {
                                                // (print module "{")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("{")]);
                                                // (alternative (quote gen-rust) module)
                                                alternative.clone().invoke(&[
                                                    Scm::symbol("gen-rust"),
                                                    module.clone(),
                                                ]);
                                                // (print module "}")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("}")])
                                            }
                                        }
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))
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
                                        Scm::symbol("ALTERNATIVE")
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
                                        // (error "Unknown message ALTERNATIVE" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message ALTERNATIVE"),
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
        // (define (make-application func args tail?) (define (repr) (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))) (define (transform fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))) (define (free-vars) (set-union (func (quote free-vars)) (args (quote free-vars)))) (define (gen-rust module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))) self)
        globals::make_minus_application.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let func = args[0].clone();
                    let args_ = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let tail_p = tail_p.clone();
                            let func = func.clone();
                            let args_ = args_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))
                                {
                                    // (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        if (tail_p.clone()).is_true() {
                                            Scm::symbol("APPLY-TC")
                                        } else {
                                            Scm::symbol("APPLY")
                                        },
                                        // (cons (func (quote repr)) (args (quote repr)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (func (quote repr))
                                            func.clone().invoke(&[Scm::symbol("repr")]),
                                            // (args (quote repr))
                                            args_.clone().invoke(&[Scm::symbol("repr")]),
                                        ]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let func = func.clone();
                            let args_ = args_.clone();
                            let tail_p = tail_p.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let fnc = args[0].clone();
                                // (letrec () (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))
                                {
                                    // (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
                                    fnc.clone().invoke(&[self_.get(), {
                                        let func = func.clone();
                                        let fnc = fnc.clone();
                                        let args_ = args_.clone();
                                        let tail_p = tail_p.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))
                                            {
                                                // (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
                                                globals::make_minus_application
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (func (quote transform) fnc)
                                                        func.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            fnc.clone(),
                                                        ]),
                                                        // (args (quote transform) fnc)
                                                        args_.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            fnc.clone(),
                                                        ]),
                                                        tail_p.clone(),
                                                    ])
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        free_minus_vars.set({
                            let func = func.clone();
                            let args_ = args_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-union (func (quote free-vars)) (args (quote free-vars))))
                                {
                                    // (set-union (func (quote free-vars)) (args (quote free-vars)))
                                    imports::set_minus_union.with(|value| value.get()).invoke(&[
                                        // (func (quote free-vars))
                                        func.clone().invoke(&[Scm::symbol("free-vars")]),
                                        // (args (quote free-vars))
                                        args_.clone().invoke(&[Scm::symbol("free-vars")]),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let func = func.clone();
                            let args_ = args_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))
                                {
                                    {
                                        // (func (quote gen-rust) module)
                                        func.clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module ".invoke(&[")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from(".invoke(&[")]);
                                        // (args (quote gen-rust) module)
                                        args_
                                            .clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module "])")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("])")])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))
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
                                        Scm::symbol("APPLICATION")
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
                                        // (error "Unknown message APPLICATION" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message APPLICATION"),
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
        // (define (make-null-arg) (define (repr) (list (quote NULL-ARG))) (define (transform fnc) (fnc self (lambda () self))) (define (free-vars) (make-set)) (define (gen-rust module) (print module "")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))) self)
        globals::make_minus_null_minus_arg.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module ""))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
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
                                // (letrec () (list (quote NULL-ARG)))
                                {
                                    // (list (quote NULL-ARG))
                                    imports::list
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("NULL-ARG")])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let fnc = args[0].clone();
                                // (letrec () (fnc self (lambda () self)))
                                {
                                    // (fnc self (lambda () self))
                                    fnc.clone().invoke(&[self_.get(), {
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
                                // (letrec () (print module ""))
                                {
                                    // (print module "")
                                    imports::print
                                        .with(|value| value.get())
                                        .invoke(&[module.clone(), Scm::from("")])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))
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
                                        Scm::symbol("NULL-ARG")
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
                                        // (error "Unknown message NULL-ARG" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message NULL-ARG"),
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
        // (define (make-args arg next) (define (repr) (cons (quote ARG) (cons arg next))) (define (transform fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))) (define (free-vars) (set-union (arg (quote free-vars)) (next (quote free-vars)))) (define (gen-rust module) (arg (quote gen-rust) module) (print module ",") (next (quote gen-rust) module)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))) self)
        globals::make_minus_args.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let arg = args[0].clone();
                    let next = args[1].clone();
                    // (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (print module ",") (next (quote gen-rust) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let arg = arg.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote ARG) (cons arg next)))
                                {
                                    // (cons (quote ARG) (cons arg next))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("ARG"),
                                        // (cons arg next)
                                        imports::cons
                                            .with(|value| value.get())
                                            .invoke(&[arg.clone(), next.clone()]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let arg = arg.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let fnc = args[0].clone();
                                // (letrec () (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))
                                {
                                    // (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
                                    fnc.clone().invoke(&[self_.get(), {
                                        let arg = arg.clone();
                                        let fnc = fnc.clone();
                                        let next = next.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))
                                            {
                                                // (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
                                                globals::make_minus_args
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (arg (quote transform) fnc)
                                                        arg.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            fnc.clone(),
                                                        ]),
                                                        // (next (quote transform) fnc)
                                                        next.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            fnc.clone(),
                                                        ]),
                                                    ])
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        free_minus_vars.set({
                            let arg = arg.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-union (arg (quote free-vars)) (next (quote free-vars))))
                                {
                                    // (set-union (arg (quote free-vars)) (next (quote free-vars)))
                                    imports::set_minus_union.with(|value| value.get()).invoke(&[
                                        // (arg (quote free-vars))
                                        arg.clone().invoke(&[Scm::symbol("free-vars")]),
                                        // (next (quote free-vars))
                                        next.clone().invoke(&[Scm::symbol("free-vars")]),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let arg = arg.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (arg (quote gen-rust) module) (print module ",") (next (quote gen-rust) module))
                                {
                                    {
                                        // (arg (quote gen-rust) module)
                                        arg.clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module ",")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from(",")]);
                                        // (next (quote gen-rust) module)
                                        next.clone()
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))
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
                                        Scm::symbol("ARG")
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
                                        // (error "Unknown message ARG" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message ARG"),
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
