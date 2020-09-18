#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::make_minus_abstraction;
    pub use super::globals::make_minus_alternative;
    pub use super::globals::make_minus_application;
    pub use super::globals::make_minus_args;
    pub use super::globals::make_minus_assert;
    pub use super::globals::make_minus_assignment;
    pub use super::globals::make_minus_boxify;
    pub use super::globals::make_minus_comment;
    pub use super::globals::make_minus_constant;
    pub use super::globals::make_minus_export;
    pub use super::globals::make_minus_fixlet;
    pub use super::globals::make_minus_import;
    pub use super::globals::make_minus_import_minus_only;
    pub use super::globals::make_minus_library;
    pub use super::globals::make_minus_nop;
    pub use super::globals::make_minus_null_minus_arg;
    pub use super::globals::make_minus_program;
    pub use super::globals::make_minus_reference;
    pub use super::globals::make_minus_scope;
    pub use super::globals::make_minus_sequence;
    pub use super::globals::make_minus_testcase;
    pub use super::globals::make_minus_testsuite;
    pub use super::globals::make_minus_vararg_minus_abstraction;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-boxify"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-program"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_scope: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-scope"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_null_minus_arg: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-null-arg"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-sequence"))}
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
    crate::sunny::env::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::rust::module::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::utils::initialize();
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
        // (define (make-sequence first next) (define (repr) (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))) (define (transform func) (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func))))) (define (free-vars) (set-union (first (quote free-vars)) (next (quote free-vars)))) (define (gen-rust-inner module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module))) (define (gen-rust module) (print module "{") (gen-rust-inner module) (print module "}")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))) self)
        globals::make_minus_sequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let first = args[0].clone();
                    let next = args[1].clone();
                    // (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust_minus_inner = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let first = first.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))
                                {
                                    // (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("SEQUENCE"),
                                        // (first (quote repr))
                                        first.clone().invoke(&[Scm::symbol("repr")]),
                                        // (next (quote repr))
                                        next.clone().invoke(&[Scm::symbol("repr")]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let first = first.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let first = first.clone();
                                        let func = func.clone();
                                        let next = next.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-sequence (first (quote transform) func) (next (quote transform) func)))
                                            {
                                                // (make-sequence (first (quote transform) func) (next (quote transform) func))
                                                globals::make_minus_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (first (quote transform) func)
                                                        first.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            func.clone(),
                                                        ]),
                                                        // (next (quote transform) func)
                                                        next.clone().invoke(&[
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
                            let first = first.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-union (first (quote free-vars)) (next (quote free-vars))))
                                {
                                    // (set-union (first (quote free-vars)) (next (quote free-vars)))
                                    imports::set_minus_union.with(|value| value.get()).invoke(&[
                                        // (first (quote free-vars))
                                        first.clone().invoke(&[Scm::symbol("free-vars")]),
                                        // (next (quote free-vars))
                                        next.clone().invoke(&[Scm::symbol("free-vars")]),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust_minus_inner.set({
                            let first = first.clone();
                            let next = next.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))
                                {
                                    {
                                        // (first (quote gen-rust) module)
                                        first
                                            .clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (print module ";")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from(";")]);
                                        if (
                                            // (eq? (quote SEQUENCE) (next (quote kind)))
                                            imports::eq_p.with(|value| value.get()).invoke(&[
                                                Scm::symbol("SEQUENCE"),
                                                // (next (quote kind))
                                                next.clone().invoke(&[Scm::symbol("kind")]),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            // (next (quote gen-rust-inner) module)
                                            next.clone().invoke(&[
                                                Scm::symbol("gen-rust-inner"),
                                                module.clone(),
                                            ])
                                        } else {
                                            // (next (quote gen-rust) module)
                                            next.clone()
                                                .invoke(&[Scm::symbol("gen-rust"), module.clone()])
                                        }
                                    }
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "{") (gen-rust-inner module) (print module "}"))
                                {
                                    {
                                        // (print module "{")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("{")]);
                                        // (gen-rust-inner module)
                                        gen_minus_rust_minus_inner.get().invoke(&[module.clone()]);
                                        // (print module "}")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("}")])
                                    }
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let free_minus_vars = free_minus_vars.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))
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
                                        Scm::symbol("SEQUENCE")
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
                                    } else if (
                                        // (eq? (quote gen-rust-inner) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("gen-rust-inner"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (gen-rust-inner (car args))
                                        gen_minus_rust_minus_inner.get().invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                        ])
                                    } else {
                                        // (error "Unknown message SEQUENCE" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message SEQUENCE"),
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
        });
        // (define (make-fixlet params body args) (define (repr) (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr)))))) (define (transform fnc) (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))))) (define (free-vars) (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))) (define (gen-rust module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (rust-block module (lambda () (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];") (body (quote gen-rust) module)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))) self)
        globals::make_minus_fixlet.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let params = args[0].clone();
                    let body = args[1].clone();
                    let args_ = args[2].clone();
                    // (letrec ((repr (lambda () (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr))))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (gen-rust (lambda (module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (rust-block module (lambda () (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let params = params.clone();
                            let args_ = args_.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr))))))
                                {
                                    // (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr)))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("FIXLET"),
                                        // (cons params (cons (args (quote repr)) (body (quote repr))))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            params.clone(),
                                            // (cons (args (quote repr)) (body (quote repr)))
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                // (args (quote repr))
                                                args_.clone().invoke(&[Scm::symbol("repr")]),
                                                // (body (quote repr))
                                                body.clone().invoke(&[Scm::symbol("repr")]),
                                            ]),
                                        ]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let params = params.clone();
                            let body = body.clone();
                            let args_ = args_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let fnc = args[0].clone();
                                // (letrec () (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))))
                                {
                                    // (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))))
                                    fnc.clone().invoke(&[self_.get(), {
                                        let params = params.clone();
                                        let body = body.clone();
                                        let fnc = fnc.clone();
                                        let args_ = args_.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))
                                            {
                                                // (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))
                                                globals::make_minus_fixlet
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        params.clone(),
                                                        // (body (quote transform) fnc)
                                                        body.clone().invoke(&[
                                                            Scm::symbol("transform"),
                                                            fnc.clone(),
                                                        ]),
                                                        // (args (quote transform) fnc)
                                                        args_.clone().invoke(&[
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
                            let body = body.clone();
                            let params = params.clone();
                            let args_ = args_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))
                                {
                                    // (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))
                                    imports::set_minus_union.with(|value| value.get()).invoke(&[
                                        // (set-remove* (body (quote free-vars)) params)
                                        imports::set_minus_remove_star_
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (body (quote free-vars))
                                                body.clone().invoke(&[Scm::symbol("free-vars")]),
                                                params.clone(),
                                            ]),
                                        // (args (quote free-vars))
                                        args_.clone().invoke(&[Scm::symbol("free-vars")]),
                                    ])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let params = params.clone();
                            let args_ = args_.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec ((gen-params (lambda (p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))))) (rust-block module (lambda () (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];") (body (quote gen-rust) module))))
                                {
                                    let gen_minus_params = Scm::uninitialized().into_boxed();
                                    gen_minus_params.set({
                                        let module = module.clone();
                                        let gen_minus_params = gen_minus_params.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let p_star_ = args[0].clone();
                                            // (letrec () (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*)))))
                                            {
                                                if (
                                                    // (pair? p*)
                                                    imports::pair_p
                                                        .with(|value| value.get())
                                                        .invoke(&[p_star_.clone()])
                                                )
                                                .is_true()
                                                {
                                                    {
                                                        // (print module (rustify-identifier (car p*)) ", ")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                // (rustify-identifier (car p*))
                                                                imports::rustify_minus_identifier
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (car p*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                p_star_.clone()
                                                                            ]),
                                                                    ]),
                                                                Scm::from(", "),
                                                            ]);
                                                        // (gen-params (cdr p*))
                                                        gen_minus_params.get().invoke(&[
                                                            // (cdr p*)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[p_star_.clone()]),
                                                        ])
                                                    }
                                                } else {
                                                    Scm::symbol("*UNSPECIFIED*")
                                                }
                                            }
                                        })
                                    });

                                    // (rust-block module (lambda () (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];") (body (quote gen-rust) module)))
                                    imports::rust_minus_block
                                        .with(|value| value.get())
                                        .invoke(&[module.clone(), {
                                            let module = module.clone();
                                            let gen_minus_params = gen_minus_params.clone();
                                            let params = params.clone();
                                            let args_ = args_.clone();
                                            let body = body.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 0 {
                                                    panic!("invalid arity")
                                                }
                                                // (letrec () (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];") (body (quote gen-rust) module))
                                                {
                                                    {
                                                        // (print module "let [")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from("let ["),
                                                            ]);
                                                        // (gen-params params)
                                                        gen_minus_params
                                                            .get()
                                                            .invoke(&[params.clone()]);
                                                        // (print module "] = [")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from("] = ["),
                                                            ]);
                                                        // (args (quote gen-rust) module)
                                                        args_.clone().invoke(&[
                                                            Scm::symbol("gen-rust"),
                                                            module.clone(),
                                                        ]);
                                                        // (print module "];")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from("];"),
                                                            ]);
                                                        // (body (quote gen-rust) module)
                                                        body.clone().invoke(&[
                                                            Scm::symbol("gen-rust"),
                                                            module.clone(),
                                                        ])
                                                    }
                                                }
                                            })
                                        }])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))
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
                                        Scm::symbol("FIXLET")
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
                                        // (error "Unknown message FIXLET" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message FIXLET"),
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
        // (define (make-scope params body args) (define (repr) (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr)))))) (define (transform fnc) (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))))) (define (free-vars-args args) (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args))))) (define (free-vars) (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params)) (define (gen-rust module) (rust-block module (lambda () (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args) (body (quote gen-rust) module)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))) self)
        globals::make_minus_scope.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let body = args[1].clone();let args_ = args[2].clone();
// (letrec ((repr (lambda () (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr))))))) (transform (lambda (fnc) (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))))) (free-vars-args (lambda (args) (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))))) (free-vars (lambda () (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params))) (gen-rust (lambda (module) (rust-block module (lambda () (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars_minus_args = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr))))))
{
// (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr)))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("SCOPE"),
// (cons params (cons (args (quote repr)) (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[params.clone(),
// (cons (args (quote repr)) (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr"),]),
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"),]),]),]),])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let body = body.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();
// (letrec () (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))))
{
// (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))))
fnc.clone().invoke(&[self_.get(),{let params = params.clone();let body = body.clone();let fnc = fnc.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))
{
// (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))
globals::make_minus_scope.with(|value| value.get()).invoke(&[params.clone(),
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"),fnc.clone(),]),
// (map (lambda (a) (a (quote transform) fnc)) args)
imports::map.with(|value| value.get()).invoke(&[{let fnc = fnc.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let a = args[0].clone();
// (letrec () (a (quote transform) fnc))
{
// (a (quote transform) fnc)
a.clone().invoke(&[Scm::symbol("transform"),fnc.clone(),])}})},args_.clone(),]),])}})},])}})});
free_minus_vars_minus_args.set({let free_minus_vars_minus_args = free_minus_vars_minus_args.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let args_ = args[0].clone();
// (letrec () (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))))
{if (
// (null? args)
imports::null_p.with(|value| value.get()).invoke(&[args_.clone(),])).is_true() {
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])} else {
// (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))
imports::set_minus_union.with(|value| value.get()).invoke(&[
// ((car args) (quote free-vars))

// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]).invoke(&[Scm::symbol("free-vars"),]),
// (free-vars-args (cdr args))
free_minus_vars_minus_args.get().invoke(&[
// (cdr args)
imports::cdr.with(|value| value.get()).invoke(&[args_.clone(),]),]),])}}})});
free_minus_vars.set({let body = body.clone();let free_minus_vars_minus_args = free_minus_vars_minus_args.clone();let args_ = args_.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params))
{
// (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params)
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (set-union (body (quote free-vars)) (free-vars-args args))
imports::set_minus_union.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"),]),
// (free-vars-args args)
free_minus_vars_minus_args.get().invoke(&[args_.clone(),]),]),params.clone(),])}})});
gen_minus_rust.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec () (rust-block module (lambda () (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args) (body (quote gen-rust) module))))
{
// (rust-block module (lambda () (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args) (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args) (body (quote gen-rust) module))
{{
// (for-each (lambda (p) (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p = args[0].clone();
// (letrec () (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();"))
{
// (println module "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),
// (rustify-identifier p)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[p.clone(),]),Scm::from(" = Scm::uninitialized().into_boxed();"),])}})},params.clone(),]);
// (for-each (lambda (p a) (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");")) params args)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p = args[0].clone();let a = args[1].clone();
// (letrec () (print module (rustify-identifier p) ".set(") (a (quote gen-rust) module) (println module ");"))
{{
// (print module (rustify-identifier p) ".set(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier p)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[p.clone(),]),Scm::from(".set("),]);
// (a (quote gen-rust) module)
a.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),]);
// (println module ");")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(");"),])}}})},params.clone(),args_.clone(),]);
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}}})},])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone(),])).is_true() {
// (print)
imports::print.with(|value| value.get()).invoke(&[])} else if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone(),])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone(),])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone(),])).is_true() {Scm::symbol("SCOPE")} else if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone(),])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else {
// (error "Unknown message SCOPE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message SCOPE"),msg.clone(),])}}})});
self_.get()}})}));
        // (define (make-abstraction params vars body) (define (repr) (cons (quote ABSTRACTION) (cons params (body (quote repr))))) (define (transform func) (func self (lambda () (make-abstraction params vars (body (quote transform) func))))) (define (free-vars) (set-remove* (body (quote free-vars)) params)) (define (prepare-closure module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars))))) (define (gen-rust module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")")))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))) self)
        globals::make_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let body = args[2].clone();
// (letrec ((repr (lambda () (cons (quote ABSTRACTION) (cons params (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let prepare_minus_closure = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote ABSTRACTION) (cons params (body (quote repr)))))
{
// (cons (quote ABSTRACTION) (cons params (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("ABSTRACTION"),
// (cons params (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[params.clone(),
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"),]),]),])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))
{
// (func self (lambda () (make-abstraction params vars (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let params = params.clone();let vars = vars.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-abstraction params vars (body (quote transform) func)))
{
// (make-abstraction params vars (body (quote transform) func))
globals::make_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(),vars.clone(),
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone(),]),])}})},])}})});
free_minus_vars.set({let body = body.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (body (quote free-vars)) params))
{
// (set-remove* (body (quote free-vars)) params)
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"),]),params.clone(),])}})});
prepare_minus_closure.set({let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let free_minus_vars = args[1].clone();
// (letrec () (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))
{if (
// (pair? free-vars)
imports::pair_p.with(|value| value.get()).invoke(&[free_minus_vars.clone(),])).is_true() {
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let [name, ] = [
// (car free-vars)
imports::car.with(|value| value.get()).invoke(&[free_minus_vars.clone(),]),];{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),]);
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(),]),]);
// (print module " = ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = "),]);
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(),]),]);
// (print module ".clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".clone();"),]);
// (prepare-closure module (cdr free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),
// (cdr free-vars)
imports::cdr.with(|value| value.get()).invoke(&[free_minus_vars.clone(),]),])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});
gen_minus_rust.set({let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))))
{let gen_minus_params = Scm::uninitialized().into_boxed();
gen_minus_params.set({let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();
// (letrec () (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))
{if (
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone(),])).is_true() {{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),]);
// (print module (rustify-identifier (car p*)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier (car p*))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone(),]),]),]);
// (print module " = args[")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = args["),]);
// (print module k)
imports::print.with(|value| value.get()).invoke(&[module.clone(),k.clone(),]);
// (print module "].clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("].clone();"),]);
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone(),]),
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(),Scm::from(1),]),])}} else {Scm::symbol("*UNSPECIFIED*")}}})});

// (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")")))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let prepare_minus_closure = prepare_minus_closure.clone();let module = module.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))
{{
// (prepare-closure module (free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),
// (free-vars)
free_minus_vars.get().invoke(&[]),]);
// (print module "Scm::func(move |args: &[Scm]|")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::func(move |args: &[Scm]|"),]);
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))
{{
// (print module "if args.len() != ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if args.len() != "),]);
// (print module (length params))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone(),]),]);
// (print module "{panic!(\"invalid arity\")}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("{panic!(\"invalid arity\")}"),]);
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0),]);
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}}})},]);
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")"),])}}})},])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone(),])).is_true() {
// (print)
imports::print.with(|value| value.get()).invoke(&[])} else if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone(),])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone(),])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone(),])).is_true() {Scm::symbol("ABSTRACTION")} else if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone(),])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"),msg.clone(),])).is_true() {params.clone()} else if (
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"),msg.clone(),])).is_true() {vars.clone()} else if (
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"),msg.clone(),])).is_true() {body.clone()} else {
// (error "Unknown message ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ABSTRACTION"),msg.clone(),])}}})});
self_.get()}})}));
        // (define (make-vararg-abstraction params vararg vars varvar body) (define (repr) (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr))))) (define (transform func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))) (define (free-vars) (set-remove* (body (quote free-vars)) (cons vararg params))) (define (prepare-closure module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars))))) (define (gen-rust module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")")))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))) self)
        globals::make_minus_vararg_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let params = args[0].clone();let vararg = args[1].clone();let vars = args[2].clone();let varvar = args[3].clone();let body = args[4].clone();
// (letrec ((repr (lambda () (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let prepare_minus_closure = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr)))))
{
// (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("VARARG-ABSTRACTION"),
// (cons params (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[params.clone(),
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"),]),]),])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))
{
// (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))
{
// (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))
globals::make_minus_vararg_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(),vararg.clone(),vars.clone(),varvar.clone(),
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone(),]),])}})},])}})});
free_minus_vars.set({let body = body.clone();let vararg = vararg.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (body (quote free-vars)) (cons vararg params)))
{
// (set-remove* (body (quote free-vars)) (cons vararg params))
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"),]),
// (cons vararg params)
imports::cons.with(|value| value.get()).invoke(&[vararg.clone(),params.clone(),]),])}})});
prepare_minus_closure.set({let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let free_minus_vars = args[1].clone();
// (letrec () (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))
{if (
// (pair? free-vars)
imports::pair_p.with(|value| value.get()).invoke(&[free_minus_vars.clone(),])).is_true() {
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let [name, ] = [
// (car free-vars)
imports::car.with(|value| value.get()).invoke(&[free_minus_vars.clone(),]),];{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),]);
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(),]),]);
// (print module " = ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = "),]);
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(),]),]);
// (print module ".clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".clone();"),]);
// (prepare-closure module (cdr free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),
// (cdr free-vars)
imports::cdr.with(|value| value.get()).invoke(&[free_minus_vars.clone(),]),])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});
gen_minus_rust.set({let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let body = body.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))))
{let gen_minus_params = Scm::uninitialized().into_boxed();
gen_minus_params.set({let module = module.clone();let gen_minus_params = gen_minus_params.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();
// (letrec () (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);"))))
{if (
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone(),])).is_true() {{
// (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),
// (rustify-identifier (car p*))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone(),]),]),Scm::from(" = args["),k.clone(),Scm::from("].clone();"),]);
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone(),]),
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(),Scm::from(1),]),])}} else {
// (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),
// (rustify-identifier vararg)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[vararg.clone(),]),Scm::from(" = Scm::list(&args["),k.clone(),Scm::from("..]);"),])}}})});

// (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")")))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let prepare_minus_closure = prepare_minus_closure.clone();let module = module.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))) (print module ")"))
{{
// (prepare-closure module (free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),
// (free-vars)
free_minus_vars.get().invoke(&[]),]);
// (print module "Scm::func(move |args: &[Scm]|")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::func(move |args: &[Scm]|"),]);
// (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))
{{
// (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if args.len() < "),
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone(),]),Scm::from("{panic!(\"not enough args\")}"),]);
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0),]);
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}}})},]);
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")"),])}}})},])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone(),])).is_true() {
// (print)
imports::print.with(|value| value.get()).invoke(&[])} else if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone(),])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone(),])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone(),])).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone(),])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"),msg.clone(),])).is_true() {params.clone()} else if (
// (eq? (quote get-vararg) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vararg"),msg.clone(),])).is_true() {vararg.clone()} else if (
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"),msg.clone(),])).is_true() {vars.clone()} else if (
// (eq? (quote get-varvar) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-varvar"),msg.clone(),])).is_true() {varvar.clone()} else if (
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"),msg.clone(),])).is_true() {body.clone()} else {
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg.clone(),])}}})});
self_.get()}})}));
        // (define (make-program globals imports init body libraries) (define (repr) (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))) (define (transform func) (func self (lambda () (make-program globals imports init (body (quote transform) func))))) (define (gen-imports module) (for-each (lambda (i) (i (quote gen-rust) module)) imports)) (define (gen-rust module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))) self)
        globals::make_minus_program.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let globals = args[0].clone();let imports = args[1].clone();let init = args[2].clone();let body = args[3].clone();let libraries = args[4].clone();
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let gen_minus_imports = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let globals = globals.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))
{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("PROGRAM"),
// (cons globals (cons imports (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[globals.clone(),
// (cons imports (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[imports.clone(),
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"),]),]),]),])}})});
transform.set({let self_ = self_.clone();let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-program globals imports init (body (quote transform) func)))))
{
// (func self (lambda () (make-program globals imports init (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-program globals imports init (body (quote transform) func)))
{
// (make-program globals imports init (body (quote transform) func))
globals::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone(),]),])}})},])}})});
gen_minus_imports.set({let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec () (for-each (lambda (i) (i (quote gen-rust) module)) imports))
{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();
// (letrec () (i (quote gen-rust) module))
{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}})},imports.clone(),])}})});
gen_minus_rust.set({let gen_minus_imports = gen_minus_imports.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let libraries = libraries.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec () (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))
{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};"),]);
// (print module "mod imports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod imports"),]);
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let gen_minus_imports = gen_minus_imports.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (gen-imports module))
{
// (gen-imports module)
gen_minus_imports.get().invoke(&[module.clone(),])}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (print module "mod globals")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod globals"),]);
// (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let globals = globals.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))
{{if (
// (any (lambda (g) (global-regular? (cdr g))) globals)
imports::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
imports::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])}})},globals.clone(),])).is_true() {
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};"),])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone(),])}}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (print module "pub fn main()")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn main()"),]);
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))
{{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module "eprintln!(\"built with\");")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("eprintln!(\"built with\");"),]);
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);"),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec () (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module))
{{
// (print module "crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("crate::"),]);
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();
// (letrec () (print module (rustify-libname l)) (print module "::"))
{{
// (print module (rustify-libname l))
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-libname l)
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone(),]),]);
// (print module "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::"),])}}})},lib.clone(),]);
// (print module "initialize();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("initialize();"),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),])}}})},init.clone(),]);
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),]);
// (println module ";")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";"),])}}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules.with(|value| value.get()).invoke(&[module.clone(),libraries.clone(),])}}})});
self_.set({let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone(),])).is_true() {
// (print)
imports::print.with(|value| value.get()).invoke(&[])} else if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone(),])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone(),])).is_true() {Scm::symbol("PROGRAM")} else if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone(),])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else {
// (error "Unknown message PROGRAM" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message PROGRAM"),msg.clone(),])}}})});
self_.get()}})}));
        // (define (make-library name globals init body imports exports) (define (repr) (append (quote LIBRARY) name exports imports globals (body (quote repr)))) (define (transform func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports)))) (define (gen-exports module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)) (define (gen-rust module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))) self)
        globals::make_minus_library.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 6{panic!("invalid arity")}let name = args[0].clone();let globals = args[1].clone();let init = args[2].clone();let body = args[3].clone();let imports = args[4].clone();let exports = args[5].clone();
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let gen_minus_exports = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let name = name.clone();let exports = exports.clone();let imports = imports.clone();let globals = globals.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (append (quote LIBRARY) name exports imports globals (body (quote repr))))
{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
globals::append.with(|value| value.get()).invoke(&[Scm::symbol("LIBRARY"),name.clone(),exports.clone(),imports.clone(),globals.clone(),
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"),]),])}})});
transform.set({let self_ = self_.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))
{
// (func self (lambda () (make-library globals init (body (quote transform) func) imports exports)))
func.clone().invoke(&[self_.get(),{let globals = globals.clone();let init = init.clone();let body = body.clone();let func = func.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-library globals init (body (quote transform) func) imports exports))
{
// (make-library globals init (body (quote transform) func) imports exports)
globals::make_minus_library.with(|value| value.get()).invoke(&[globals.clone(),init.clone(),
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone(),]),imports.clone(),exports.clone(),])}})},])}})});
gen_minus_exports.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let exports = args[1].clone();
// (letrec () (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))
{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo = args[0].clone();
// (letrec () (expo (quote gen-rust) module))
{
// (expo (quote gen-rust) module)
expo.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}})},exports.clone(),])}})});
gen_minus_rust.set({let imports = imports.clone();let gen_minus_exports = gen_minus_exports.clone();let exports = exports.clone();let globals = globals.clone();let body = body.clone();let init = init.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();
// (letrec () (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))))
{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};"),]);
// (print module "mod imports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod imports"),]);
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (for-each (lambda (i) (i (quote gen-rust) module)) imports))
{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();
// (letrec () (i (quote gen-rust) module))
{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}})},imports.clone(),])}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (print module "pub mod exports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub mod exports"),]);
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let gen_minus_exports = gen_minus_exports.clone();let module = module.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (gen-exports module exports))
{
// (gen-exports module exports)
gen_minus_exports.get().invoke(&[module.clone(),exports.clone(),])}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (print module "mod globals")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod globals"),]);
// (rust-block module (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let globals = globals.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println module "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs module globals))
{{if (
// (any (lambda (g) (global-regular? (cdr g))) globals)
imports::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
imports::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])}})},globals.clone(),])).is_true() {
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};"),])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone(),])}}})},]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);if (
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("NOP"),
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind"),]),])).is_true() {
// (println module "pub fn initialize() {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn initialize() {"),])} else {{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }"),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),]);
// (println module "pub fn initialize() {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn initialize() {"),]);
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }"),]);
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));"),]);
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),])}};
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec () (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();"))
{{
// (print module "crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("crate::"),]);
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();
// (letrec () (print module (rustify-libname l) "::"))
{
// (print module (rustify-libname l) "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),
// (rustify-libname l)
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone(),]),Scm::from("::"),])}})},lib.clone(),]);
// (println module "initialize();")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("initialize();"),])}}})},init.clone(),]);
// (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests)))
{let [tests, ] = [
// (list (quote dummy))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("dummy"),]),];{
// ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module)

// (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
body.clone().invoke(&[Scm::symbol("transform"),{let tests = tests.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node = args[0].clone();let ignore = args[1].clone();
// (letrec () (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))
{if (
// (eq? (node (quote kind)) (quote TESTSUITE))
imports::eq_p.with(|value| value.get()).invoke(&[
// (node (quote kind))
node.clone().invoke(&[Scm::symbol("kind"),]),Scm::symbol("TESTSUITE"),])).is_true() {{
// (set-cdr! tests (cons node (cdr tests)))
imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[tests.clone(),
// (cons node (cdr tests))
imports::cons.with(|value| value.get()).invoke(&[node.clone(),
// (cdr tests)
imports::cdr.with(|value| value.get()).invoke(&[tests.clone(),]),]),]);
// (make-constant (quote *UNSPECIFIED*))
globals::make_minus_constant.with(|value| value.get()).invoke(&[Scm::symbol("*UNSPECIFIED*"),])}} else {
// (ignore)
ignore.clone().invoke(&[])}}})},]).invoke(&[Scm::symbol("gen-rust"),module.clone(),]);
// (println module ";}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";}"),]);
// (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test = args[0].clone();
// (letrec () (test (quote gen-rust) module))
{
// (test (quote gen-rust) module)
test.clone().invoke(&[Scm::symbol("gen-rust"),module.clone(),])}})},
// (cdr tests)
imports::cdr.with(|value| value.get()).invoke(&[tests.clone(),]),])}}}}})});
self_.set({let transform = transform.clone();let name = name.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone(),])).is_true() {
// (print)
imports::print.with(|value| value.get()).invoke(&[])} else if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone(),])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone(),])).is_true() {Scm::symbol("LIBRARY")} else if (
// (eq? (quote libname) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("libname"),msg.clone(),])).is_true() {name.clone()} else if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone(),])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(),]),])} else {
// (error "Unknown message LIBRARY" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message LIBRARY"),msg.clone(),])}}})});
self_.get()}})}));
        // (define (make-boxify name body) (define (repr) (cons (quote BOXIFY) (cons name (body (quote repr))))) (define (transform func) (func self (lambda () (make-boxify name (body (quote transform) func))))) (define (free-vars) (body (quote free-vars))) (define (gen-rust module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))) self)
        globals::make_minus_boxify.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let body = args[1].clone();
                    // (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let name = name.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote BOXIFY) (cons name (body (quote repr)))))
                                {
                                    // (cons (quote BOXIFY) (cons name (body (quote repr))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("BOXIFY"),
                                        // (cons name (body (quote repr)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            name.clone(),
                                            // (body (quote repr))
                                            body.clone().invoke(&[Scm::symbol("repr")]),
                                        ]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let name = name.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-boxify name (body (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-boxify name (body (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let name = name.clone();
                                        let body = body.clone();
                                        let func = func.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-boxify name (body (quote transform) func)))
                                            {
                                                // (make-boxify name (body (quote transform) func))
                                                globals::make_minus_boxify
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        name.clone(),
                                                        // (body (quote transform) func)
                                                        body.clone().invoke(&[
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
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (body (quote free-vars)))
                                {
                                    // (body (quote free-vars))
                                    body.clone().invoke(&[Scm::symbol("free-vars")])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let name = name.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))
                                {
                                    // (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module)))
                                    imports::rust_minus_block
                                        .with(|value| value.get())
                                        .invoke(&[module.clone(), {
                                            let module = module.clone();
                                            let name = name.clone();
                                            let body = body.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 0 {
                                                    panic!("invalid arity")
                                                }
                                                // (letrec () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))
                                                {
                                                    {
                                                        // (print module "let ")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from("let "),
                                                            ]);
                                                        // (print module (rustify-identifier name))
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                // (rustify-identifier name)
                                                                imports::rustify_minus_identifier
                                                                    .with(|value| value.get())
                                                                    .invoke(&[name.clone()]),
                                                            ]);
                                                        // (print module " = ")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from(" = "),
                                                            ]);
                                                        // (print module (rustify-identifier name))
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                // (rustify-identifier name)
                                                                imports::rustify_minus_identifier
                                                                    .with(|value| value.get())
                                                                    .invoke(&[name.clone()]),
                                                            ]);
                                                        // (print module ".into_boxed();")
                                                        imports::print
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                module.clone(),
                                                                Scm::from(".into_boxed();"),
                                                            ]);
                                                        // (body (quote gen-rust) module)
                                                        body.clone().invoke(&[
                                                            Scm::symbol("gen-rust"),
                                                            module.clone(),
                                                        ])
                                                    }
                                                }
                                            })
                                        }])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))
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
                                        Scm::symbol("BOXIFY")
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
                                        // (error "Unknown message BOXIFY" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message BOXIFY"),
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
        // (define (make-export env name exname) (define (repr) (list (quote EXPORT) name (quote AS) exname)) (define (transform func) (func self (lambda () self))) (define (gen-rust module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (print module "globals::")) ((eq? (quote IMPORT-REF) (variable-getter var)) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))) self)
        globals::make_minus_export.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let env = args[0].clone();
                    let name = args[1].clone();
                    let exname = args[2].clone();
                    // (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (print module "globals::")) ((eq? (quote IMPORT-REF) (variable-getter var)) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let name = name.clone();
                            let exname = exname.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote EXPORT) name (quote AS) exname))
                                {
                                    // (list (quote EXPORT) name (quote AS) exname)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("EXPORT"),
                                        name.clone(),
                                        Scm::symbol("AS"),
                                        exname.clone(),
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
                        gen_minus_rust.set({
                            let name = name.clone();
                            let env = env.clone();
                            let exname = exname.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (print module "globals::")) ((eq? (quote IMPORT-REF) (variable-getter var)) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))
                                {
                                    {
                                        // (print module "pub use super::")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("pub use super::"),
                                        ]);
                                        // (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (print module "globals::")) ((eq? (quote IMPORT-REF) (variable-getter var)) (print module "imports::")) (else (error "invalid export variable" var name))))
                                        {
                                            let [var] = [
                                                // (lookup name env)
                                                imports::lookup
                                                    .with(|value| value.get())
                                                    .invoke(&[name.clone(), env.clone()]),
                                            ];
                                            // (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (print module "globals::")) ((eq? (quote IMPORT-REF) (variable-getter var)) (print module "imports::")) (else (error "invalid export variable" var name)))
                                            if (
                                                // (not var)
                                                imports::not
                                                    .with(|value| value.get())
                                                    .invoke(&[var.clone()])
                                            )
                                            .is_true()
                                            {
                                                // (error "undefined export" name)
                                                imports::error.with(|value| value.get()).invoke(&[
                                                    Scm::from("undefined export"),
                                                    name.clone(),
                                                ])
                                            } else if (
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
                                                // (print module "globals::")
                                                imports::print.with(|value| value.get()).invoke(&[
                                                    module.clone(),
                                                    Scm::from("globals::"),
                                                ])
                                            } else if (
                                                // (eq? (quote IMPORT-REF) (variable-getter var))
                                                imports::eq_p.with(|value| value.get()).invoke(&[
                                                    Scm::symbol("IMPORT-REF"),
                                                    // (variable-getter var)
                                                    imports::variable_minus_getter
                                                        .with(|value| value.get())
                                                        .invoke(&[var.clone()]),
                                                ])
                                            )
                                            .is_true()
                                            {
                                                // (print module "imports::")
                                                imports::print.with(|value| value.get()).invoke(&[
                                                    module.clone(),
                                                    Scm::from("imports::"),
                                                ])
                                            } else {
                                                // (error "invalid export variable" var name)
                                                imports::error.with(|value| value.get()).invoke(&[
                                                    Scm::from("invalid export variable"),
                                                    var.clone(),
                                                    name.clone(),
                                                ])
                                            }
                                        };
                                        // (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
                                        imports::println.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            // (rustify-identifier name)
                                            imports::rustify_minus_identifier
                                                .with(|value| value.get())
                                                .invoke(&[name.clone()]),
                                            Scm::from(" as "),
                                            // (rustify-identifier exname)
                                            imports::rustify_minus_identifier
                                                .with(|value| value.get())
                                                .invoke(&[exname.clone()]),
                                            Scm::from(";"),
                                        ])
                                    }
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))
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
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("EXPORT")
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
                                        // (error "Unknown message EXPORT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message EXPORT"),
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
        // (define (make-import lib) (define (repr) (cons (quote IMPORT) lib)) (define (transform func) (func self (lambda () (make-import lib)))) (define (free-vars) (make-set)) (define (gen-libname module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib))))) (define (gen-rust module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))) self)
        globals::make_minus_import.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    // (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_libname = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let lib = lib.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote IMPORT) lib))
                                {
                                    // (cons (quote IMPORT) lib)
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("IMPORT"), lib.clone()])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let lib = lib.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-import lib))))
                                {
                                    // (func self (lambda () (make-import lib)))
                                    func.clone().invoke(&[self_.get(), {
                                        let lib = lib.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-import lib))
                                            {
                                                // (make-import lib)
                                                globals::make_minus_import
                                                    .with(|value| value.get())
                                                    .invoke(&[lib.clone()])
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
                        gen_minus_libname.set({
                            let gen_minus_libname = gen_minus_libname.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                let lib = args[1].clone();
                                // (letrec () (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))
                                {
                                    if (
                                        // (null? lib)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[lib.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print module "")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("")])
                                    } else {
                                        {
                                            // (print module (rustify-libname (car lib)))
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                // (rustify-libname (car lib))
                                                imports::rustify_minus_libname
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car lib)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                    ]),
                                            ]);
                                            if (
                                                // (null? (cdr lib))
                                                imports::null_p.with(|value| value.get()).invoke(&[
                                                    // (cdr lib)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ])
                                            )
                                            .is_true()
                                            {
                                                // (print module "")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("")])
                                            } else {
                                                // (print module "::")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("::")])
                                            };
                                            // (gen-libname module (cdr lib))
                                            gen_minus_libname.get().invoke(&[
                                                module.clone(),
                                                // (cdr lib)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[lib.clone()]),
                                            ])
                                        }
                                    }
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let gen_minus_libname = gen_minus_libname.clone();
                            let lib = lib.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))
                                {
                                    {
                                        // (print module "pub use crate::")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("pub use crate::"),
                                        ]);
                                        // (gen-libname module lib)
                                        gen_minus_libname
                                            .get()
                                            .invoke(&[module.clone(), lib.clone()]);
                                        // (print module "::exports::*;")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("::exports::*;")]);
                                        // (println module)
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone()])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))
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
                                        Scm::symbol("IMPORT")
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
                                        // (error "Unknown message IMPORT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message IMPORT"),
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
        // (define (make-import-only lib names) (define (repr) (cons (quote IMPORT-ONLY) (cons lib names))) (define (transform func) (func self (lambda () (make-import-only lib names)))) (define (free-vars) (make-set)) (define (gen-libname module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib))))) (define (gen-imports module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names))))) (define (gen-rust module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))) self)
        globals::make_minus_import_minus_only.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    let names = args[1].clone();
                    // (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_libname = Scm::uninitialized().into_boxed();
                        let gen_minus_imports = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let lib = lib.clone();
                            let names = names.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (cons (quote IMPORT-ONLY) (cons lib names)))
                                {
                                    // (cons (quote IMPORT-ONLY) (cons lib names))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("IMPORT-ONLY"),
                                        // (cons lib names)
                                        imports::cons
                                            .with(|value| value.get())
                                            .invoke(&[lib.clone(), names.clone()]),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let lib = lib.clone();
                            let names = names.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-import-only lib names))))
                                {
                                    // (func self (lambda () (make-import-only lib names)))
                                    func.clone().invoke(&[self_.get(), {
                                        let lib = lib.clone();
                                        let names = names.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-import-only lib names))
                                            {
                                                // (make-import-only lib names)
                                                globals::make_minus_import_minus_only
                                                    .with(|value| value.get())
                                                    .invoke(&[lib.clone(), names.clone()])
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
                        gen_minus_libname.set({
                            let gen_minus_libname = gen_minus_libname.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                let lib = args[1].clone();
                                // (letrec () (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))
                                {
                                    if (
                                        // (null? lib)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[lib.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (print module "")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("")])
                                    } else {
                                        {
                                            // (print module (rustify-libname (car lib)))
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                // (rustify-libname (car lib))
                                                imports::rustify_minus_libname
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car lib)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                    ]),
                                            ]);
                                            if (
                                                // (null? (cdr lib))
                                                imports::null_p.with(|value| value.get()).invoke(&[
                                                    // (cdr lib)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ])
                                            )
                                            .is_true()
                                            {
                                                // (print module "")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("")])
                                            } else {
                                                // (print module "::")
                                                imports::print
                                                    .with(|value| value.get())
                                                    .invoke(&[module.clone(), Scm::from("::")])
                                            };
                                            // (gen-libname module (cdr lib))
                                            gen_minus_libname.get().invoke(&[
                                                module.clone(),
                                                // (cdr lib)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[lib.clone()]),
                                            ])
                                        }
                                    }
                                }
                            })
                        });
                        gen_minus_imports.set({
                            let gen_minus_imports = gen_minus_imports.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                let names = args[1].clone();
                                // (letrec () (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))
                                {
                                    if (
                                        // (null? names)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[names.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("DONE")
                                    } else {
                                        {
                                            // (print module (rustify-identifier (car names)))
                                            imports::print.with(|value| value.get()).invoke(&[
                                                module.clone(),
                                                // (rustify-identifier (car names))
                                                imports::rustify_minus_identifier
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car names)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[names.clone()]),
                                                    ]),
                                            ]);
                                            // (print module ", ")
                                            imports::print
                                                .with(|value| value.get())
                                                .invoke(&[module.clone(), Scm::from(", ")]);
                                            // (gen-imports module (cdr names))
                                            gen_minus_imports.get().invoke(&[
                                                module.clone(),
                                                // (cdr names)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[names.clone()]),
                                            ])
                                        }
                                    }
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let gen_minus_libname = gen_minus_libname.clone();
                            let lib = lib.clone();
                            let gen_minus_imports = gen_minus_imports.clone();
                            let names = names.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))
                                {
                                    {
                                        // (print module "pub use crate::")
                                        imports::print.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("pub use crate::"),
                                        ]);
                                        // (gen-libname module lib)
                                        gen_minus_libname
                                            .get()
                                            .invoke(&[module.clone(), lib.clone()]);
                                        // (print module "::exports::{")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("::exports::{")]);
                                        // (gen-imports module names)
                                        gen_minus_imports
                                            .get()
                                            .invoke(&[module.clone(), names.clone()]);
                                        // (print module "};")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("};")]);
                                        // (println module)
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone()])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))
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
                                        Scm::symbol("IMPORT")
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
                                        // (error "Unknown message IMPORT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message IMPORT"),
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
        // (define (make-testcase description body) (define (repr) (list (quote TESTCASE) description body)) (define (transform func) (func self (lambda () (make-testcase description (body (quote transform) func))))) (define (gen-rust module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))) self)
        globals::make_minus_testcase.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let description = args[0].clone();
                    let body = args[1].clone();
                    // (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let description = description.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote TESTCASE) description body))
                                {
                                    // (list (quote TESTCASE) description body)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("TESTCASE"),
                                        description.clone(),
                                        body.clone(),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let description = description.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-testcase description (body (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-testcase description (body (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let description = description.clone();
                                        let body = body.clone();
                                        let func = func.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-testcase description (body (quote transform) func)))
                                            {
                                                // (make-testcase description (body (quote transform) func))
                                                globals::make_minus_testcase
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        description.clone(),
                                                        // (body (quote transform) func)
                                                        body.clone().invoke(&[
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
                        gen_minus_rust.set({
                            let description = description.clone();
                            let body = body.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))
                                {
                                    {
                                        // (println module "#[test]")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("#[test]")]);
                                        // (println module "fn " (rustify-testname description) "() {")
                                        imports::println.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("fn "),
                                            // (rustify-testname description)
                                            imports::rustify_minus_testname
                                                .with(|value| value.get())
                                                .invoke(&[description.clone()]),
                                            Scm::from("() {"),
                                        ]);
                                        // (println module "super::initialize();")
                                        imports::println.with(|value| value.get()).invoke(&[
                                            module.clone(),
                                            Scm::from("super::initialize();"),
                                        ]);
                                        // (body (quote gen-rust) module)
                                        body.clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (println module "}")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("}")])
                                    }
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))
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
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("TESTCASE")
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
                                        // (error "Unknown message TESTCASE" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message TESTCASE"),
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
        // (define (make-testsuite name cases) (define (repr) (list (quote TESTSUITE) name cases)) (define (transform func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))) (define (gen-rust module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))) self)
        globals::make_minus_testsuite.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let cases = args[1].clone();
                    // (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let name = name.clone();
                            let cases = cases.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote TESTSUITE) name cases))
                                {
                                    // (list (quote TESTSUITE) name cases)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("TESTSUITE"),
                                        name.clone(),
                                        cases.clone(),
                                    ])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let name = name.clone();
                            let cases = cases.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))
                                {
                                    // (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
                                    func.clone().invoke(&[self_.get(), {
                                        let name = name.clone();
                                        let func = func.clone();
                                        let cases = cases.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))
                                            {
                                                // (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
                                                globals::make_minus_testsuite
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        name.clone(),
                                                        // (map (lambda (c) (c (quote transform) func)) cases)
                                                        imports::map
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                {
                                                                    let func = func.clone();
                                                                    Scm::func(
                                                                        move |args: &[Scm]| {
                                                                            if args.len() != 1 {
                                                                                panic!(
                                                                                    "invalid arity"
                                                                                )
                                                                            }
                                                                            let c = args[0].clone();
                                                                            // (letrec () (c (quote transform) func))
                                                                            {
                                                                                // (c (quote transform) func)
                                                                                c.clone().invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    func.clone(),
                                                                                ])
                                                                            }
                                                                        },
                                                                    )
                                                                },
                                                                cases.clone(),
                                                            ]),
                                                    ])
                                            }
                                        })
                                    }])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let cases = cases.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))
                                {
                                    {
                                        // (println module "#[cfg(test)]")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("#[cfg(test)]")]);
                                        // (println module "mod tests {")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("mod tests {")]);
                                        // (println module "use super::*;")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("use super::*;")]);
                                        // (for-each (lambda (c) (c (quote gen-rust) module)) cases)
                                        imports::for_minus_each.with(|value| value.get()).invoke(
                                            &[
                                                {
                                                    let module = module.clone();
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 1 {
                                                            panic!("invalid arity")
                                                        }
                                                        let c = args[0].clone();
                                                        // (letrec () (c (quote gen-rust) module))
                                                        {
                                                            // (c (quote gen-rust) module)
                                                            c.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module.clone(),
                                                            ])
                                                        }
                                                    })
                                                },
                                                cases.clone(),
                                            ],
                                        );
                                        // (println module "}")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("}")])
                                    }
                                }
                            })
                        });
                        self_.set({
                            let transform = transform.clone();
                            let gen_minus_rust = gen_minus_rust.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() < 1 {
                                    panic!("not enough args")
                                }
                                let msg = args[0].clone();
                                let args_ = Scm::list(&args[1..]);
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))
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
                                        // (eq? (quote kind) msg)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("kind"), msg.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::symbol("TESTSUITE")
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
                                        // (error "Unknown message TESTSUITE" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message TESTSUITE"),
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
        // (define (make-assert condition) (define (repr) (list (quote ASSERT) condition)) (define (transform func) (func self (lambda () (make-assert (condition (quote transform) func))))) (define (free-vars) (condition (quote free-vars))) (define (gen-rust module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))) self)
        globals::make_minus_assert.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let condition = args[0].clone();
                    // (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
                    {
                        let repr = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        let free_minus_vars = Scm::uninitialized().into_boxed();
                        let gen_minus_rust = Scm::uninitialized().into_boxed();
                        let self_ = Scm::uninitialized().into_boxed();
                        repr.set({
                            let condition = condition.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (list (quote ASSERT) condition))
                                {
                                    // (list (quote ASSERT) condition)
                                    imports::list
                                        .with(|value| value.get())
                                        .invoke(&[Scm::symbol("ASSERT"), condition.clone()])
                                }
                            })
                        });
                        transform.set({
                            let self_ = self_.clone();
                            let condition = condition.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let func = args[0].clone();
                                // (letrec () (func self (lambda () (make-assert (condition (quote transform) func)))))
                                {
                                    // (func self (lambda () (make-assert (condition (quote transform) func))))
                                    func.clone().invoke(&[self_.get(), {
                                        let condition = condition.clone();
                                        let func = func.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 0 {
                                                panic!("invalid arity")
                                            }
                                            // (letrec () (make-assert (condition (quote transform) func)))
                                            {
                                                // (make-assert (condition (quote transform) func))
                                                globals::make_minus_assert
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (condition (quote transform) func)
                                                        condition.clone().invoke(&[
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
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 0 {
                                    panic!("invalid arity")
                                }
                                // (letrec () (condition (quote free-vars)))
                                {
                                    // (condition (quote free-vars))
                                    condition.clone().invoke(&[Scm::symbol("free-vars")])
                                }
                            })
                        });
                        gen_minus_rust.set({
                            let condition = condition.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let module = args[0].clone();
                                // (letrec () (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))
                                {
                                    {
                                        // (print module "assert!(")
                                        imports::print
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from("assert!(")]);
                                        // (condition (quote gen-rust) module)
                                        condition
                                            .clone()
                                            .invoke(&[Scm::symbol("gen-rust"), module.clone()]);
                                        // (println module ".is_true());")
                                        imports::println
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), Scm::from(".is_true());")])
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
                                // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))
                                {
                                    // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))
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
                                        Scm::symbol("ASSERT")
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
                                        // (error "Unknown message ASSERT" msg)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from("Unknown message ASSERT"),
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
