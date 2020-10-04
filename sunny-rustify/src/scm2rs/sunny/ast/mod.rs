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
    pub use super::globals::ast_minus_node_p;
    pub use super::globals::make_minus_abstraction;
    pub use super::globals::make_minus_alternative;
    pub use super::globals::make_minus_application;
    pub use super::globals::make_minus_args;
    pub use super::globals::make_minus_assert;
    pub use super::globals::make_minus_assignment;
    pub use super::globals::make_minus_boxify;
    pub use super::globals::make_minus_closure;
    pub use super::globals::make_minus_comment;
    pub use super::globals::make_minus_constant;
    pub use super::globals::make_minus_definition;
    pub use super::globals::make_minus_export;
    pub use super::globals::make_minus_fixlet;
    pub use super::globals::make_minus_import;
    pub use super::globals::make_minus_import_minus_only;
    pub use super::globals::make_minus_library;
    pub use super::globals::make_minus_nop;
    pub use super::globals::make_minus_null_minus_arg;
    pub use super::globals::make_minus_program;
    pub use super::globals::make_minus_reference;
    pub use super::globals::make_minus_sequence;
    pub use super::globals::make_minus_testcase;
    pub use super::globals::make_minus_testsuite;
    pub use super::globals::make_minus_vararg_minus_abstraction;
    pub use super::globals::procedure_minus_node_p;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static ast_minus_node_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION ast-node?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-assignment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-boxify"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_closure: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-closure"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_comment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-comment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_definition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-definition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_nop: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-nop"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_null_minus_arg: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-null-arg"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-program"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-reference"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION make-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static procedure_minus_node_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION procedure-node?"))}
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
        {
            // (define (ast-node? obj) ...)
            globals::ast_minus_node_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let obj = args[0].clone();
                        {
                            // (procedure? obj)
                            imports::procedure_p
                                .with(|value| value.get())
                                .invoke(&[obj.clone()])
                        }
                    })
                })
            })
        };
        {
            // (define (procedure-node? obj) ...)
            globals::procedure_minus_node_p.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let obj = args[0].clone();
                        if ({
                            // (eq? (obj (quote kind)) (quote ABSTRACTION))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                {
                                    // (obj (quote kind))
                                    obj.clone().invoke(&[Scm::symbol("kind")])
                                },
                                Scm::symbol("ABSTRACTION"),
                            ])
                        })
                        .is_true()
                        {
                            Scm::True
                        } else {
                            {
                                // (eq? (obj (quote kind)) (quote VARARG-ABSTRACTION))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    {
                                        // (obj (quote kind))
                                        obj.clone().invoke(&[Scm::symbol("kind")])
                                    },
                                    Scm::symbol("VARARG-ABSTRACTION"),
                                ])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (make-comment comment node) ...)
            globals::make_minus_comment.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let comment = args[0].clone();let node = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, gen_minus_rust_minus_inner, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("COMMENT"),comment.clone(),{
// (node (quote repr))
node.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func.clone().invoke(&[self_.get(),{let comment = comment.clone();let node = node.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-comment comment (node (quote transform) func))
globals::make_minus_comment.with(|value| value.get()).invoke(&[comment.clone(),{
// (node (quote transform) func)
node.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (node (quote free-vars))
node.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "// ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln.with(|value| value.get()).invoke(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust) module)
node.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});gen_minus_rust_minus_inner.set({let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "// ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln.with(|value| value.get()).invoke(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust-inner) module)
node.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote inner) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("inner"),msg.clone()])}).is_true() {node.clone()} else {{
// (error "Unknown message COMMENT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message COMMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-nop) ...)
            globals::make_minus_nop.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (letrec ((repr (lambda () (quote (NOP)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module "(/*NOP*/)"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (quote (NOP)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module "(/*NOP*/)"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}Scm::pair(Scm::symbol("NOP"), Scm::Nil)})});transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_rust.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (print module "(/*NOP*/)")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("(/*NOP*/)")])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("NOP")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message NOP" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message NOP"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-constant val) ...)
            globals::make_minus_constant.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let val = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_constant, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_constant = gen_minus_constant.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("CONSTANT"),val.clone()])}})});transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_constant.set({let gen_minus_constant = gen_minus_constant.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let val = args[1].clone();{
// (cond ...)
if ({
// (null? val)
imports::null_p.with(|value| value.get()).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::Nil")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
imports::eq_p.with(|value| value.get()).invoke(&[val.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
imports::eq_p.with(|value| value.get()).invoke(&[val.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
imports::symbol_p.with(|value| value.get()).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::symbol(\""),val.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
imports::eq_p.with(|value| value.get()).invoke(&[val.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
imports::char_p.with(|value| value.get()).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::char('"),val.clone(),Scm::from("')")])}} else if ({
// (pair? val)
imports::pair_p.with(|value| value.get()).invoke(&[val.clone()])}).is_true() {{{
// (print module "Scm::pair(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (car val)
imports::car.with(|value| value.get()).invoke(&[val.clone()])}])};{
// (print module ", ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (cdr val)
imports::cdr.with(|value| value.get()).invoke(&[val.clone()])}])};{
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::from(")])};{
// (show module val)
imports::show.with(|value| value.get()).invoke(&[module.clone(),val.clone()])};{
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")")])}}}}})});gen_minus_rust.set({let gen_minus_constant = gen_minus_constant.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (gen-constant module val)
gen_minus_constant.get().invoke(&[module.clone(),val.clone()])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message CONSTANT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-reference name var) ...)
            globals::make_minus_reference.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();{
// (letrec ((repr (lambda () (list (quote GET) name))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) name)))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((import-variable? var) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) name))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) name)))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((import-variable? var) (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) name)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("GET"),name.clone()])}})});transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({let var = var.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor.with(|value| value.get()).invoke(&[{
// (global-variable? var)
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])},{
// (global-function? var)
imports::global_minus_function_p.with(|value| value.get()).invoke(&[var.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}} else {{
// (set-add (make-set) name)
imports::set_minus_add.with(|value| value.get()).invoke(&[{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])},name.clone()])}}})});gen_minus_rust.set({let var = var.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
imports::global_minus_function_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "globals::" (rustify-identifier name) ".with(|value| value.get())")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "imports::" (rustify-identifier name) ".with(|value| value.get())")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("imports::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".get()")
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".get()")])}} else {{
// (print module (rustify-identifier name) ".clone()")
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".clone()")])}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-var"),msg.clone()])}).is_true() {var.clone()} else {{
// (error "Unknown message REFERENCE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message REFERENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-assignment name var val) ...)
            globals::make_minus_assignment.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();let val = args[2].clone();{
// (letrec ((repr (lambda () (list (quote SET!) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) name (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) name))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) name (val (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("SET!"),name.clone(),{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-assignment name var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{let name = name.clone();let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment name var (val (quote transform) func))
globals::make_minus_assignment.with(|value| value.get()).invoke(&[name.clone(),var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let val = val.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) name)
imports::set_minus_add.with(|value| value.get()).invoke(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},name.clone()])}})});gen_minus_rust.set({let var = var.clone();let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{{
// (print module "globals::" (rustify-identifier name) ".with(|value| value.set(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("))")])}}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".set(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")")])}}} else {{
// (error "set! on unboxed variable" name var)
imports::error.with(|value| value.get()).invoke(&[Scm::from("set! on unboxed variable"),name.clone(),var.clone()])}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ASSIGNMENT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ASSIGNMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-definition name var val) ...)
            globals::make_minus_definition.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();let val = args[2].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) name (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition name var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) name))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) name (val (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("DEFINE"),name.clone(),{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-definition name var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{let name = name.clone();let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition name var (val (quote transform) func))
globals::make_minus_definition.with(|value| value.get()).invoke(&[name.clone(),var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let val = val.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) name)
imports::set_minus_add.with(|value| value.get()).invoke(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},name.clone()])}})});gen_minus_rust.set({let var = var.clone();let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{{
// (print module "globals::" (rustify-identifier name) ".with(|value| value.set(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("))")])}}} else if ({
// (global-function? var)
imports::global_minus_function_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{{
// (print module "globals::" (rustify-identifier name) ".with(|value| value.set(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("))")])}}} else {{
// (error "definition! of non-global variable")
imports::error.with(|value| value.get()).invoke(&[Scm::from("definition! of non-global variable")])}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message DEFINITION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message DEFINITION"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-alternative condition consequent alternative) ...)
            globals::make_minus_alternative.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let condition = args[0].clone();let consequent = args[1].clone();let alternative = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("IF"),{
// (condition (quote repr))
condition.clone().invoke(&[Scm::symbol("repr")])},{
// (consequent (quote repr))
consequent.clone().invoke(&[Scm::symbol("repr")])},{
// (alternative (quote repr))
alternative.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func.clone().invoke(&[self_.get(),{let condition = condition.clone();let func = func.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
globals::make_minus_alternative.with(|value| value.get()).invoke(&[{
// (condition (quote transform) func)
condition.clone().invoke(&[Scm::symbol("transform"),func.clone()])},{
// (consequent (quote transform) func)
consequent.clone().invoke(&[Scm::symbol("transform"),func.clone()])},{
// (alternative (quote transform) func)
alternative.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (condition (quote free-vars))
condition.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "if (")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ").is_true() {")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "} else ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
imports::eq_p.with(|value| value.get()).invoke(&[{
// (alternative (quote kind))
alternative.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}} else {{{
// (print module "{")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("}")])}}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ALTERNATIVE"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-sequence first next) ...)
            globals::make_minus_sequence.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let first = args[0].clone();let next = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("SEQUENCE"),{
// (first (quote repr))
first.clone().invoke(&[Scm::symbol("repr")])},{
// (next (quote repr))
next.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let next = next.clone();let first = first.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))))
func.clone().invoke(&[self_.get(),{let next = next.clone();let func = func.clone();let first = first.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))
{
// (let ((a (first (quote transform) func))) (let ((b (next (quote transform) func))) (begin (make-sequence a b))))
{let a = {
// (first (quote transform) func)
first.clone().invoke(&[Scm::symbol("transform"),func.clone()])};
// (let ((b (next (quote transform) func))) (begin (make-sequence a b)))
let b = {
// (next (quote transform) func)
next.clone().invoke(&[Scm::symbol("transform"),func.clone()])};{
// (make-sequence a b)
globals::make_minus_sequence.with(|value| value.get()).invoke(&[a.clone(),b.clone()])}}}}})}])}})});free_minus_vars.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (first (quote free-vars))
first.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (first (quote gen-rust) module)
first.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else {{
// (next (quote gen-rust) module)
next.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}}})});gen_minus_rust.set({let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "{")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])};{
// (print module "}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message SEQUENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-application func args tail?) ...)
            globals::make_minus_application.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let func = args[0].clone();let args_ = args[1].clone();let tail_p = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let tail_p = tail_p.clone();let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[if (tail_p.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[{
// (func (quote repr))
func.clone().invoke(&[Scm::symbol("repr")])},{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])}])}])}})});transform.set({let self_ = self_.clone();let func = func.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc.clone().invoke(&[self_.get(),{let func = func.clone();let fnc = fnc.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
globals::make_minus_application.with(|value| value.get()).invoke(&[{
// (func (quote transform) fnc)
func.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},tail_p.clone()])}})}])}})});free_minus_vars.set({let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (func (quote free-vars))
func.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (func (quote gen-rust) module)
func.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ".invoke(&[")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "])")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("])")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message APPLICATION"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-null-arg) ...)
            globals::make_minus_null_minus_arg.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module ""))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote NULL-ARG)))) (set! transform (lambda (fnc) (fnc self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module ""))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote NULL-ARG))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("NULL-ARG")])}})});transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () self))
fnc.clone().invoke(&[self_.get(),{let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_rust.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (print module "")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("")])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("NULL-ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message NULL-ARG" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message NULL-ARG"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-args arg next) ...)
            globals::make_minus_args.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let arg = args[0].clone();let next = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote ARG) (cons arg next)))) (set! transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (set! gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let arg = arg.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote ARG) (cons arg next))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("ARG"),{
// (cons arg next)
imports::cons.with(|value| value.get()).invoke(&[arg.clone(),next.clone()])}])}})});transform.set({let self_ = self_.clone();let arg = arg.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
fnc.clone().invoke(&[self_.get(),{let arg = arg.clone();let fnc = fnc.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
globals::make_minus_args.with(|value| value.get()).invoke(&[{
// (arg (quote transform) fnc)
arg.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (next (quote transform) fnc)
next.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])}])}})}])}})});free_minus_vars.set({let arg = arg.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (arg (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (arg (quote free-vars))
arg.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({let arg = arg.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (arg (quote gen-rust) module)
arg.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};if ({
// (not (eq? (quote NULL-ARG) (next (quote kind))))
imports::not.with(|value| value.get()).invoke(&[{
// (eq? (quote NULL-ARG) (next (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("NULL-ARG"),{
// (next (quote kind))
next.clone().invoke(&[Scm::symbol("kind")])}])}])}).is_true() {{{
// (print module ",")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(",")])};{
// (next (quote gen-rust) module)
next.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ARG" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ARG"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-fixlet params vars args body) ...)
            globals::make_minus_fixlet.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 4{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let args_ = args[2].clone();let body = args[3].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) params (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) params (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) params (args (quote repr)) (body (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("FIXLET"),params.clone(),{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc.clone().invoke(&[self_.get(),{let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let fnc = fnc.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc))
globals::make_minus_fixlet.with(|value| value.get()).invoke(&[params.clone(),vars.clone(),{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])}])}})}])}})});free_minus_vars.set({let body = body.clone();let params = params.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))
imports::set_minus_union.with(|value| value.get()).invoke(&[{
// (set-remove* (body (quote free-vars)) params)
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},params.clone()])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*)))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p_star_ = args[0].clone();if ({
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module (rustify-identifier (car p*)) ", ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier (car p*))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone()])}])},Scm::from(", ")])};{
// (gen-params (cdr p*))
gen_minus_params.get().invoke(&[{
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (cond ...)
if ({
// (= 0 (length params))
imports::_e_.with(|value| value.get()).invoke(&[Scm::from(0),{
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length params))
imports::_e_.with(|value| value.get()).invoke(&[Scm::from(1),{
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone()])}])}).is_true() {{{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (car params)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier (car params))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (car params)
imports::car.with(|value| value.get()).invoke(&[params.clone()])}])}])};{
// (print module " = ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let [")])};{
// (gen-params params)
gen_minus_params.get().invoke(&[params.clone()])};{
// (print module "] = [")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "];")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("FIXLET"),{
// ((body (quote inner)) (quote kind))
{
// (body (quote inner))
body.clone().invoke(&[Scm::symbol("inner")])}.invoke(&[Scm::symbol("kind")])}])}} else {Scm::False}}).is_true() {{
// (body (quote gen-rust-inner) module)
body.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else {{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}}}}}}})});gen_minus_rust.set({let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (gen-rust-inner module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])}})}])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let arg_star_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car arg*))
transform.get().invoke(&[{
// (car arg*)
imports::car.with(|value| value.get()).invoke(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-args) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-args"),msg.clone()])}).is_true() {args_.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust.get().invoke(&[{
// (car arg*)
imports::car.with(|value| value.get()).invoke(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car arg*)
imports::car.with(|value| value.get()).invoke(&[arg_star_.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message FIXLET"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-closure function) ...)
            globals::make_minus_closure.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let function = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr, transform, free_minus_vars, prepare_minus_closure, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let prepare_minus_closure = prepare_minus_closure.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("CLOSURE"),function.clone()])}})});transform.set({let self_ = self_.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-closure (function (quote transform) func))))
func.clone().invoke(&[self_.get(),{let function = function.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-closure (function (quote transform) func))
globals::make_minus_closure.with(|value| value.get()).invoke(&[{
// (function (quote transform) func)
function.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (function (quote free-vars))
function.clone().invoke(&[Scm::symbol("free-vars")])}})});prepare_minus_closure.set({let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let free_minus_vars = args[1].clone();if ({
// (pair? free-vars)
imports::pair_p.with(|value| value.get()).invoke(&[free_minus_vars.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name = {
// (car free-vars)
imports::car.with(|value| value.get()).invoke(&[free_minus_vars.clone()])};{{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])}])};{
// (print module " = ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])}])};{
// (print module ".clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (cdr free-vars)
imports::cdr.with(|value| value.get()).invoke(&[free_minus_vars.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});gen_minus_rust.set({let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let prepare_minus_closure = prepare_minus_closure.clone();let module = module.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (prepare-closure module (free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (free-vars)
free_minus_vars.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(")")])}}})}])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("inner-function"),msg.clone()])}).is_true() {function.clone()} else {{
// (error "Unknown message CLOSURE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message CLOSURE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-abstraction params vars body) ...)
            globals::make_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let body = args[2].clone();{
// (letrec ((repr (lambda () (list (quote ABSTRACTION) params (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ABSTRACTION) params (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ABSTRACTION) params (body (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("ABSTRACTION"),params.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-abstraction params vars (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let params = params.clone();let vars = vars.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-abstraction params vars (body (quote transform) func))
globals::make_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(),vars.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let body = body.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) params)
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},params.clone()])}})});gen_minus_rust.set({let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (car p*)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier (car p*))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone()])}])}])};{
// (print module " = args[")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = args[")])};{
// (print module k)
imports::print.with(|value| value.get()).invoke(&[module.clone(),k.clone()])};{
// (print module "].clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone()])},{
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if args.len() != ")])};{
// (print module (length params))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-vararg-abstraction params vararg vars varvar body) ...)
            globals::make_minus_vararg_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let params = args[0].clone();let vararg = args[1].clone();let vars = args[2].clone();let varvar = args[3].clone();let body = args[4].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) params (body (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("VARARG-ABSTRACTION"),params.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))
globals::make_minus_vararg_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(),vararg.clone(),vars.clone(),varvar.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let body = body.clone();let vararg = vararg.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (cons vararg params))
imports::set_minus_remove_star_.with(|value| value.get()).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},{
// (cons vararg params)
imports::cons.with(|value| value.get()).invoke(&[vararg.clone(),params.clone()])}])}})});gen_minus_rust.set({let vararg = vararg.clone();let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({let module = module.clone();let gen_minus_params = gen_minus_params.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),{
// (rustify-identifier (car p*))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone()])}])},Scm::from(" = args["),k.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone()])},{
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let "),{
// (rustify-identifier vararg)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[vararg.clone()])},Scm::from(" = Scm::list(&args["),k.clone(),Scm::from("..]);")])}}})});{
// (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if args.len() < "),{
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vararg) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vararg"),msg.clone()])}).is_true() {vararg.clone()} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-varvar) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-varvar"),msg.clone()])}).is_true() {varvar.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-program globals imports init body libraries) ...)
            globals::make_minus_program.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let globals = args[0].clone();let imports = args[1].clone();let init = args[2].clone();let body = args[3].clone();let libraries = args[4].clone();{
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals))) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (set! transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (set! gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals))) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))) self))
{let [repr, transform, gen_minus_imports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_imports = gen_minus_imports.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let globals = globals.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[globals.clone(),{
// (cons imports (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[imports.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}])}])}})});transform.set({let self_ = self_.clone();let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-program globals imports init (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-program globals imports init (body (quote transform) func))
globals::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});gen_minus_imports.set({let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})});gen_minus_rust.set({let gen_minus_imports = gen_minus_imports.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let libraries = libraries.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let gen_minus_imports = gen_minus_imports.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports.get().invoke(&[module.clone()])}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "mod globals")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod globals")])};{
// (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let globals = globals.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};")])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone()])}}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "pub fn main()")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module "eprintln!(\"built with\");")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{{
// (print module (rustify-libname l))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone()])}])};{
// (print module "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::")])}}})},lib.clone()])};{
// (print module "initialize();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("initialize();")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])}}})},init.clone()])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";")])}}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules.with(|value| value.get()).invoke(&[module.clone(),libraries.clone()])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message PROGRAM"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-library name globals init body imports exports) ...)
            globals::make_minus_library.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 6{panic!("invalid arity")}let name = args[0].clone();let globals = args[1].clone();let init = args[2].clone();let body = args[3].clone();let imports = args[4].clone();let exports = args[5].clone();{
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals))) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-exports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))) (set! gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (print module "mod globals") (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals))) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))) self))
{let [repr, transform, gen_minus_exports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_exports = gen_minus_exports.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let exports = exports.clone();let imports = imports.clone();let globals = globals.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
imports::append.with(|value| value.get()).invoke(&[Scm::symbol("LIBRARY"),name.clone(),exports.clone(),imports.clone(),globals.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({let self_ = self_.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-library globals init (body (quote transform) func) imports exports)))
func.clone().invoke(&[self_.get(),{let globals = globals.clone();let init = init.clone();let body = body.clone();let func = func.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-library globals init (body (quote transform) func) imports exports)
globals::make_minus_library.with(|value| value.get()).invoke(&[globals.clone(),init.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])},imports.clone(),exports.clone()])}})}])}})});gen_minus_exports.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let exports = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo = args[0].clone();{
// (expo (quote gen-rust) module)
expo.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},exports.clone()])}})});gen_minus_rust.set({let imports = imports.clone();let gen_minus_exports = gen_minus_exports.clone();let exports = exports.clone();let globals = globals.clone();let body = body.clone();let init = init.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "pub mod exports")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let gen_minus_exports = gen_minus_exports.clone();let module = module.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports.get().invoke(&[module.clone(),exports.clone()])}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (print module "mod globals")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod globals")])};{
// (rust-block module (lambda () (println module "use sunny_core::{Mut, Scm};") (rust-gen-global-defs module globals)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let globals = globals.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};")])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone()])}}})}])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("NOP"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])};{
// (println module "pub fn initialize() {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{
// (print module (rustify-libname l) "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone()])},Scm::from("::")])}})},lib.clone()])};{
// (println module "initialize();")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("initialize();")])}}})},init.clone()])};{
// (let ((tests (list (quote dummy)))) ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests)))
{let tests = {
// (list (quote dummy))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("dummy")])};{{
// ((body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))) (quote gen-rust) module)
{
// (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
body.clone().invoke(&[Scm::symbol("transform"),{let tests = tests.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let node = args[0].clone();let ignore = args[1].clone();if ({
// (eq? (node (quote kind)) (quote TESTSUITE))
imports::eq_p.with(|value| value.get()).invoke(&[{
// (node (quote kind))
node.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("TESTSUITE")])}).is_true() {{{
// (set-cdr! tests (cons node (cdr tests)))
imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[tests.clone(),{
// (cons node (cdr tests))
imports::cons.with(|value| value.get()).invoke(&[node.clone(),{
// (cdr tests)
imports::cdr.with(|value| value.get()).invoke(&[tests.clone()])}])}])};{
// (make-constant (quote *UNSPECIFIED*))
globals::make_minus_constant.with(|value| value.get()).invoke(&[Scm::symbol("*UNSPECIFIED*")])}}} else {{
// (ignore)
ignore.clone().invoke(&[])}}})}])}.invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) (cdr tests))
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test = args[0].clone();{
// (test (quote gen-rust) module)
test.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},{
// (cdr tests)
imports::cdr.with(|value| value.get()).invoke(&[tests.clone()])}])}}}}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let name = name.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("libname"),msg.clone()])}).is_true() {name.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message LIBRARY"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-boxify name body) ...)
            globals::make_minus_boxify.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let body = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (set! transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote BOXIFY) (cons name (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("BOXIFY"),{
// (cons name (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[name.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}])}})});transform.set({let self_ = self_.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-boxify name (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let name = name.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-boxify name (body (quote transform) func))
globals::make_minus_boxify.with(|value| value.get()).invoke(&[name.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module)))
imports::rust_minus_block.with(|value| value.get()).invoke(&[module.clone(),{let module = module.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])}])};{
// (print module " = ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])}])};{
// (print module ".into_boxed();")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message BOXIFY"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-export env name exname) ...)
            globals::make_minus_export.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let env = args[0].clone();let name = args[1].clone();let exname = args[2].clone();{
// (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "globals::")) ((global-function? var) (print module "globals::")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "globals::")) ((global-function? var) (print module "globals::")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))) self))
{let [repr, transform, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote EXPORT) name (quote AS) exname)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("EXPORT"),name.clone(),Scm::symbol("AS"),exname.clone()])}})});transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});gen_minus_rust.set({let name = name.clone();let env = env.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use super::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub use super::")])};{
// (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "globals::")) ((global-function? var) (print module "globals::")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))))
{let var = {
// (lookup name env)
imports::lookup.with(|value| value.get()).invoke(&[name.clone(),env.clone()])};{
// (cond ...)
if ({
// (not var)
imports::not.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (error "undefined export" name)
imports::error.with(|value| value.get()).invoke(&[Scm::from("undefined export"),name.clone()])}} else if ({
// (global-variable? var)
imports::global_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "globals::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::")])}} else if ({
// (global-function? var)
imports::global_minus_function_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "globals::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("globals::")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p.with(|value| value.get()).invoke(&[var.clone()])}).is_true() {{
// (print module "imports::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
imports::error.with(|value| value.get()).invoke(&[Scm::from("invalid export variable"),var.clone(),name.clone()])}}}}};{
// (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
imports::println.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone()])},Scm::from(" as "),{
// (rustify-identifier exname)
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[exname.clone()])},Scm::from(";")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("EXPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message EXPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message EXPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}})}))
        };
        {
            // (define (make-import lib) ...)
            globals::make_minus_import.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT"),lib.clone()])}})});transform.set({let self_ = self_.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import lib)))
func.clone().invoke(&[self_.get(),{let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
globals::make_minus_import.with(|value| value.get()).invoke(&[lib.clone()])}})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_libname.set({let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
imports::null_p.with(|value| value.get()).invoke(&[lib.clone()])}).is_true() {{
// (print module "")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[{
// (car lib)
imports::car.with(|value| value.get()).invoke(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p.with(|value| value.get()).invoke(&[{
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone()])}])}).is_true() {{
// (print module "")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone()])}])}}}})});gen_minus_rust.set({let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::*;")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::exports::*;")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}})}))
        };
        {
            // (define (make-import-only lib names) ...)
            globals::make_minus_import_minus_only.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let lib = args[0].clone();let names = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_imports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_imports = gen_minus_imports.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
imports::cons.with(|value| value.get()).invoke(&[lib.clone(),names.clone()])}])}})});transform.set({let self_ = self_.clone();let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func.clone().invoke(&[self_.get(),{let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
globals::make_minus_import_minus_only.with(|value| value.get()).invoke(&[lib.clone(),names.clone()])}})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_libname.set({let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
imports::null_p.with(|value| value.get()).invoke(&[lib.clone()])}).is_true() {{
// (print module "")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[{
// (car lib)
imports::car.with(|value| value.get()).invoke(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p.with(|value| value.get()).invoke(&[{
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone()])}])}).is_true() {{
// (print module "")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone()])}])}}}})});gen_minus_imports.set({let gen_minus_imports = gen_minus_imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let names = args[1].clone();if ({
// (null? names)
imports::null_p.with(|value| value.get()).invoke(&[names.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
imports::print.with(|value| value.get()).invoke(&[module.clone(),{
// (rustify-identifier (car names))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[{
// (car names)
imports::car.with(|value| value.get()).invoke(&[names.clone()])}])}])};{
// (print module ", ")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports.get().invoke(&[module.clone(),{
// (cdr names)
imports::cdr.with(|value| value.get()).invoke(&[names.clone()])}])}}}})});gen_minus_rust.set({let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();let gen_minus_imports = gen_minus_imports.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::{")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports.get().invoke(&[module.clone(),names.clone()])};{
// (print module "};")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("};")])};{
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone()])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}})}))
        };
        {
            // (define (make-testcase description body) ...)
            globals::make_minus_testcase.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let description = args[0].clone();let body = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTCASE) description body)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("TESTCASE"),description.clone(),body.clone()])}})});transform.set({let self_ = self_.clone();let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-testcase description (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{let description = description.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testcase description (body (quote transform) func))
globals::make_minus_testcase.with(|value| value.get()).invoke(&[description.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_rust.set({let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[test]")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[test]")])};{
// (println module "fn " (rustify-testname description) "() {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("fn "),{
// (rustify-testname description)
imports::rustify_minus_testname.with(|value| value.get()).invoke(&[description.clone()])},Scm::from("() {")])};{
// (println module "super::initialize();")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("super::initialize();")])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module "}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("TESTCASE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message TESTCASE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message TESTCASE"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-testsuite name cases) ...)
            globals::make_minus_testsuite.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let cases = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("TESTSUITE"),name.clone(),cases.clone()])}})});transform.set({let self_ = self_.clone();let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func.clone().invoke(&[self_.get(),{let name = name.clone();let func = func.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
globals::make_minus_testsuite.with(|value| value.get()).invoke(&[name.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
imports::map.with(|value| value.get()).invoke(&[{let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote transform) func)
c.clone().invoke(&[Scm::symbol("transform"),func.clone()])}})},cases.clone()])}])}})}])}})});free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}})});gen_minus_rust.set({let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[cfg(test)]")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote gen-rust) module)
c.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},cases.clone()])};{
// (println module "}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message TESTSUITE"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        };
        {
            // (define (make-assert condition) ...)
            globals::make_minus_assert.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let condition = args[0].clone();{
// (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ASSERT) condition))) (set! transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (set! free-vars (lambda () (condition (quote free-vars)))) (set! gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({let condition = condition.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ASSERT) condition)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("ASSERT"),condition.clone()])}})});transform.set({let self_ = self_.clone();let condition = condition.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-assert (condition (quote transform) func))))
func.clone().invoke(&[self_.get(),{let condition = condition.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assert (condition (quote transform) func))
globals::make_minus_assert.with(|value| value.get()).invoke(&[{
// (condition (quote transform) func)
condition.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({let condition = condition.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (condition (quote free-vars))
condition.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({let condition = condition.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "assert!(")
imports::print.with(|value| value.get()).invoke(&[module.clone(),Scm::from("assert!(")])};{
// (condition (quote gen-rust) module)
condition.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ".is_true());")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from(".is_true());")])}}})});self_.set({let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ASSERT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ASSERT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ASSERT"),msg.clone()])}}}})});self_.get()}}}}}}}}}})}))
        }
    };
}
