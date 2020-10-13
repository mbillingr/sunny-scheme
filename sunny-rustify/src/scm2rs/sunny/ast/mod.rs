#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::table::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::ast_minus_node_p;
    pub use super::make_minus_abstraction;
    pub use super::make_minus_alternative;
    pub use super::make_minus_application;
    pub use super::make_minus_args;
    pub use super::make_minus_assert;
    pub use super::make_minus_assignment;
    pub use super::make_minus_boxify;
    pub use super::make_minus_closure;
    pub use super::make_minus_comment;
    pub use super::make_minus_constant;
    pub use super::make_minus_definition;
    pub use super::make_minus_export;
    pub use super::make_minus_fixlet;
    pub use super::make_minus_function_minus_application;
    pub use super::make_minus_import;
    pub use super::make_minus_import_minus_only;
    pub use super::make_minus_library;
    pub use super::make_minus_nop;
    pub use super::make_minus_null_minus_arg;
    pub use super::make_minus_program;
    pub use super::make_minus_reference;
    pub use super::make_minus_sequence;
    pub use super::make_minus_testcase;
    pub use super::make_minus_testsuite;
    pub use super::make_minus_vararg_minus_abstraction;
    pub use super::procedure_minus_node_p;
}

pub fn __make_minus_library(args: &[Scm]) -> Scm {
    {if args.len() != 7{panic!("invalid arity")}let name__36 = args[0].clone();let globals__2 = args[1].clone();let init__5 = args[2].clone();let body__7 = args[3].clone();let imports__2 = args[4].clone();let exports__1 = args[5].clone();let testsuite__0 = args[6].clone();{
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-exports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (set! gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))) self))
{let [repr__17, transform__17, gen_minus_exports__0, gen_minus_rust__17, self__27, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__27 = self__27.into_boxed();{let gen_minus_rust__17 = gen_minus_rust__17.into_boxed();{let gen_minus_exports__0 = gen_minus_exports__0.into_boxed();{let transform__17 = transform__17.into_boxed();{let repr__17 = repr__17.into_boxed();{repr__17.set({// Closure
let name__36 = name__36.clone();let exports__1 = exports__1.clone();let imports__2 = imports__2.clone();let globals__2 = globals__2.clone();let body__7 = body__7.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
imports::append(&[Scm::symbol("LIBRARY"),name__36.clone(),exports__1.clone(),imports__2.clone(),globals__2.clone(),{
// (body (quote repr))
body__7.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__17.set({// Closure
let self__27 = self__27.clone();let name__36 = name__36.clone();let globals__2 = globals__2.clone();let init__5 = init__5.clone();let body__7 = body__7.clone();let imports__2 = imports__2.clone();let exports__1 = exports__1.clone();let testsuite__0 = testsuite__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__19 = args[0].clone();{
// (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))))
func__19.clone().invoke(&[self__27.get(),{// Closure
let name__36 = name__36.clone();let globals__2 = globals__2.clone();let init__5 = init__5.clone();let body__7 = body__7.clone();let func__19 = func__19.clone();let imports__2 = imports__2.clone();let exports__1 = exports__1.clone();let testsuite__0 = testsuite__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))
Scm::func(__make_minus_library).invoke(&[name__36.clone(),globals__2.clone(),init__5.clone(),{
// (body (quote transform) func)
body__7.clone().invoke(&[Scm::symbol("transform"),func__19.clone()])},imports__2.clone(),exports__1.clone(),{
// (map (lambda (t) (t (quote transform) func)) testsuite)
imports::map(&[{// Closure
let func__19 = func__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let t__13 = args[0].clone();{
// (t (quote transform) func)
t__13.clone().invoke(&[Scm::symbol("transform"),func__19.clone()])}})},testsuite__0.clone()])}])}})}])}})});Scm::anything();gen_minus_exports__0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__34 = args[0].clone();let exports__2 = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each(&[{// Closure
let module__34 = module__34.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo__0 = args[0].clone();{
// (expo (quote gen-rust) module)
expo__0.clone().invoke(&[Scm::symbol("gen-rust"),module__34.clone()])}})},exports__2.clone()])}})});Scm::anything();gen_minus_rust__17.set({// Closure
let imports__2 = imports__2.clone();let gen_minus_exports__0 = gen_minus_exports__0.clone();let exports__1 = exports__1.clone();let globals__2 = globals__2.clone();let body__7 = body__7.clone();let init__5 = init__5.clone();let testsuite__0 = testsuite__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__35 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println(&[module__35.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
imports::print(&[module__35.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block(&[module__35.clone(),{// Closure
let module__35 = module__35.clone();let imports__2 = imports__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module__35 = module__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i__1 = args[0].clone();{
// (i (quote gen-rust) module)
i__1.clone().invoke(&[Scm::symbol("gen-rust"),module__35.clone()])}})},imports__2.clone()])}})}])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (print module "pub mod exports")
imports::print(&[module__35.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block(&[module__35.clone(),{// Closure
let gen_minus_exports__0 = gen_minus_exports__0.clone();let module__35 = module__35.clone();let exports__1 = exports__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports__0.get().invoke(&[module__35.clone(),exports__1.clone()])}})}])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module__35.clone(),globals__2.clone()])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (println module)
imports::println(&[module__35.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p(&[Scm::symbol("NOP"),{
// (body (quote kind))
body__7.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
imports::println(&[module__35.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println(&[module__35.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
imports::println(&[module__35.clone()])};{
// (println module "pub fn initialize() {")
imports::println(&[module__35.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println(&[module__35.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println(&[module__35.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
imports::println(&[module__35.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each(&[{// Closure
let module__35 = module__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib__3 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module__35.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each(&[{// Closure
let module__35 = module__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l__1 = args[0].clone();{
// (print module (rustify-libname l) "::")
imports::print(&[module__35.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l__1.clone()])},Scm::from("::")])}})},lib__3.clone()])};{
// (println module "initialize();")
imports::println(&[module__35.clone(),Scm::from("initialize();")])}}})},init__5.clone()])};{
// (body (quote gen-rust) module)
body__7.clone().invoke(&[Scm::symbol("gen-rust"),module__35.clone()])};{
// (println module ";}")
imports::println(&[module__35.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) testsuite)
imports::for_minus_each(&[{// Closure
let module__35 = module__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test__0 = args[0].clone();{
// (test (quote gen-rust) module)
test__0.clone().invoke(&[Scm::symbol("gen-rust"),module__35.clone()])}})},testsuite__0.clone()])}}})});Scm::anything();self__27.set({// Closure
let repr__17 = repr__17.clone();let transform__17 = transform__17.clone();let name__36 = name__36.clone();let gen_minus_rust__17 = gen_minus_rust__17.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__17 = args[0].clone();let args__28 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__17.clone()])}).is_true() {{
// (repr)
repr__17.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__17.clone()])}).is_true() {{
// (transform (car args))
transform__17.get().invoke(&[{
// (car args)
imports::car(&[args__28.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__17.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
imports::eq_p(&[Scm::symbol("libname"),msg__17.clone()])}).is_true() {name__36.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__17.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__17.get().invoke(&[{
// (car args)
imports::car(&[args__28.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
imports::error(&[Scm::from("Unknown message LIBRARY"),msg__17.clone()])}}}})});Scm::anything();self__27.get()}}}}}}}}}}.into()
}
pub fn ast_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__15 = args[0].clone();
        {
            // (procedure? obj)
            imports::procedure_p(&[obj__15.clone()])
        }
    }
    .into()
}
pub fn free_minus_var_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__39 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (symbol? name)
                imports::symbol_p(&[name__39.clone()])
            })
            .is_true()
            {
                name__39.clone()
            } else if ({
                // (string? name)
                imports::string_p(&[name__39.clone()])
            })
            .is_true()
            {
                {
                    // (string->symbol name)
                    imports::string_minus__g_symbol(&[name__39.clone()])
                }
            } else {
                {
                    // (error "Invalid variable name" name)
                    imports::error(&[Scm::from("Invalid variable name"), name__39.clone()])
                }
            }
        }
    }
    .into()
}
pub fn make_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let vars__1 = args[0].clone();let body__3 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))) self))
{let [repr__14, transform__14, free_minus_vars__15, gen_minus_rust__14, self__24, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__24 = self__24.into_boxed();{let gen_minus_rust__14 = gen_minus_rust__14.into_boxed();{let free_minus_vars__15 = free_minus_vars__15.into_boxed();{let transform__14 = transform__14.into_boxed();{let repr__14 = repr__14.into_boxed();{repr__14.set({// Closure
let vars__1 = vars__1.clone();let body__3 = body__3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ABSTRACTION) vars (body (quote repr)))
imports::list(&[Scm::symbol("ABSTRACTION"),vars__1.clone(),{
// (body (quote repr))
body__3.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__14.set({// Closure
let self__24 = self__24.clone();let vars__1 = vars__1.clone();let body__3 = body__3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__16 = args[0].clone();{
// (func self (lambda () (make-abstraction vars (body (quote transform) func))))
func__16.clone().invoke(&[self__24.get(),{// Closure
let vars__1 = vars__1.clone();let body__3 = body__3.clone();let func__16 = func__16.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-abstraction vars (body (quote transform) func))
Scm::func(make_minus_abstraction).invoke(&[vars__1.clone(),{
// (body (quote transform) func)
body__3.clone().invoke(&[Scm::symbol("transform"),func__16.clone()])}])}})}])}})});Scm::anything();free_minus_vars__15.set({// Closure
let body__3 = body__3.clone();let vars__1 = vars__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__3.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__1.clone()])}])}])}})});Scm::anything();gen_minus_rust__14.set({// Closure
let vars__1 = vars__1.clone();let body__3 = body__3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__30 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params__1 = Scm::symbol("*uninitialized*");{let gen_minus_params__1 = gen_minus_params__1.into_boxed();{gen_minus_params__1.set({// Closure
let module__30 = module__30.clone();let gen_minus_params__1 = gen_minus_params__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star___0 = args[0].clone();let k__0 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star___0.clone()])}).is_true() {{{
// (print module "let ")
imports::print(&[module__30.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name (car p*))))
imports::print(&[module__30.clone(),{
// (rustify-identifier (variable-name (car p*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star___0.clone()])}])}])}])};{
// (print module " = args[")
imports::print(&[module__30.clone(),Scm::from(" = args[")])};{
// (print module k)
imports::print(&[module__30.clone(),k__0.clone()])};{
// (print module "].clone();")
imports::print(&[module__30.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params__1.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star___0.clone()])},{
// (+ k 1)
imports::_plus_(&[k__0.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__30.clone(),{// Closure
let module__30 = module__30.clone();let vars__1 = vars__1.clone();let gen_minus_params__1 = gen_minus_params__1.clone();let body__3 = body__3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
imports::print(&[module__30.clone(),Scm::from("if args.len() != ")])};{
// (print module (length vars))
imports::print(&[module__30.clone(),{
// (length vars)
imports::length(&[vars__1.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
imports::print(&[module__30.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params vars 0)
gen_minus_params__1.get().invoke(&[vars__1.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body__3.clone().invoke(&[Scm::symbol("gen-rust"),module__30.clone()])}}})}])}}}}}}})});Scm::anything();self__24.set({// Closure
let repr__14 = repr__14.clone();let transform__14 = transform__14.clone();let free_minus_vars__15 = free_minus_vars__15.clone();let gen_minus_rust__14 = gen_minus_rust__14.clone();let vars__1 = vars__1.clone();let body__3 = body__3.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__14 = args[0].clone();let args__25 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__14.clone()])}).is_true() {{
// (repr)
repr__14.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__14.clone()])}).is_true() {{
// (transform (car args))
transform__14.get().invoke(&[{
// (car args)
imports::car(&[args__25.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__14.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__15.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__14.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__14.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__14.get().invoke(&[{
// (car args)
imports::car(&[args__25.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__14.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__1.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__14.clone()])}).is_true() {vars__1.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__14.clone()])}).is_true() {body__3.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message ABSTRACTION"),msg__14.clone()])}}}})});Scm::anything();self__24.get()}}}}}}}}}}.into()
}
pub fn make_minus_alternative(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let condition__0 = args[0].clone();let consequent__0 = args[1].clone();let alternative__0 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr__6, transform__6, free_minus_vars__6, gen_minus_rust__6, self__16, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__16 = self__16.into_boxed();{let gen_minus_rust__6 = gen_minus_rust__6.into_boxed();{let free_minus_vars__6 = free_minus_vars__6.into_boxed();{let transform__6 = transform__6.into_boxed();{let repr__6 = repr__6.into_boxed();{repr__6.set({// Closure
let condition__0 = condition__0.clone();let consequent__0 = consequent__0.clone();let alternative__0 = alternative__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list(&[Scm::symbol("IF"),{
// (condition (quote repr))
condition__0.clone().invoke(&[Scm::symbol("repr")])},{
// (consequent (quote repr))
consequent__0.clone().invoke(&[Scm::symbol("repr")])},{
// (alternative (quote repr))
alternative__0.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__6.set({// Closure
let self__16 = self__16.clone();let condition__0 = condition__0.clone();let consequent__0 = consequent__0.clone();let alternative__0 = alternative__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__12 = args[0].clone();{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func__12.clone().invoke(&[self__16.get(),{// Closure
let condition__0 = condition__0.clone();let func__12 = func__12.clone();let consequent__0 = consequent__0.clone();let alternative__0 = alternative__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
Scm::func(make_minus_alternative).invoke(&[{
// (condition (quote transform) func)
condition__0.clone().invoke(&[Scm::symbol("transform"),func__12.clone()])},{
// (consequent (quote transform) func)
consequent__0.clone().invoke(&[Scm::symbol("transform"),func__12.clone()])},{
// (alternative (quote transform) func)
alternative__0.clone().invoke(&[Scm::symbol("transform"),func__12.clone()])}])}})}])}})});Scm::anything();free_minus_vars__6.set({// Closure
let condition__0 = condition__0.clone();let consequent__0 = consequent__0.clone();let alternative__0 = alternative__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
imports::set_minus_union(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
imports::set_minus_union(&[{
// (condition (quote free-vars))
condition__0.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent__0.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative__0.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__6.set({// Closure
let condition__0 = condition__0.clone();let consequent__0 = consequent__0.clone();let alternative__0 = alternative__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__19 = args[0].clone();{{
// (print module "if (")
imports::print(&[module__19.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition__0.clone().invoke(&[Scm::symbol("gen-rust"),module__19.clone()])};{
// (print module ").is_true() {")
imports::print(&[module__19.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent__0.clone().invoke(&[Scm::symbol("gen-rust"),module__19.clone()])};{
// (print module "} else ")
imports::print(&[module__19.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
imports::eq_p(&[{
// (alternative (quote kind))
alternative__0.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative__0.clone().invoke(&[Scm::symbol("gen-rust"),module__19.clone()])}} else {{{
// (print module "{")
imports::print(&[module__19.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative__0.clone().invoke(&[Scm::symbol("gen-rust"),module__19.clone()])};{
// (print module "}")
imports::print(&[module__19.clone(),Scm::from("}")])}}}}})});Scm::anything();self__16.set({// Closure
let repr__6 = repr__6.clone();let transform__6 = transform__6.clone();let free_minus_vars__6 = free_minus_vars__6.clone();let gen_minus_rust__6 = gen_minus_rust__6.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__6 = args[0].clone();let args__15 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__6.clone()])}).is_true() {{
// (repr)
repr__6.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__6.clone()])}).is_true() {{
// (transform (car args))
transform__6.get().invoke(&[{
// (car args)
imports::car(&[args__15.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__6.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__6.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__6.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__6.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__6.get().invoke(&[{
// (car args)
imports::car(&[args__15.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
imports::error(&[Scm::from("Unknown message ALTERNATIVE"),msg__6.clone()])}}}})});Scm::anything();self__16.get()}}}}}}}}}}.into()
}
pub fn make_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let func__14 = args[0].clone();let args__17 = args[1].clone();let tail_p__0 = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr__8, transform__8, free_minus_vars__8, gen_minus_rust__8, self__18, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__18 = self__18.into_boxed();{let gen_minus_rust__8 = gen_minus_rust__8.into_boxed();{let free_minus_vars__8 = free_minus_vars__8.into_boxed();{let transform__8 = transform__8.into_boxed();{let repr__8 = repr__8.into_boxed();{repr__8.set({// Closure
let tail_p__0 = tail_p__0.clone();let func__14 = func__14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons(&[if (tail_p__0.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
imports::cons(&[{
// (func (quote repr))
func__14.clone().invoke(&[Scm::symbol("repr")])},{
// (args (quote repr))
args__17.clone().invoke(&[Scm::symbol("repr")])}])}])}})});Scm::anything();transform__8.set({// Closure
let self__18 = self__18.clone();let func__14 = func__14.clone();let args__17 = args__17.clone();let tail_p__0 = tail_p__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__0 = args[0].clone();{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc__0.clone().invoke(&[self__18.get(),{// Closure
let func__14 = func__14.clone();let fnc__0 = fnc__0.clone();let args__17 = args__17.clone();let tail_p__0 = tail_p__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
Scm::func(make_minus_application).invoke(&[{
// (func (quote transform) fnc)
func__14.clone().invoke(&[Scm::symbol("transform"),fnc__0.clone()])},{
// (args (quote transform) fnc)
args__17.clone().invoke(&[Scm::symbol("transform"),fnc__0.clone()])},tail_p__0.clone()])}})}])}})});Scm::anything();free_minus_vars__8.set({// Closure
let func__14 = func__14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
imports::set_minus_union(&[{
// (func (quote free-vars))
func__14.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args__17.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__8.set({// Closure
let func__14 = func__14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__22 = args[0].clone();{{
// (func (quote gen-rust) module)
func__14.clone().invoke(&[Scm::symbol("gen-rust"),module__22.clone()])};{
// (print module ".invoke(&[")
imports::print(&[module__22.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args__17.clone().invoke(&[Scm::symbol("gen-rust"),module__22.clone()])};{
// (print module "])")
imports::print(&[module__22.clone(),Scm::from("])")])}}})});Scm::anything();self__18.set({// Closure
let repr__8 = repr__8.clone();let transform__8 = transform__8.clone();let free_minus_vars__8 = free_minus_vars__8.clone();let gen_minus_rust__8 = gen_minus_rust__8.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__8 = args[0].clone();let args__18 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__8.clone()])}).is_true() {{
// (repr)
repr__8.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__8.clone()])}).is_true() {{
// (transform (car args))
transform__8.get().invoke(&[{
// (car args)
imports::car(&[args__18.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__8.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__8.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__8.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__8.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__8.get().invoke(&[{
// (car args)
imports::car(&[args__18.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
imports::error(&[Scm::from("Unknown message APPLICATION"),msg__8.clone()])}}}})});Scm::anything();self__18.get()}}}}}}}}}}.into()
}
pub fn make_minus_args(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let arg__0 = args[0].clone();let next__1 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote ARG) (cons arg next)))) (set! transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (set! gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))) self))
{let [repr__11, transform__11, free_minus_vars__11, gen_minus_rust__11, self__21, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__21 = self__21.into_boxed();{let gen_minus_rust__11 = gen_minus_rust__11.into_boxed();{let free_minus_vars__11 = free_minus_vars__11.into_boxed();{let transform__11 = transform__11.into_boxed();{let repr__11 = repr__11.into_boxed();{repr__11.set({// Closure
let arg__0 = arg__0.clone();let next__1 = next__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote ARG) (cons arg next))
imports::cons(&[Scm::symbol("ARG"),{
// (cons arg next)
imports::cons(&[arg__0.clone(),next__1.clone()])}])}})});Scm::anything();transform__11.set({// Closure
let self__21 = self__21.clone();let arg__0 = arg__0.clone();let next__1 = next__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__3 = args[0].clone();{
// (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
fnc__3.clone().invoke(&[self__21.get(),{// Closure
let arg__0 = arg__0.clone();let fnc__3 = fnc__3.clone();let next__1 = next__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
Scm::func(make_minus_args).invoke(&[{
// (arg (quote transform) fnc)
arg__0.clone().invoke(&[Scm::symbol("transform"),fnc__3.clone()])},{
// (next (quote transform) fnc)
next__1.clone().invoke(&[Scm::symbol("transform"),fnc__3.clone()])}])}})}])}})});Scm::anything();free_minus_vars__11.set({// Closure
let arg__0 = arg__0.clone();let next__1 = next__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (arg (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (arg (quote free-vars))
arg__0.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next__1.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__11.set({// Closure
let arg__0 = arg__0.clone();let next__1 = next__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__25 = args[0].clone();{{
// (arg (quote gen-rust) module)
arg__0.clone().invoke(&[Scm::symbol("gen-rust"),module__25.clone()])};if ({
// (not (eq? (quote NULL-ARG) (next (quote kind))))
imports::not(&[{
// (eq? (quote NULL-ARG) (next (quote kind)))
imports::eq_p(&[Scm::symbol("NULL-ARG"),{
// (next (quote kind))
next__1.clone().invoke(&[Scm::symbol("kind")])}])}])}).is_true() {{{
// (print module ",")
imports::print(&[module__25.clone(),Scm::from(",")])};{
// (next (quote gen-rust) module)
next__1.clone().invoke(&[Scm::symbol("gen-rust"),module__25.clone()])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});Scm::anything();self__21.set({// Closure
let repr__11 = repr__11.clone();let transform__11 = transform__11.clone();let free_minus_vars__11 = free_minus_vars__11.clone();let gen_minus_rust__11 = gen_minus_rust__11.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__11 = args[0].clone();let args__22 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__11.clone()])}).is_true() {{
// (repr)
repr__11.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__11.clone()])}).is_true() {{
// (transform (car args))
transform__11.get().invoke(&[{
// (car args)
imports::car(&[args__22.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__11.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__11.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__11.clone()])}).is_true() {Scm::symbol("ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__11.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__11.get().invoke(&[{
// (car args)
imports::car(&[args__22.clone()])}])}} else {{
// (error "Unknown message ARG" msg)
imports::error(&[Scm::from("Unknown message ARG"),msg__11.clone()])}}}})});Scm::anything();self__21.get()}}}}}}}}}}.into()
}
pub fn make_minus_assert(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let condition__1 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ASSERT) condition))) (set! transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (set! free-vars (lambda () (condition (quote free-vars)))) (set! gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))) self))
{let [repr__24, transform__24, free_minus_vars__22, gen_minus_rust__24, self__34, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__34 = self__34.into_boxed();{let gen_minus_rust__24 = gen_minus_rust__24.into_boxed();{let free_minus_vars__22 = free_minus_vars__22.into_boxed();{let transform__24 = transform__24.into_boxed();{let repr__24 = repr__24.into_boxed();{repr__24.set({// Closure
let condition__1 = condition__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ASSERT) condition)
imports::list(&[Scm::symbol("ASSERT"),condition__1.clone()])}})});Scm::anything();transform__24.set({// Closure
let self__34 = self__34.clone();let condition__1 = condition__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__26 = args[0].clone();{
// (func self (lambda () (make-assert (condition (quote transform) func))))
func__26.clone().invoke(&[self__34.get(),{// Closure
let condition__1 = condition__1.clone();let func__26 = func__26.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assert (condition (quote transform) func))
Scm::func(make_minus_assert).invoke(&[{
// (condition (quote transform) func)
condition__1.clone().invoke(&[Scm::symbol("transform"),func__26.clone()])}])}})}])}})});Scm::anything();free_minus_vars__22.set({// Closure
let condition__1 = condition__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (condition (quote free-vars))
condition__1.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__24.set({// Closure
let condition__1 = condition__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__45 = args[0].clone();{{
// (print module "assert!(")
imports::print(&[module__45.clone(),Scm::from("assert!(")])};{
// (condition (quote gen-rust) module)
condition__1.clone().invoke(&[Scm::symbol("gen-rust"),module__45.clone()])};{
// (println module ".is_true());")
imports::println(&[module__45.clone(),Scm::from(".is_true());")])}}})});Scm::anything();self__34.set({// Closure
let repr__24 = repr__24.clone();let transform__24 = transform__24.clone();let free_minus_vars__22 = free_minus_vars__22.clone();let gen_minus_rust__24 = gen_minus_rust__24.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__24 = args[0].clone();let args__35 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__24.clone()])}).is_true() {{
// (repr)
repr__24.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__24.clone()])}).is_true() {{
// (transform (car args))
transform__24.get().invoke(&[{
// (car args)
imports::car(&[args__35.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__24.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__22.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__24.clone()])}).is_true() {Scm::symbol("ASSERT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__24.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__24.get().invoke(&[{
// (car args)
imports::car(&[args__35.clone()])}])}} else {{
// (error "Unknown message ASSERT" msg)
imports::error(&[Scm::from("Unknown message ASSERT"),msg__24.clone()])}}}})});Scm::anything();self__34.get()}}}}}}}}}}.into()
}
pub fn make_minus_assignment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__16 = args[0].clone();let val__4 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr__4, transform__4, free_minus_vars__4, gen_minus_rust__4, self__14, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__14 = self__14.into_boxed();{let gen_minus_rust__4 = gen_minus_rust__4.into_boxed();{let free_minus_vars__4 = free_minus_vars__4.into_boxed();{let transform__4 = transform__4.into_boxed();{let repr__4 = repr__4.into_boxed();{repr__4.set({// Closure
let var__16 = var__16.clone();let val__4 = val__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("SET!"),{
// (variable-name var)
imports::variable_minus_name(&[var__16.clone()])},{
// (val (quote repr))
val__4.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__4.set({// Closure
let self__14 = self__14.clone();let var__16 = var__16.clone();let val__4 = val__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__10 = args[0].clone();{
// (func self (lambda () (make-assignment var (val (quote transform) func))))
func__10.clone().invoke(&[self__14.get(),{// Closure
let var__16 = var__16.clone();let val__4 = val__4.clone();let func__10 = func__10.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment var (val (quote transform) func))
Scm::func(make_minus_assignment).invoke(&[var__16.clone(),{
// (val (quote transform) func)
val__4.clone().invoke(&[Scm::symbol("transform"),func__10.clone()])}])}})}])}})});Scm::anything();free_minus_vars__4.set({// Closure
let var__16 = var__16.clone();let val__4 = val__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var__16.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var__16.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var__16.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val__4.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__16.clone()])}])}])}}})});Scm::anything();gen_minus_rust__4.set({// Closure
let var__16 = var__16.clone();let val__4 = val__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__17 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var))))
{let name__32 = {
// (variable-name var)
imports::variable_minus_name(&[var__16.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__16.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__17.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__32.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__4.clone().invoke(&[Scm::symbol("gen-rust"),module__17.clone()])};{
// (print module "));")
imports::print(&[module__17.clone(),Scm::from("));")])}}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var__16.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".set(")
imports::print(&[module__17.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__32.clone()])},Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val__4.clone().invoke(&[Scm::symbol("gen-rust"),module__17.clone()])};{
// (print module ");")
imports::print(&[module__17.clone(),Scm::from(");")])}}} else {{
// (error "set! on unboxed variable" name var)
imports::error(&[Scm::from("set! on unboxed variable"),name__32.clone(),var__16.clone()])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module__17.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__14.set({// Closure
let repr__4 = repr__4.clone();let transform__4 = transform__4.clone();let free_minus_vars__4 = free_minus_vars__4.clone();let gen_minus_rust__4 = gen_minus_rust__4.clone();let var__16 = var__16.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__4 = args[0].clone();let args__13 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__4.clone()])}).is_true() {{
// (repr)
repr__4.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__4.clone()])}).is_true() {{
// (transform (car args))
transform__4.get().invoke(&[{
// (car args)
imports::car(&[args__13.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__4.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__4.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__4.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__4.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__4.get().invoke(&[{
// (car args)
imports::car(&[args__13.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__4.clone()])}).is_true() {var__16.clone()} else {{
// (error "Unknown message ASSIGNMENT" msg)
imports::error(&[Scm::from("Unknown message ASSIGNMENT"),msg__4.clone()])}}}})});Scm::anything();self__14.get()}}}}}}}}}}.into()
}
pub fn make_minus_boxify(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__19 = args[0].clone();let body__8 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote BOXIFY) var (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-boxify var (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier (variable-name var))) (print module " = ") (print module (rustify-identifier (variable-name var))) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote BOXIFY) var (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-boxify var (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier (variable-name var))) (print module " = ") (print module (rustify-identifier (variable-name var))) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr__18, transform__18, free_minus_vars__17, gen_minus_rust__18, self__28, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__28 = self__28.into_boxed();{let gen_minus_rust__18 = gen_minus_rust__18.into_boxed();{let free_minus_vars__17 = free_minus_vars__17.into_boxed();{let transform__18 = transform__18.into_boxed();{let repr__18 = repr__18.into_boxed();{repr__18.set({// Closure
let var__19 = var__19.clone();let body__8 = body__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote BOXIFY) var (body (quote repr)))
imports::list(&[Scm::symbol("BOXIFY"),var__19.clone(),{
// (body (quote repr))
body__8.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__18.set({// Closure
let self__28 = self__28.clone();let var__19 = var__19.clone();let body__8 = body__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__20 = args[0].clone();{
// (func self (lambda () (make-boxify var (body (quote transform) func))))
func__20.clone().invoke(&[self__28.get(),{// Closure
let var__19 = var__19.clone();let body__8 = body__8.clone();let func__20 = func__20.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-boxify var (body (quote transform) func))
Scm::func(make_minus_boxify).invoke(&[var__19.clone(),{
// (body (quote transform) func)
body__8.clone().invoke(&[Scm::symbol("transform"),func__20.clone()])}])}})}])}})});Scm::anything();free_minus_vars__17.set({// Closure
let body__8 = body__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (body (quote free-vars))
body__8.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__18.set({// Closure
let var__19 = var__19.clone();let body__8 = body__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__36 = args[0].clone();{
// (rust-block module (lambda () (print module "let ") (print module (rustify-identifier (variable-name var))) (print module " = ") (print module (rustify-identifier (variable-name var))) (print module ".into_boxed();") (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__36.clone(),{// Closure
let module__36 = module__36.clone();let var__19 = var__19.clone();let body__8 = body__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
imports::print(&[module__36.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name var)))
imports::print(&[module__36.clone(),{
// (rustify-identifier (variable-name var))
imports::rustify_minus_identifier(&[{
// (variable-name var)
imports::variable_minus_name(&[var__19.clone()])}])}])};{
// (print module " = ")
imports::print(&[module__36.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier (variable-name var)))
imports::print(&[module__36.clone(),{
// (rustify-identifier (variable-name var))
imports::rustify_minus_identifier(&[{
// (variable-name var)
imports::variable_minus_name(&[var__19.clone()])}])}])};{
// (print module ".into_boxed();")
imports::print(&[module__36.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body__8.clone().invoke(&[Scm::symbol("gen-rust"),module__36.clone()])}}})}])}})});Scm::anything();self__28.set({// Closure
let repr__18 = repr__18.clone();let transform__18 = transform__18.clone();let free_minus_vars__17 = free_minus_vars__17.clone();let gen_minus_rust__18 = gen_minus_rust__18.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__18 = args[0].clone();let args__29 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__18.clone()])}).is_true() {{
// (repr)
repr__18.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__18.clone()])}).is_true() {{
// (transform (car args))
transform__18.get().invoke(&[{
// (car args)
imports::car(&[args__29.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__18.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__17.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__18.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__18.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__18.get().invoke(&[{
// (car args)
imports::car(&[args__29.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
imports::error(&[Scm::from("Unknown message BOXIFY"),msg__18.clone()])}}}})});Scm::anything();self__28.get()}}}}}}}}}}.into()
}
pub fn make_minus_closure(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let function__0 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr__13, transform__13, free_minus_vars__13, prepare_minus_closure__0, gen_minus_rust__13, self__23, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__23 = self__23.into_boxed();{let gen_minus_rust__13 = gen_minus_rust__13.into_boxed();{let prepare_minus_closure__0 = prepare_minus_closure__0.into_boxed();{let free_minus_vars__13 = free_minus_vars__13.into_boxed();{let transform__13 = transform__13.into_boxed();{let repr__13 = repr__13.into_boxed();{repr__13.set({// Closure
let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
imports::list(&[Scm::symbol("CLOSURE"),function__0.clone()])}})});Scm::anything();transform__13.set({// Closure
let self__23 = self__23.clone();let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__15 = args[0].clone();{
// (func self (lambda () (make-closure (function (quote transform) func))))
func__15.clone().invoke(&[self__23.get(),{// Closure
let function__0 = function__0.clone();let func__15 = func__15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-closure (function (quote transform) func))
Scm::func(make_minus_closure).invoke(&[{
// (function (quote transform) func)
function__0.clone().invoke(&[Scm::symbol("transform"),func__15.clone()])}])}})}])}})});Scm::anything();free_minus_vars__13.set({// Closure
let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (function (quote free-vars))
function__0.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();prepare_minus_closure__0.set({// Closure
let prepare_minus_closure__0 = prepare_minus_closure__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__28 = args[0].clone();let free_minus_vars__14 = args[1].clone();if ({
// (pair? free-vars)
imports::pair_p(&[free_minus_vars__14.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name__34 = {
// (car free-vars)
imports::car(&[free_minus_vars__14.clone()])};{{
// (print module "let ")
imports::print(&[module__28.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__28.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__34.clone()])}])};{
// (print module " = ")
imports::print(&[module__28.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__28.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__34.clone()])}])};{
// (print module ".clone();")
imports::print(&[module__28.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure__0.get().invoke(&[module__28.clone(),{
// (cdr free-vars)
imports::cdr(&[free_minus_vars__14.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();gen_minus_rust__13.set({// Closure
let prepare_minus_closure__0 = prepare_minus_closure__0.clone();let free_minus_vars__13 = free_minus_vars__13.clone();let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__29 = args[0].clone();{
// (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
imports::rust_minus_block(&[module__29.clone(),{// Closure
let module__29 = module__29.clone();let prepare_minus_closure__0 = prepare_minus_closure__0.clone();let free_minus_vars__13 = free_minus_vars__13.clone();let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "// Closure")
imports::println(&[module__29.clone(),Scm::from("// Closure")])};{
// (prepare-closure module (free-vars))
prepare_minus_closure__0.get().invoke(&[module__29.clone(),{
// (free-vars)
free_minus_vars__13.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
imports::print(&[module__29.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function__0.clone().invoke(&[Scm::symbol("gen-rust"),module__29.clone()])};{
// (print module ")")
imports::print(&[module__29.clone(),Scm::from(")")])}}})}])}})});Scm::anything();self__23.set({// Closure
let repr__13 = repr__13.clone();let transform__13 = transform__13.clone();let free_minus_vars__13 = free_minus_vars__13.clone();let gen_minus_rust__13 = gen_minus_rust__13.clone();let function__0 = function__0.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__13 = args[0].clone();let args__24 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__13.clone()])}).is_true() {{
// (repr)
repr__13.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__13.clone()])}).is_true() {{
// (transform (car args))
transform__13.get().invoke(&[{
// (car args)
imports::car(&[args__24.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__13.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__13.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__13.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__13.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__13.get().invoke(&[{
// (car args)
imports::car(&[args__24.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
imports::eq_p(&[Scm::symbol("inner-function"),msg__13.clone()])}).is_true() {function__0.clone()} else {{
// (error "Unknown message CLOSURE" msg)
imports::error(&[Scm::from("Unknown message CLOSURE"),msg__13.clone()])}}}})});Scm::anything();self__23.get()}}}}}}}}}}}.into()
}
pub fn make_minus_comment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let comment__0 = args[0].clone();let node__8 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr__0, transform__0, free_minus_vars__0, gen_minus_rust__0, gen_minus_rust_minus_inner__0, self__10, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__10 = self__10.into_boxed();{let gen_minus_rust_minus_inner__0 = gen_minus_rust_minus_inner__0.into_boxed();{let gen_minus_rust__0 = gen_minus_rust__0.into_boxed();{let free_minus_vars__0 = free_minus_vars__0.into_boxed();{let transform__0 = transform__0.into_boxed();{let repr__0 = repr__0.into_boxed();{repr__0.set({// Closure
let comment__0 = comment__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
imports::list(&[Scm::symbol("COMMENT"),comment__0.clone(),{
// (node (quote repr))
node__8.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__0.set({// Closure
let self__10 = self__10.clone();let comment__0 = comment__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__6 = args[0].clone();{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func__6.clone().invoke(&[self__10.get(),{// Closure
let comment__0 = comment__0.clone();let node__8 = node__8.clone();let func__6 = func__6.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-comment comment (node (quote transform) func))
Scm::func(make_minus_comment).invoke(&[comment__0.clone(),{
// (node (quote transform) func)
node__8.clone().invoke(&[Scm::symbol("transform"),func__6.clone()])}])}})}])}})});Scm::anything();free_minus_vars__0.set({// Closure
let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (node (quote free-vars))
node__8.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__0.set({// Closure
let comment__0 = comment__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__11 = args[0].clone();{
// (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)))
imports::rust_minus_block(&[module__11.clone(),{// Closure
let module__11 = module__11.clone();let comment__0 = comment__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module__11.clone()])};{
// (print module "// ")
imports::print(&[module__11.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module__11.clone(),comment__0.clone()])};{
// (node (quote gen-rust) module)
node__8.clone().invoke(&[Scm::symbol("gen-rust"),module__11.clone()])}}})}])}})});Scm::anything();gen_minus_rust_minus_inner__0.set({// Closure
let comment__0 = comment__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__12 = args[0].clone();{{
// (println module)
imports::println(&[module__12.clone()])};{
// (print module "// ")
imports::print(&[module__12.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module__12.clone(),comment__0.clone()])};{
// (node (quote gen-rust-inner) module)
node__8.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__12.clone()])}}})});Scm::anything();self__10.set({// Closure
let repr__0 = repr__0.clone();let transform__0 = transform__0.clone();let free_minus_vars__0 = free_minus_vars__0.clone();let gen_minus_rust__0 = gen_minus_rust__0.clone();let gen_minus_rust_minus_inner__0 = gen_minus_rust_minus_inner__0.clone();let node__8 = node__8.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__0 = args[0].clone();let args__9 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__0.clone()])}).is_true() {{
// (repr)
repr__0.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__0.clone()])}).is_true() {{
// (transform (car args))
transform__0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__0.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__0.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__0.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__0.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__0.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner__0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote inner) msg)
imports::eq_p(&[Scm::symbol("inner"),msg__0.clone()])}).is_true() {node__8.clone()} else {{
// (error "Unknown message COMMENT" msg)
imports::error(&[Scm::from("Unknown message COMMENT"),msg__0.clone()])}}}})});Scm::anything();self__10.get()}}}}}}}}}}}.into()
}
pub fn make_minus_constant(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let val__2 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr__2, transform__2, free_minus_vars__2, gen_minus_constant__0, gen_minus_rust__2, self__12, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__12 = self__12.into_boxed();{let gen_minus_rust__2 = gen_minus_rust__2.into_boxed();{let gen_minus_constant__0 = gen_minus_constant__0.into_boxed();{let free_minus_vars__2 = free_minus_vars__2.into_boxed();{let transform__2 = transform__2.into_boxed();{let repr__2 = repr__2.into_boxed();{repr__2.set({// Closure
let val__2 = val__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
imports::cons(&[Scm::symbol("CONSTANT"),val__2.clone()])}})});Scm::anything();transform__2.set({// Closure
let self__12 = self__12.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__8 = args[0].clone();{
// (func self (lambda () self))
func__8.clone().invoke(&[self__12.get(),{// Closure
let self__12 = self__12.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__12.get()})}])}})});Scm::anything();free_minus_vars__2.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_constant__0.set({// Closure
let gen_minus_constant__0 = gen_minus_constant__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__14 = args[0].clone();let val__3 = args[1].clone();{
// (cond ...)
if ({
// (null? val)
imports::null_p(&[val__3.clone()])}).is_true() {{
// (print module "Scm::Nil")
imports::print(&[module__14.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
imports::eq_p(&[val__3.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
imports::print(&[module__14.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
imports::eq_p(&[val__3.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
imports::print(&[module__14.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
imports::symbol_p(&[val__3.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
imports::print(&[module__14.clone(),Scm::from("Scm::symbol(\""),val__3.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
imports::eq_p(&[val__3.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
imports::print(&[module__14.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
imports::char_p(&[val__3.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
imports::print(&[module__14.clone(),Scm::from("Scm::char('"),val__3.clone(),Scm::from("')")])}} else if ({
// (pair? val)
imports::pair_p(&[val__3.clone()])}).is_true() {{{
// (print module "Scm::pair(")
imports::print(&[module__14.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant__0.get().invoke(&[module__14.clone(),{
// (car val)
imports::car(&[val__3.clone()])}])};{
// (print module ", ")
imports::print(&[module__14.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant__0.get().invoke(&[module__14.clone(),{
// (cdr val)
imports::cdr(&[val__3.clone()])}])};{
// (print module ")")
imports::print(&[module__14.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
imports::print(&[module__14.clone(),Scm::from("Scm::from(")])};{
// (show module val)
imports::show(&[module__14.clone(),val__3.clone()])};{
// (print module ")")
imports::print(&[module__14.clone(),Scm::from(")")])}}}}})});Scm::anything();gen_minus_rust__2.set({// Closure
let gen_minus_constant__0 = gen_minus_constant__0.clone();let val__2 = val__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__15 = args[0].clone();{
// (gen-constant module val)
gen_minus_constant__0.get().invoke(&[module__15.clone(),val__2.clone()])}})});Scm::anything();self__12.set({// Closure
let repr__2 = repr__2.clone();let transform__2 = transform__2.clone();let free_minus_vars__2 = free_minus_vars__2.clone();let gen_minus_rust__2 = gen_minus_rust__2.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__2 = args[0].clone();let args__11 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__2.clone()])}).is_true() {{
// (repr)
repr__2.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__2.clone()])}).is_true() {{
// (transform (car args))
transform__2.get().invoke(&[{
// (car args)
imports::car(&[args__11.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__2.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__2.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__2.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__2.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__2.get().invoke(&[{
// (car args)
imports::car(&[args__11.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
imports::error(&[Scm::from("Unknown message CONSTANT"),msg__2.clone()])}}}})});Scm::anything();self__12.get()}}}}}}}}}}}.into()
}
pub fn make_minus_definition(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__17 = args[0].clone();let val__5 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr__5, transform__5, free_minus_vars__5, gen_minus_rust__5, self__15, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__15 = self__15.into_boxed();{let gen_minus_rust__5 = gen_minus_rust__5.into_boxed();{let free_minus_vars__5 = free_minus_vars__5.into_boxed();{let transform__5 = transform__5.into_boxed();{let repr__5 = repr__5.into_boxed();{repr__5.set({// Closure
let var__17 = var__17.clone();let val__5 = val__5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("DEFINE"),{
// (variable-name var)
imports::variable_minus_name(&[var__17.clone()])},{
// (val (quote repr))
val__5.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__5.set({// Closure
let self__15 = self__15.clone();let var__17 = var__17.clone();let val__5 = val__5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__11 = args[0].clone();{
// (func self (lambda () (make-definition var (val (quote transform) func))))
func__11.clone().invoke(&[self__15.get(),{// Closure
let var__17 = var__17.clone();let val__5 = val__5.clone();let func__11 = func__11.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition var (val (quote transform) func))
Scm::func(make_minus_definition).invoke(&[var__17.clone(),{
// (val (quote transform) func)
val__5.clone().invoke(&[Scm::symbol("transform"),func__11.clone()])}])}})}])}})});Scm::anything();free_minus_vars__5.set({// Closure
let val__5 = val__5.clone();let var__17 = var__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val__5.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__17.clone()])}])}])}})});Scm::anything();gen_minus_rust__5.set({// Closure
let var__17 = var__17.clone();let val__5 = val__5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__18 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable"))))
{let name__33 = {
// (variable-name var)
imports::variable_minus_name(&[var__17.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__17.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__18.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__33.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__5.clone().invoke(&[Scm::symbol("gen-rust"),module__18.clone()])};{
// (print module "));")
imports::print(&[module__18.clone(),Scm::from("));")])}}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__17.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__18.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__33.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__5.clone().invoke(&[Scm::symbol("gen-rust"),module__18.clone()])};{
// (print module "));")
imports::print(&[module__18.clone(),Scm::from("));")])}}} else {{
// (error "definition! of non-global variable")
imports::error(&[Scm::from("definition! of non-global variable")])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module__18.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__15.set({// Closure
let repr__5 = repr__5.clone();let transform__5 = transform__5.clone();let free_minus_vars__5 = free_minus_vars__5.clone();let gen_minus_rust__5 = gen_minus_rust__5.clone();let var__17 = var__17.clone();let val__5 = val__5.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__5 = args[0].clone();let args__14 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__5.clone()])}).is_true() {{
// (repr)
repr__5.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__5.clone()])}).is_true() {{
// (transform (car args))
transform__5.get().invoke(&[{
// (car args)
imports::car(&[args__14.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__5.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__5.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__5.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__5.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__5.get().invoke(&[{
// (car args)
imports::car(&[args__14.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg__5.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var__17.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__5.clone()])}).is_true() {var__17.clone()} else if ({
// (eq? (quote get-val) msg)
imports::eq_p(&[Scm::symbol("get-val"),msg__5.clone()])}).is_true() {val__5.clone()} else {{
// (error "Unknown message DEFINITION" msg)
imports::error(&[Scm::from("Unknown message DEFINITION"),msg__5.clone()])}}}})});Scm::anything();self__15.get()}}}}}}}}}}.into()
}
pub fn make_minus_export(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__20 = args[0].clone();let exname__0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote EXPORT) var (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) var (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))) self))
{let [repr__19, transform__19, gen_minus_rust__19, self__29, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__29 = self__29.into_boxed();{let gen_minus_rust__19 = gen_minus_rust__19.into_boxed();{let transform__19 = transform__19.into_boxed();{let repr__19 = repr__19.into_boxed();{repr__19.set({// Closure
let var__20 = var__20.clone();let exname__0 = exname__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote EXPORT) var (quote AS) exname)
imports::list(&[Scm::symbol("EXPORT"),var__20.clone(),Scm::symbol("AS"),exname__0.clone()])}})});Scm::anything();transform__19.set({// Closure
let self__29 = self__29.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__21 = args[0].clone();{
// (func self (lambda () self))
func__21.clone().invoke(&[self__29.get(),{// Closure
let self__29 = self__29.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__29.get()})}])}})});Scm::anything();gen_minus_rust__19.set({// Closure
let var__20 = var__20.clone();let exname__0 = exname__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__37 = args[0].clone();{{
// (print module "pub use super::")
imports::print(&[module__37.clone(),Scm::from("pub use super::")])};{
// (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))
{
// (let ((name (variable-name var))) (begin (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")))
{let name__37 = {
// (variable-name var)
imports::variable_minus_name(&[var__20.clone()])};{{
// (cond ...)
if ({
// (not var)
imports::not(&[var__20.clone()])}).is_true() {{
// (error "undefined export" name)
imports::error(&[Scm::from("undefined export"),name__37.clone()])}} else if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__20.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__37.clone(),Scm::from("")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__20.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__37.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__20.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module__37.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
imports::error(&[Scm::from("invalid export variable"),var__20.clone(),name__37.clone()])}}};{
// (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
imports::println(&[module__37.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__37.clone()])},Scm::from(" as "),{
// (rustify-identifier exname)
imports::rustify_minus_identifier(&[exname__0.clone()])},Scm::from(";")])}}}}}}})});Scm::anything();self__29.set({// Closure
let repr__19 = repr__19.clone();let transform__19 = transform__19.clone();let gen_minus_rust__19 = gen_minus_rust__19.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__19 = args[0].clone();let args__30 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__19.clone()])}).is_true() {{
// (repr)
repr__19.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__19.clone()])}).is_true() {{
// (transform (car args))
transform__19.get().invoke(&[{
// (car args)
imports::car(&[args__30.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__19.clone()])}).is_true() {Scm::symbol("EXPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__19.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__19.get().invoke(&[{
// (car args)
imports::car(&[args__30.clone()])}])}} else {{
// (error "Unknown message EXPORT" msg)
imports::error(&[Scm::from("Unknown message EXPORT"),msg__19.clone()])}}}})});Scm::anything();self__29.get()}}}}}}}}}.into()
}
pub fn make_minus_fixlet(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars__0 = args[0].clone();let args__23 = args[1].clone();let body__2 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr__12, transform__12, free_minus_vars__12, gen_minus_rust_minus_inner__2, gen_minus_rust__12, self__22, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__22 = self__22.into_boxed();{let gen_minus_rust__12 = gen_minus_rust__12.into_boxed();{let gen_minus_rust_minus_inner__2 = gen_minus_rust_minus_inner__2.into_boxed();{let free_minus_vars__12 = free_minus_vars__12.into_boxed();{let transform__12 = transform__12.into_boxed();{let repr__12 = repr__12.into_boxed();{repr__12.set({// Closure
let vars__0 = vars__0.clone();let args__23 = args__23.clone();let body__2 = body__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) vars (args (quote repr)) (body (quote repr)))
imports::list(&[Scm::symbol("FIXLET"),vars__0.clone(),{
// (args (quote repr))
args__23.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body__2.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__12.set({// Closure
let self__22 = self__22.clone();let vars__0 = vars__0.clone();let args__23 = args__23.clone();let body__2 = body__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__4 = args[0].clone();{
// (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc__4.clone().invoke(&[self__22.get(),{// Closure
let vars__0 = vars__0.clone();let args__23 = args__23.clone();let fnc__4 = fnc__4.clone();let body__2 = body__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))
Scm::func(make_minus_fixlet).invoke(&[vars__0.clone(),{
// (args (quote transform) fnc)
args__23.clone().invoke(&[Scm::symbol("transform"),fnc__4.clone()])},{
// (body (quote transform) fnc)
body__2.clone().invoke(&[Scm::symbol("transform"),fnc__4.clone()])}])}})}])}})});Scm::anything();free_minus_vars__12.set({// Closure
let body__2 = body__2.clone();let vars__0 = vars__0.clone();let args__23 = args__23.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars)))
imports::set_minus_union(&[{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__2.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__0.clone()])}])}])},{
// (args (quote free-vars))
args__23.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner__2.set({// Closure
let vars__0 = vars__0.clone();let args__23 = args__23.clone();let body__2 = body__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__26 = args[0].clone();{
// (letrec ((gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*)))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params__0 = Scm::symbol("*uninitialized*");{let gen_minus_params__0 = gen_minus_params__0.into_boxed();{gen_minus_params__0.set({// Closure
let module__26 = module__26.clone();let gen_minus_params__0 = gen_minus_params__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let v_star___0 = args[0].clone();if ({
// (pair? v*)
imports::pair_p(&[v_star___0.clone()])}).is_true() {{{
// (print module (rustify-identifier (variable-name (car v*))) ", ")
imports::print(&[module__26.clone(),{
// (rustify-identifier (variable-name (car v*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car v*))
imports::variable_minus_name(&[{
// (car v*)
imports::car(&[v_star___0.clone()])}])}])},Scm::from(", ")])};{
// (gen-params (cdr v*))
gen_minus_params__0.get().invoke(&[{
// (cdr v*)
imports::cdr(&[v_star___0.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (cond ...)
if ({
// (= 0 (length vars))
imports::_e_(&[Scm::from(0),{
// (length vars)
imports::length(&[vars__0.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length vars))
imports::_e_(&[Scm::from(1),{
// (length vars)
imports::length(&[vars__0.clone()])}])}).is_true() {{{
// (print module "let ")
imports::print(&[module__26.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name (car vars))))
imports::print(&[module__26.clone(),{
// (rustify-identifier (variable-name (car vars)))
imports::rustify_minus_identifier(&[{
// (variable-name (car vars))
imports::variable_minus_name(&[{
// (car vars)
imports::car(&[vars__0.clone()])}])}])}])};{
// (print module " = ")
imports::print(&[module__26.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args__23.clone().invoke(&[Scm::symbol("gen-rust"),module__26.clone()])};{
// (print module ";")
imports::print(&[module__26.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
imports::print(&[module__26.clone(),Scm::from("let [")])};{
// (gen-params vars)
gen_minus_params__0.get().invoke(&[vars__0.clone()])};{
// (print module "] = [")
imports::print(&[module__26.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args__23.clone().invoke(&[Scm::symbol("gen-rust"),module__26.clone()])};{
// (print module "];")
imports::print(&[module__26.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body__2.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body__2.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__26.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
imports::eq_p(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body__2.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// ((body (quote inner)) (quote kind))
{
// (body (quote inner))
body__2.clone().invoke(&[Scm::symbol("inner")])}.invoke(&[Scm::symbol("kind")])}])}} else {Scm::False}}).is_true() {{
// (body (quote gen-rust-inner) module)
body__2.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__26.clone()])}} else {{
// (body (quote gen-rust) module)
body__2.clone().invoke(&[Scm::symbol("gen-rust"),module__26.clone()])}}}}}}}})});Scm::anything();gen_minus_rust__12.set({// Closure
let gen_minus_rust_minus_inner__2 = gen_minus_rust_minus_inner__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__27 = args[0].clone();{
// (rust-block module (lambda () (gen-rust-inner module)))
imports::rust_minus_block(&[module__27.clone(),{// Closure
let gen_minus_rust_minus_inner__2 = gen_minus_rust_minus_inner__2.clone();let module__27 = module__27.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner__2.get().invoke(&[module__27.clone()])}})}])}})});Scm::anything();self__22.set({// Closure
let repr__12 = repr__12.clone();let transform__12 = transform__12.clone();let free_minus_vars__12 = free_minus_vars__12.clone();let vars__0 = vars__0.clone();let args__23 = args__23.clone();let body__2 = body__2.clone();let gen_minus_rust__12 = gen_minus_rust__12.clone();let gen_minus_rust_minus_inner__2 = gen_minus_rust_minus_inner__2.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__12 = args[0].clone();let arg_star___0 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__12.clone()])}).is_true() {{
// (repr)
repr__12.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__12.clone()])}).is_true() {{
// (transform (car arg*))
transform__12.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___0.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__12.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__12.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__12.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__12.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__0.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__12.clone()])}).is_true() {vars__0.clone()} else if ({
// (eq? (quote get-args) msg)
imports::eq_p(&[Scm::symbol("get-args"),msg__12.clone()])}).is_true() {args__23.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__12.clone()])}).is_true() {body__2.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__12.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust__12.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___0.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__12.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner__2.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___0.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
imports::error(&[Scm::from("Unknown message FIXLET"),msg__12.clone()])}}}})});Scm::anything();self__22.get()}}}}}}}}}}}.into()
}
pub fn make_minus_function_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let var__18 = args[0].clone();let args__19 = args[1].clone();let tail_p__1 = args[2].clone();{
// (letrec ((repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (free-vars (lambda () (args (quote free-vars)))) (gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message FN-APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (args (quote free-vars)))) (set! gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message FN-APPLICATION" msg))))) self))
{let [repr__9, transform__9, free_minus_vars__9, gen_minus_rust__9, self__19, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__19 = self__19.into_boxed();{let gen_minus_rust__9 = gen_minus_rust__9.into_boxed();{let free_minus_vars__9 = free_minus_vars__9.into_boxed();{let transform__9 = transform__9.into_boxed();{let repr__9 = repr__9.into_boxed();{repr__9.set({// Closure
let tail_p__1 = tail_p__1.clone();let var__18 = var__18.clone();let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr)))
imports::list(&[if (tail_p__1.clone()).is_true() {Scm::symbol("FN-APPLY-TC")} else {Scm::symbol("FN-APPLY")},{
// (variable-name var)
imports::variable_minus_name(&[var__18.clone()])},{
// (args (quote repr))
args__19.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__9.set({// Closure
let self__19 = self__19.clone();let var__18 = var__18.clone();let args__19 = args__19.clone();let tail_p__1 = tail_p__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__1 = args[0].clone();{
// (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?)))
fnc__1.clone().invoke(&[self__19.get(),{// Closure
let var__18 = var__18.clone();let args__19 = args__19.clone();let fnc__1 = fnc__1.clone();let tail_p__1 = tail_p__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-function-application var (args (quote transform) fnc) tail?)
Scm::func(make_minus_function_minus_application).invoke(&[var__18.clone(),{
// (args (quote transform) fnc)
args__19.clone().invoke(&[Scm::symbol("transform"),fnc__1.clone()])},tail_p__1.clone()])}})}])}})});Scm::anything();free_minus_vars__9.set({// Closure
let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (args (quote free-vars))
args__19.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__9.set({// Closure
let var__18 = var__18.clone();let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__23 = args[0].clone();{{
// (cond ...)
if ({
// (global-function? var)
imports::global_minus_function_p(&[var__18.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__23.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__18.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module__23.clone(),Scm::from("imports::")])}} else {{
// (error "invalid function application" var)
imports::error(&[Scm::from("invalid function application"),var__18.clone()])}}};{
// (print module (rustify-identifier (variable-name var)) "(&[")
imports::print(&[module__23.clone(),{
// (rustify-identifier (variable-name var))
imports::rustify_minus_identifier(&[{
// (variable-name var)
imports::variable_minus_name(&[var__18.clone()])}])},Scm::from("(&[")])};{
// (args (quote gen-rust) module)
args__19.clone().invoke(&[Scm::symbol("gen-rust"),module__23.clone()])};{
// (print module "])")
imports::print(&[module__23.clone(),Scm::from("])")])}}})});Scm::anything();self__19.set({// Closure
let repr__9 = repr__9.clone();let transform__9 = transform__9.clone();let free_minus_vars__9 = free_minus_vars__9.clone();let gen_minus_rust__9 = gen_minus_rust__9.clone();let var__18 = var__18.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__9 = args[0].clone();let args__20 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__9.clone()])}).is_true() {{
// (repr)
repr__9.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__9.clone()])}).is_true() {{
// (transform (car args))
transform__9.get().invoke(&[{
// (car args)
imports::car(&[args__20.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__9.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__9.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__9.clone()])}).is_true() {Scm::symbol("FN-APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__9.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__9.get().invoke(&[{
// (car args)
imports::car(&[args__20.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__9.clone()])}).is_true() {var__18.clone()} else {{
// (error "Unknown message FN-APPLICATION" msg)
imports::error(&[Scm::from("Unknown message FN-APPLICATION"),msg__9.clone()])}}}})});Scm::anything();self__19.get()}}}}}}}}}}.into()
}
pub fn make_minus_import(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let lib__4 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr__20, transform__20, free_minus_vars__18, gen_minus_libname__0, gen_minus_rust__20, self__30, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__30 = self__30.into_boxed();{let gen_minus_rust__20 = gen_minus_rust__20.into_boxed();{let gen_minus_libname__0 = gen_minus_libname__0.into_boxed();{let free_minus_vars__18 = free_minus_vars__18.into_boxed();{let transform__20 = transform__20.into_boxed();{let repr__20 = repr__20.into_boxed();{repr__20.set({// Closure
let lib__4 = lib__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
imports::cons(&[Scm::symbol("IMPORT"),lib__4.clone()])}})});Scm::anything();transform__20.set({// Closure
let self__30 = self__30.clone();let lib__4 = lib__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__22 = args[0].clone();{
// (func self (lambda () (make-import lib)))
func__22.clone().invoke(&[self__30.get(),{// Closure
let lib__4 = lib__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
Scm::func(make_minus_import).invoke(&[lib__4.clone()])}})}])}})});Scm::anything();free_minus_vars__18.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname__0.set({// Closure
let gen_minus_libname__0 = gen_minus_libname__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__38 = args[0].clone();let lib__5 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib__5.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__38.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module__38.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib__5.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib__5.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module__38.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module__38.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname__0.get().invoke(&[module__38.clone(),{
// (cdr lib)
imports::cdr(&[lib__5.clone()])}])}}}})});Scm::anything();gen_minus_rust__20.set({// Closure
let gen_minus_libname__0 = gen_minus_libname__0.clone();let lib__4 = lib__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__39 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module__39.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname__0.get().invoke(&[module__39.clone(),lib__4.clone()])};{
// (print module "::exports::*;")
imports::print(&[module__39.clone(),Scm::from("::exports::*;")])};{
// (println module)
imports::println(&[module__39.clone()])}}})});Scm::anything();self__30.set({// Closure
let repr__20 = repr__20.clone();let transform__20 = transform__20.clone();let free_minus_vars__18 = free_minus_vars__18.clone();let gen_minus_rust__20 = gen_minus_rust__20.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__20 = args[0].clone();let args__31 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__20.clone()])}).is_true() {{
// (repr)
repr__20.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__20.clone()])}).is_true() {{
// (transform (car args))
transform__20.get().invoke(&[{
// (car args)
imports::car(&[args__31.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__20.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__18.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__20.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__20.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__20.get().invoke(&[{
// (car args)
imports::car(&[args__31.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg__20.clone()])}}}})});Scm::anything();self__30.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import_minus_only(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let lib__6 = args[0].clone();let names__0 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr__21, transform__21, free_minus_vars__19, gen_minus_libname__1, gen_minus_imports__1, gen_minus_rust__21, self__31, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__31 = self__31.into_boxed();{let gen_minus_rust__21 = gen_minus_rust__21.into_boxed();{let gen_minus_imports__1 = gen_minus_imports__1.into_boxed();{let gen_minus_libname__1 = gen_minus_libname__1.into_boxed();{let free_minus_vars__19 = free_minus_vars__19.into_boxed();{let transform__21 = transform__21.into_boxed();{let repr__21 = repr__21.into_boxed();{repr__21.set({// Closure
let lib__6 = lib__6.clone();let names__0 = names__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
imports::cons(&[lib__6.clone(),names__0.clone()])}])}})});Scm::anything();transform__21.set({// Closure
let self__31 = self__31.clone();let lib__6 = lib__6.clone();let names__0 = names__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__23 = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func__23.clone().invoke(&[self__31.get(),{// Closure
let lib__6 = lib__6.clone();let names__0 = names__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
Scm::func(make_minus_import_minus_only).invoke(&[lib__6.clone(),names__0.clone()])}})}])}})});Scm::anything();free_minus_vars__19.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname__1.set({// Closure
let gen_minus_libname__1 = gen_minus_libname__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__40 = args[0].clone();let lib__7 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib__7.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__40.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module__40.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib__7.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib__7.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module__40.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module__40.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname__1.get().invoke(&[module__40.clone(),{
// (cdr lib)
imports::cdr(&[lib__7.clone()])}])}}}})});Scm::anything();gen_minus_imports__1.set({// Closure
let gen_minus_imports__1 = gen_minus_imports__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__41 = args[0].clone();let names__1 = args[1].clone();if ({
// (null? names)
imports::null_p(&[names__1.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
imports::print(&[module__41.clone(),{
// (rustify-identifier (car names))
imports::rustify_minus_identifier(&[{
// (car names)
imports::car(&[names__1.clone()])}])}])};{
// (print module ", ")
imports::print(&[module__41.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports__1.get().invoke(&[module__41.clone(),{
// (cdr names)
imports::cdr(&[names__1.clone()])}])}}}})});Scm::anything();gen_minus_rust__21.set({// Closure
let gen_minus_libname__1 = gen_minus_libname__1.clone();let lib__6 = lib__6.clone();let gen_minus_imports__1 = gen_minus_imports__1.clone();let names__0 = names__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__42 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module__42.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname__1.get().invoke(&[module__42.clone(),lib__6.clone()])};{
// (print module "::exports::{")
imports::print(&[module__42.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports__1.get().invoke(&[module__42.clone(),names__0.clone()])};{
// (print module "};")
imports::print(&[module__42.clone(),Scm::from("};")])};{
// (println module)
imports::println(&[module__42.clone()])}}})});Scm::anything();self__31.set({// Closure
let repr__21 = repr__21.clone();let transform__21 = transform__21.clone();let free_minus_vars__19 = free_minus_vars__19.clone();let gen_minus_rust__21 = gen_minus_rust__21.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__21 = args[0].clone();let args__32 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__21.clone()])}).is_true() {{
// (repr)
repr__21.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__21.clone()])}).is_true() {{
// (transform (car args))
transform__21.get().invoke(&[{
// (car args)
imports::car(&[args__32.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__21.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__19.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__21.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__21.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__21.get().invoke(&[{
// (car args)
imports::car(&[args__32.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg__21.clone()])}}}})});Scm::anything();self__31.get()}}}}}}}}}}}}.into()
}
pub fn make_minus_library(args: &[Scm]) -> Scm {
    {
        if args.len() != 6 {
            panic!("invalid arity")
        }
        let name__35 = args[0].clone();
        let globals__1 = args[1].clone();
        let init__4 = args[2].clone();
        let body__6 = args[3].clone();
        let imports__1 = args[4].clone();
        let exports__0 = args[5].clone();
        {
            // (let* ((tests (list (quote dummy))) (new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (_make-library name globals init new-body imports exports (cdr tests)))
            {
                // (let ((tests (list (quote dummy)))) (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests)))))
                {
                    let tests__0 = {
                        // (list (quote dummy))
                        imports::list(&[Scm::symbol("dummy")])
                    };
                    // (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests))))
                    let new_minus_body__0 = {
                        // (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
                        body__6.clone().invoke(&[Scm::symbol("transform"), {
                            // Closure
                            let tests__0 = tests__0.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node__9 = args[0].clone();
                                let ignore__0 = args[1].clone();
                                if ({
                                    // (eq? (node (quote kind)) (quote TESTSUITE))
                                    imports::eq_p(&[
                                        {
                                            // (node (quote kind))
                                            node__9.clone().invoke(&[Scm::symbol("kind")])
                                        },
                                        Scm::symbol("TESTSUITE"),
                                    ])
                                })
                                .is_true()
                                {
                                    {
                                        {
                                            // (set-cdr! tests (cons node (cdr tests)))
                                            imports::set_minus_cdr_i(&[tests__0.clone(), {
                                                // (cons node (cdr tests))
                                                imports::cons(&[node__9.clone(), {
                                                    // (cdr tests)
                                                    imports::cdr(&[tests__0.clone()])
                                                }])
                                            }])
                                        };
                                        {
                                            // (make-constant (quote *UNSPECIFIED*))
                                            make_minus_constant(&[Scm::symbol("*UNSPECIFIED*")])
                                        }
                                    }
                                } else {
                                    {
                                        // (ignore)
                                        ignore__0.clone().invoke(&[])
                                    }
                                }
                            })
                        }])
                    };
                    {
                        // (_make-library name globals init new-body imports exports (cdr tests))
                        Scm::func(__make_minus_library).invoke(&[
                            name__35.clone(),
                            globals__1.clone(),
                            init__4.clone(),
                            new_minus_body__0.clone(),
                            imports__1.clone(),
                            exports__0.clone(),
                            {
                                // (cdr tests)
                                imports::cdr(&[tests__0.clone()])
                            },
                        ])
                    }
                }
            }
        }
    }
    .into()
}
pub fn make_minus_nop(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (letrec ((repr (lambda () (quote (NOP)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module "(/*NOP*/)"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (quote (NOP)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module "(/*NOP*/)"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))) self))
                {
                    let [repr__1, transform__1, free_minus_vars__1, gen_minus_rust__1, self__11] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__11 = self__11.into_boxed();
                        {
                            let gen_minus_rust__1 = gen_minus_rust__1.into_boxed();
                            {
                                let free_minus_vars__1 = free_minus_vars__1.into_boxed();
                                {
                                    let transform__1 = transform__1.into_boxed();
                                    {
                                        let repr__1 = repr__1.into_boxed();
                                        {
                                            repr__1.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    Scm::pair(Scm::symbol("NOP"), Scm::Nil)
                                                })
                                            });
                                            Scm::anything();
                                            transform__1.set({
                                                // Closure
                                                let self__11 = self__11.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func__7 = args[0].clone();
                                                    {
                                                        // (func self (lambda () self))
                                                        func__7.clone().invoke(&[self__11.get(), {
                                                            // Closure
                                                            let self__11 = self__11.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                self__11.get()
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars__1.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (make-set)
                                                        imports::make_minus_set(&[])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            gen_minus_rust__1.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module__13 = args[0].clone();
                                                    {
                                                        // (print module "(/*NOP*/)")
                                                        imports::print(&[
                                                            module__13.clone(),
                                                            Scm::from("(/*NOP*/)"),
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__11.set({
                                                // Closure
                                                let repr__1 = repr__1.clone();
                                                let transform__1 = transform__1.clone();
                                                let free_minus_vars__1 = free_minus_vars__1.clone();
                                                let gen_minus_rust__1 = gen_minus_rust__1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg__1 = args[0].clone();
                                                    let args__10 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg__1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr__1.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg__1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform__1.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__10.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg__1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars__1.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg__1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NOP")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg__1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust__1.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__10.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NOP" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message NOP",
                                                                    ),
                                                                    msg__1.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__11.get()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    .into()
}
pub fn make_minus_null_minus_arg(args: &[Scm]) -> Scm {
    {if args.len() != 0{panic!("invalid arity")}{
// (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module ""))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote NULL-ARG)))) (set! transform (lambda (fnc) (fnc self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module ""))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))) self))
{let [repr__10, transform__10, free_minus_vars__10, gen_minus_rust__10, self__20, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__20 = self__20.into_boxed();{let gen_minus_rust__10 = gen_minus_rust__10.into_boxed();{let free_minus_vars__10 = free_minus_vars__10.into_boxed();{let transform__10 = transform__10.into_boxed();{let repr__10 = repr__10.into_boxed();{repr__10.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote NULL-ARG))
imports::list(&[Scm::symbol("NULL-ARG")])}})});Scm::anything();transform__10.set({// Closure
let self__20 = self__20.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__2 = args[0].clone();{
// (fnc self (lambda () self))
fnc__2.clone().invoke(&[self__20.get(),{// Closure
let self__20 = self__20.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__20.get()})}])}})});Scm::anything();free_minus_vars__10.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__10.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__24 = args[0].clone();{
// (print module "")
imports::print(&[module__24.clone(),Scm::from("")])}})});Scm::anything();self__20.set({// Closure
let repr__10 = repr__10.clone();let transform__10 = transform__10.clone();let free_minus_vars__10 = free_minus_vars__10.clone();let gen_minus_rust__10 = gen_minus_rust__10.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__10 = args[0].clone();let args__21 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__10.clone()])}).is_true() {{
// (repr)
repr__10.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__10.clone()])}).is_true() {{
// (transform (car args))
transform__10.get().invoke(&[{
// (car args)
imports::car(&[args__21.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__10.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__10.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__10.clone()])}).is_true() {Scm::symbol("NULL-ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__10.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__10.get().invoke(&[{
// (car args)
imports::car(&[args__21.clone()])}])}} else {{
// (error "Unknown message NULL-ARG" msg)
imports::error(&[Scm::from("Unknown message NULL-ARG"),msg__10.clone()])}}}})});Scm::anything();self__20.get()}}}}}}}}}}.into()
}
pub fn make_minus_program(args: &[Scm]) -> Scm {
    {if args.len() != 5{panic!("invalid arity")}let globals__0 = args[0].clone();let imports__0 = args[1].clone();let init__3 = args[2].clone();let body__5 = args[3].clone();let libraries__0 = args[4].clone();{
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (set! transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (set! gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))) self))
{let [repr__16, transform__16, gen_minus_imports__0, gen_minus_rust__16, self__26, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__26 = self__26.into_boxed();{let gen_minus_rust__16 = gen_minus_rust__16.into_boxed();{let gen_minus_imports__0 = gen_minus_imports__0.into_boxed();{let transform__16 = transform__16.into_boxed();{let repr__16 = repr__16.into_boxed();{repr__16.set({// Closure
let globals__0 = globals__0.clone();let imports__0 = imports__0.clone();let body__5 = body__5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
imports::cons(&[globals__0.clone(),{
// (cons imports (body (quote repr)))
imports::cons(&[imports__0.clone(),{
// (body (quote repr))
body__5.clone().invoke(&[Scm::symbol("repr")])}])}])}])}})});Scm::anything();transform__16.set({// Closure
let self__26 = self__26.clone();let globals__0 = globals__0.clone();let imports__0 = imports__0.clone();let init__3 = init__3.clone();let body__5 = body__5.clone();let libraries__0 = libraries__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__18 = args[0].clone();{
// (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries)))
func__18.clone().invoke(&[self__26.get(),{// Closure
let globals__0 = globals__0.clone();let imports__0 = imports__0.clone();let init__3 = init__3.clone();let body__5 = body__5.clone();let func__18 = func__18.clone();let libraries__0 = libraries__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-program globals imports init (body (quote transform) func) libraries)
Scm::func(make_minus_program).invoke(&[globals__0.clone(),imports__0.clone(),init__3.clone(),{
// (body (quote transform) func)
body__5.clone().invoke(&[Scm::symbol("transform"),func__18.clone()])},libraries__0.clone()])}})}])}})});Scm::anything();gen_minus_imports__0.set({// Closure
let imports__0 = imports__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__32 = args[0].clone();{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module__32 = module__32.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i__0 = args[0].clone();{
// (i (quote gen-rust) module)
i__0.clone().invoke(&[Scm::symbol("gen-rust"),module__32.clone()])}})},imports__0.clone()])}})});Scm::anything();gen_minus_rust__16.set({// Closure
let gen_minus_imports__0 = gen_minus_imports__0.clone();let globals__0 = globals__0.clone();let init__3 = init__3.clone();let body__5 = body__5.clone();let libraries__0 = libraries__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__33 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println(&[module__33.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
imports::print(&[module__33.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block(&[module__33.clone(),{// Closure
let gen_minus_imports__0 = gen_minus_imports__0.clone();let module__33 = module__33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports__0.get().invoke(&[module__33.clone()])}})}])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module__33.clone(),globals__0.clone()])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (print module "pub fn main()")
imports::print(&[module__33.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block(&[module__33.clone(),{// Closure
let module__33 = module__33.clone();let init__3 = init__3.clone();let body__5 = body__5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module__33.clone()])};{
// (println module "eprintln!(\"built with\");")
imports::println(&[module__33.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println(&[module__33.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each(&[{// Closure
let module__33 = module__33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib__2 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module__33.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each(&[{// Closure
let module__33 = module__33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l__0 = args[0].clone();{{
// (print module (rustify-libname l))
imports::print(&[module__33.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l__0.clone()])}])};{
// (print module "::")
imports::print(&[module__33.clone(),Scm::from("::")])}}})},lib__2.clone()])};{
// (print module "initialize();")
imports::print(&[module__33.clone(),Scm::from("initialize();")])};{
// (println module)
imports::println(&[module__33.clone()])}}})},init__3.clone()])};{
// (body (quote gen-rust) module)
body__5.clone().invoke(&[Scm::symbol("gen-rust"),module__33.clone()])};{
// (println module ";")
imports::println(&[module__33.clone(),Scm::from(";")])}}})}])};{
// (println module)
imports::println(&[module__33.clone()])};{
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules(&[module__33.clone(),libraries__0.clone()])}}})});Scm::anything();self__26.set({// Closure
let repr__16 = repr__16.clone();let transform__16 = transform__16.clone();let gen_minus_rust__16 = gen_minus_rust__16.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__16 = args[0].clone();let args__27 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__16.clone()])}).is_true() {{
// (repr)
repr__16.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__16.clone()])}).is_true() {{
// (transform (car args))
transform__16.get().invoke(&[{
// (car args)
imports::car(&[args__27.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__16.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__16.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__16.get().invoke(&[{
// (car args)
imports::car(&[args__27.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
imports::error(&[Scm::from("Unknown message PROGRAM"),msg__16.clone()])}}}})});Scm::anything();self__26.get()}}}}}}}}}}.into()
}
pub fn make_minus_reference(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let var__15 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote GET) (variable-name var)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) (variable-name var)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr__3, transform__3, free_minus_vars__3, gen_minus_rust__3, self__13, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__13 = self__13.into_boxed();{let gen_minus_rust__3 = gen_minus_rust__3.into_boxed();{let free_minus_vars__3 = free_minus_vars__3.into_boxed();{let transform__3 = transform__3.into_boxed();{let repr__3 = repr__3.into_boxed();{repr__3.set({// Closure
let var__15 = var__15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) (variable-name var))
imports::list(&[Scm::symbol("GET"),{
// (variable-name var)
imports::variable_minus_name(&[var__15.clone()])}])}})});Scm::anything();transform__3.set({// Closure
let self__13 = self__13.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__9 = args[0].clone();{
// (func self (lambda () self))
func__9.clone().invoke(&[self__13.get(),{// Closure
let self__13 = self__13.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__13.get()})}])}})});Scm::anything();free_minus_vars__3.set({// Closure
let var__15 = var__15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var__15.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var__15.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var__15.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (make-set) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (make-set)
imports::make_minus_set(&[])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__15.clone()])}])}])}}})});Scm::anything();gen_minus_rust__3.set({// Closure
let var__15 = var__15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__16 = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))
{let name__31 = {
// (variable-name var)
imports::variable_minus_name(&[var__15.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__15.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".with(|value| value.get())")
imports::print(&[module__16.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__31.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__15.clone()])}).is_true() {{
// (print module "Scm::func(" (rustify-identifier name) ")")
imports::print(&[module__16.clone(),Scm::from("Scm::func("),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__31.clone()])},Scm::from(")")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__15.clone()])}).is_true() {{
// (print module "Scm::func(imports::" (rustify-identifier name) ")")
imports::print(&[module__16.clone(),Scm::from("Scm::func(imports::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__31.clone()])},Scm::from(")")])}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var__15.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".get()")
imports::print(&[module__16.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__31.clone()])},Scm::from(".get()")])}} else {{
// (print module (rustify-identifier name) ".clone()")
imports::print(&[module__16.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__31.clone()])},Scm::from(".clone()")])}}}}}})});Scm::anything();self__13.set({// Closure
let repr__3 = repr__3.clone();let transform__3 = transform__3.clone();let free_minus_vars__3 = free_minus_vars__3.clone();let gen_minus_rust__3 = gen_minus_rust__3.clone();let var__15 = var__15.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__3 = args[0].clone();let args__12 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__3.clone()])}).is_true() {{
// (repr)
repr__3.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__3.clone()])}).is_true() {{
// (transform (car args))
transform__3.get().invoke(&[{
// (car args)
imports::car(&[args__12.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__3.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__3.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__3.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__3.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__3.get().invoke(&[{
// (car args)
imports::car(&[args__12.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg__3.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var__15.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__3.clone()])}).is_true() {var__15.clone()} else {{
// (error "Unknown message REFERENCE" msg)
imports::error(&[Scm::from("Unknown message REFERENCE"),msg__3.clone()])}}}})});Scm::anything();self__13.get()}}}}}}}}}}.into()
}
pub fn make_minus_sequence(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let first__1 = args[0].clone();let next__0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr__7, transform__7, free_minus_vars__7, gen_minus_rust_minus_inner__1, gen_minus_rust__7, self__17, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__17 = self__17.into_boxed();{let gen_minus_rust__7 = gen_minus_rust__7.into_boxed();{let gen_minus_rust_minus_inner__1 = gen_minus_rust_minus_inner__1.into_boxed();{let free_minus_vars__7 = free_minus_vars__7.into_boxed();{let transform__7 = transform__7.into_boxed();{let repr__7 = repr__7.into_boxed();{repr__7.set({// Closure
let first__1 = first__1.clone();let next__0 = next__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list(&[Scm::symbol("SEQUENCE"),{
// (first (quote repr))
first__1.clone().invoke(&[Scm::symbol("repr")])},{
// (next (quote repr))
next__0.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__7.set({// Closure
let self__17 = self__17.clone();let next__0 = next__0.clone();let first__1 = first__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__13 = args[0].clone();{
// (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))))
func__13.clone().invoke(&[self__17.get(),{// Closure
let next__0 = next__0.clone();let func__13 = func__13.clone();let first__1 = first__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))
{
// (let ((a (first (quote transform) func))) (let ((b (next (quote transform) func))) (begin (make-sequence a b))))
{let a__5 = {
// (first (quote transform) func)
first__1.clone().invoke(&[Scm::symbol("transform"),func__13.clone()])};
// (let ((b (next (quote transform) func))) (begin (make-sequence a b)))
let b__1 = {
// (next (quote transform) func)
next__0.clone().invoke(&[Scm::symbol("transform"),func__13.clone()])};{
// (make-sequence a b)
Scm::func(make_minus_sequence).invoke(&[a__5.clone(),b__1.clone()])}}}}})}])}})});Scm::anything();free_minus_vars__7.set({// Closure
let first__1 = first__1.clone();let next__0 = next__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (first (quote free-vars))
first__1.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next__0.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner__1.set({// Closure
let first__1 = first__1.clone();let next__0 = next__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__20 = args[0].clone();{{
// (first (quote gen-rust) module)
first__1.clone().invoke(&[Scm::symbol("gen-rust"),module__20.clone()])};{
// (print module ";")
imports::print(&[module__20.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next__0.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next__0.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__20.clone()])}} else {{
// (next (quote gen-rust) module)
next__0.clone().invoke(&[Scm::symbol("gen-rust"),module__20.clone()])}}}})});Scm::anything();gen_minus_rust__7.set({// Closure
let gen_minus_rust_minus_inner__1 = gen_minus_rust_minus_inner__1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__21 = args[0].clone();{{
// (print module "{")
imports::print(&[module__21.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner__1.get().invoke(&[module__21.clone()])};{
// (print module "}")
imports::print(&[module__21.clone(),Scm::from("}")])}}})});Scm::anything();self__17.set({// Closure
let repr__7 = repr__7.clone();let transform__7 = transform__7.clone();let free_minus_vars__7 = free_minus_vars__7.clone();let gen_minus_rust__7 = gen_minus_rust__7.clone();let gen_minus_rust_minus_inner__1 = gen_minus_rust_minus_inner__1.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__7 = args[0].clone();let args__16 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__7.clone()])}).is_true() {{
// (repr)
repr__7.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__7.clone()])}).is_true() {{
// (transform (car args))
transform__7.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__7.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__7.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__7.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__7.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__7.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__7.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner__1.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
imports::error(&[Scm::from("Unknown message SEQUENCE"),msg__7.clone()])}}}})});Scm::anything();self__17.get()}}}}}}}}}}}.into()
}
pub fn make_minus_testcase(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let description__0 = args[0].clone();let body__9 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
{let [repr__22, transform__22, free_minus_vars__20, gen_minus_rust__22, self__32, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__32 = self__32.into_boxed();{let gen_minus_rust__22 = gen_minus_rust__22.into_boxed();{let free_minus_vars__20 = free_minus_vars__20.into_boxed();{let transform__22 = transform__22.into_boxed();{let repr__22 = repr__22.into_boxed();{repr__22.set({// Closure
let description__0 = description__0.clone();let body__9 = body__9.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTCASE) description body)
imports::list(&[Scm::symbol("TESTCASE"),description__0.clone(),body__9.clone()])}})});Scm::anything();transform__22.set({// Closure
let self__32 = self__32.clone();let description__0 = description__0.clone();let body__9 = body__9.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__24 = args[0].clone();{
// (func self (lambda () (make-testcase description (body (quote transform) func))))
func__24.clone().invoke(&[self__32.get(),{// Closure
let description__0 = description__0.clone();let body__9 = body__9.clone();let func__24 = func__24.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testcase description (body (quote transform) func))
Scm::func(make_minus_testcase).invoke(&[description__0.clone(),{
// (body (quote transform) func)
body__9.clone().invoke(&[Scm::symbol("transform"),func__24.clone()])}])}})}])}})});Scm::anything();free_minus_vars__20.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__22.set({// Closure
let description__0 = description__0.clone();let body__9 = body__9.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__43 = args[0].clone();{{
// (println module "#[test]")
imports::println(&[module__43.clone(),Scm::from("#[test]")])};{
// (println module "fn " (rustify-testname description) "() {")
imports::println(&[module__43.clone(),Scm::from("fn "),{
// (rustify-testname description)
imports::rustify_minus_testname(&[description__0.clone()])},Scm::from("() {")])};{
// (println module "super::initialize();")
imports::println(&[module__43.clone(),Scm::from("super::initialize();")])};{
// (body (quote gen-rust) module)
body__9.clone().invoke(&[Scm::symbol("gen-rust"),module__43.clone()])};{
// (println module "}")
imports::println(&[module__43.clone(),Scm::from("}")])}}})});Scm::anything();self__32.set({// Closure
let repr__22 = repr__22.clone();let transform__22 = transform__22.clone();let free_minus_vars__20 = free_minus_vars__20.clone();let gen_minus_rust__22 = gen_minus_rust__22.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__22 = args[0].clone();let args__33 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__22.clone()])}).is_true() {{
// (repr)
repr__22.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__22.clone()])}).is_true() {{
// (transform (car args))
transform__22.get().invoke(&[{
// (car args)
imports::car(&[args__33.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__22.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__20.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__22.clone()])}).is_true() {Scm::symbol("TESTCASE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__22.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__22.get().invoke(&[{
// (car args)
imports::car(&[args__33.clone()])}])}} else {{
// (error "Unknown message TESTCASE" msg)
imports::error(&[Scm::from("Unknown message TESTCASE"),msg__22.clone()])}}}})});Scm::anything();self__32.get()}}}}}}}}}}.into()
}
pub fn make_minus_testsuite(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name__38 = args[0].clone();let cases__0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr__23, transform__23, free_minus_vars__21, gen_minus_rust__23, self__33, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__33 = self__33.into_boxed();{let gen_minus_rust__23 = gen_minus_rust__23.into_boxed();{let free_minus_vars__21 = free_minus_vars__21.into_boxed();{let transform__23 = transform__23.into_boxed();{let repr__23 = repr__23.into_boxed();{repr__23.set({// Closure
let name__38 = name__38.clone();let cases__0 = cases__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
imports::list(&[Scm::symbol("TESTSUITE"),name__38.clone(),cases__0.clone()])}})});Scm::anything();transform__23.set({// Closure
let self__33 = self__33.clone();let name__38 = name__38.clone();let cases__0 = cases__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__25 = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func__25.clone().invoke(&[self__33.get(),{// Closure
let name__38 = name__38.clone();let func__25 = func__25.clone();let cases__0 = cases__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
Scm::func(make_minus_testsuite).invoke(&[name__38.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
imports::map(&[{// Closure
let func__25 = func__25.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c__0 = args[0].clone();{
// (c (quote transform) func)
c__0.clone().invoke(&[Scm::symbol("transform"),func__25.clone()])}})},cases__0.clone()])}])}})}])}})});Scm::anything();free_minus_vars__21.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__23.set({// Closure
let cases__0 = cases__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__44 = args[0].clone();{{
// (println module "#[cfg(test)]")
imports::println(&[module__44.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
imports::println(&[module__44.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
imports::println(&[module__44.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
imports::for_minus_each(&[{// Closure
let module__44 = module__44.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c__1 = args[0].clone();{
// (c (quote gen-rust) module)
c__1.clone().invoke(&[Scm::symbol("gen-rust"),module__44.clone()])}})},cases__0.clone()])};{
// (println module "}")
imports::println(&[module__44.clone(),Scm::from("}")])}}})});Scm::anything();self__33.set({// Closure
let repr__23 = repr__23.clone();let transform__23 = transform__23.clone();let free_minus_vars__21 = free_minus_vars__21.clone();let gen_minus_rust__23 = gen_minus_rust__23.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__23 = args[0].clone();let args__34 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__23.clone()])}).is_true() {{
// (repr)
repr__23.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__23.clone()])}).is_true() {{
// (transform (car args))
transform__23.get().invoke(&[{
// (car args)
imports::car(&[args__34.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__23.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__21.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__23.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__23.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__23.get().invoke(&[{
// (car args)
imports::car(&[args__34.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
imports::error(&[Scm::from("Unknown message TESTSUITE"),msg__23.clone()])}}}})});Scm::anything();self__33.get()}}}}}}}}}}.into()
}
pub fn make_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars__2 = args[0].clone();let varvar__0 = args[1].clone();let body__4 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr__15, transform__15, free_minus_vars__16, gen_minus_rust__15, self__25, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__25 = self__25.into_boxed();{let gen_minus_rust__15 = gen_minus_rust__15.into_boxed();{let free_minus_vars__16 = free_minus_vars__16.into_boxed();{let transform__15 = transform__15.into_boxed();{let repr__15 = repr__15.into_boxed();{repr__15.set({// Closure
let vars__2 = vars__2.clone();let varvar__0 = varvar__0.clone();let body__4 = body__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr)))
imports::list(&[Scm::symbol("VARARG-ABSTRACTION"),vars__2.clone(),varvar__0.clone(),{
// (body (quote repr))
body__4.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__15.set({// Closure
let self__25 = self__25.clone();let vars__2 = vars__2.clone();let varvar__0 = varvar__0.clone();let body__4 = body__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__17 = args[0].clone();{
// (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func))))
func__17.clone().invoke(&[self__25.get(),{// Closure
let vars__2 = vars__2.clone();let varvar__0 = varvar__0.clone();let body__4 = body__4.clone();let func__17 = func__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-vararg-abstraction vars varvar (body (quote transform) func))
Scm::func(make_minus_vararg_minus_abstraction).invoke(&[vars__2.clone(),varvar__0.clone(),{
// (body (quote transform) func)
body__4.clone().invoke(&[Scm::symbol("transform"),func__17.clone()])}])}})}])}})});Scm::anything();free_minus_vars__16.set({// Closure
let body__4 = body__4.clone();let varvar__0 = varvar__0.clone();let vars__2 = vars__2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars))))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__4.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name (cons varvar vars)))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name (cons varvar vars))
imports::map(&[Scm::func(imports::variable_minus_name),{
// (cons varvar vars)
imports::cons(&[varvar__0.clone(),vars__2.clone()])}])}])}])}})});Scm::anything();gen_minus_rust__15.set({// Closure
let varvar__0 = varvar__0.clone();let vars__2 = vars__2.clone();let body__4 = body__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__31 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params__2 = Scm::symbol("*uninitialized*");{let gen_minus_params__2 = gen_minus_params__2.into_boxed();{gen_minus_params__2.set({// Closure
let module__31 = module__31.clone();let gen_minus_params__2 = gen_minus_params__2.clone();let varvar__0 = varvar__0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star___1 = args[0].clone();let k__1 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star___1.clone()])}).is_true() {{{
// (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();")
imports::print(&[module__31.clone(),Scm::from("let "),{
// (rustify-identifier (variable-name (car p*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star___1.clone()])}])}])},Scm::from(" = args["),k__1.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params__2.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star___1.clone()])},{
// (+ k 1)
imports::_plus_(&[k__1.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")
imports::print(&[module__31.clone(),Scm::from("let "),{
// (rustify-identifier (variable-name varvar))
imports::rustify_minus_identifier(&[{
// (variable-name varvar)
imports::variable_minus_name(&[varvar__0.clone()])}])},Scm::from(" = Scm::list(&args["),k__1.clone(),Scm::from("..]);")])}}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__31.clone(),{// Closure
let module__31 = module__31.clone();let vars__2 = vars__2.clone();let gen_minus_params__2 = gen_minus_params__2.clone();let body__4 = body__4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}")
imports::print(&[module__31.clone(),Scm::from("if args.len() < "),{
// (length vars)
imports::length(&[vars__2.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params vars 0)
gen_minus_params__2.get().invoke(&[vars__2.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body__4.clone().invoke(&[Scm::symbol("gen-rust"),module__31.clone()])}}})}])}}}}}}})});Scm::anything();self__25.set({// Closure
let repr__15 = repr__15.clone();let transform__15 = transform__15.clone();let free_minus_vars__16 = free_minus_vars__16.clone();let gen_minus_rust__15 = gen_minus_rust__15.clone();let vars__2 = vars__2.clone();let varvar__0 = varvar__0.clone();let body__4 = body__4.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__15 = args[0].clone();let args__26 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__15.clone()])}).is_true() {{
// (repr)
repr__15.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__15.clone()])}).is_true() {{
// (transform (car args))
transform__15.get().invoke(&[{
// (car args)
imports::car(&[args__26.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__15.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__16.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__15.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__15.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__15.get().invoke(&[{
// (car args)
imports::car(&[args__26.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__15.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__2.clone()])}} else if ({
// (eq? (quote get-vararg) msg)
imports::eq_p(&[Scm::symbol("get-vararg"),msg__15.clone()])}).is_true() {{
// (variable-name varvar)
imports::variable_minus_name(&[varvar__0.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__15.clone()])}).is_true() {vars__2.clone()} else if ({
// (eq? (quote get-varvar) msg)
imports::eq_p(&[Scm::symbol("get-varvar"),msg__15.clone()])}).is_true() {varvar__0.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__15.clone()])}).is_true() {body__4.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg__15.clone()])}}}})});Scm::anything();self__25.get()}}}}}}}}}}.into()
}
pub fn procedure_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__16 = args[0].clone();
        if ({
            // (eq? (obj (quote kind)) (quote ABSTRACTION))
            imports::eq_p(&[
                {
                    // (obj (quote kind))
                    obj__16.clone().invoke(&[Scm::symbol("kind")])
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
                imports::eq_p(&[
                    {
                        // (obj (quote kind))
                        obj__16.clone().invoke(&[Scm::symbol("kind")])
                    },
                    Scm::symbol("VARARG-ABSTRACTION"),
                ])
            }
        }
    }
    .into()
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
    crate::sunny::table::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (ast-node? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (procedure-node? obj) ...)
            (/*NOP*/)
        };
        {
            // (define (make-comment comment node) ...)
            (/*NOP*/)
        };
        {
            // (define (make-nop) ...)
            (/*NOP*/)
        };
        {
            // (define (make-constant val) ...)
            (/*NOP*/)
        };
        {
            // (define (make-reference var) ...)
            (/*NOP*/)
        };
        {
            // (define (make-assignment var val) ...)
            (/*NOP*/)
        };
        {
            // (define (make-definition var val) ...)
            (/*NOP*/)
        };
        {
            // (define (make-alternative condition consequent alternative) ...)
            (/*NOP*/)
        };
        {
            // (define (make-sequence first next) ...)
            (/*NOP*/)
        };
        {
            // (define (make-application func args tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (make-function-application var args tail?) ...)
            (/*NOP*/)
        };
        {
            // (define (make-null-arg) ...)
            (/*NOP*/)
        };
        {
            // (define (make-args arg next) ...)
            (/*NOP*/)
        };
        {
            // (define (make-fixlet vars args body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-closure function) ...)
            (/*NOP*/)
        };
        {
            // (define (make-abstraction vars body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-vararg-abstraction vars varvar body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-program globals imports init body libraries) ...)
            (/*NOP*/)
        };
        {
            // (define (make-library name globals init body imports exports) ...)
            (/*NOP*/)
        };
        {
            // (define (_make-library name globals init body imports exports testsuite) ...)
            (/*NOP*/)
        };
        {
            // (define (make-boxify var body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-export var exname) ...)
            (/*NOP*/)
        };
        {
            // (define (make-import lib) ...)
            (/*NOP*/)
        };
        {
            // (define (make-import-only lib names) ...)
            (/*NOP*/)
        };
        {
            // (define (make-testcase description body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-testsuite name cases) ...)
            (/*NOP*/)
        };
        {
            // (define (make-assert condition) ...)
            (/*NOP*/)
        };
        {
            // (define (free-var-name name) ...)
            (/*NOP*/)
        }
    };
}
