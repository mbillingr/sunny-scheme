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
    {if args.len() != 7{panic!("invalid arity")}let name_36 = args[0].clone();let globals_2 = args[1].clone();let init_5 = args[2].clone();let body_7 = args[3].clone();let imports_2 = args[4].clone();let exports_1 = args[5].clone();let testsuite_0 = args[6].clone();{
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) (map (lambda (i) (i (quote transform) func)) imports) (map (lambda (e) (e (quote transform) func)) exports) (map (lambda (t) (t (quote transform) func)) testsuite)))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-exports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) (map (lambda (i) (i (quote transform) func)) imports) (map (lambda (e) (e (quote transform) func)) exports) (map (lambda (t) (t (quote transform) func)) testsuite)))))) (set! gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))) self))
{let [repr_17, transform_17, gen_minus_exports_0, gen_minus_rust_17, self__33, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__33 = self__33.into_boxed();{let gen_minus_rust_17 = gen_minus_rust_17.into_boxed();{let gen_minus_exports_0 = gen_minus_exports_0.into_boxed();{let transform_17 = transform_17.into_boxed();{let repr_17 = repr_17.into_boxed();{repr_17.set({// Closure
let name_36 = name_36.clone();let exports_1 = exports_1.clone();let imports_2 = imports_2.clone();let globals_2 = globals_2.clone();let body_7 = body_7.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
imports::append(&[Scm::symbol("LIBRARY"),name_36.clone(),exports_1.clone(),imports_2.clone(),globals_2.clone(),{
// (body (quote repr))
body_7.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_17.set({// Closure
let self__33 = self__33.clone();let name_36 = name_36.clone();let globals_2 = globals_2.clone();let init_5 = init_5.clone();let body_7 = body_7.clone();let imports_2 = imports_2.clone();let exports_1 = exports_1.clone();let testsuite_0 = testsuite_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_19 = args[0].clone();{
// (func self (lambda () (_make-library name globals init (body (quote transform) func) (map (lambda (i) (i (quote transform) func)) imports) (map (lambda (e) (e (quote transform) func)) exports) (map (lambda (t) (t (quote transform) func)) testsuite))))
func_19.clone().invoke(&[self__33.get(),{// Closure
let name_36 = name_36.clone();let globals_2 = globals_2.clone();let init_5 = init_5.clone();let body_7 = body_7.clone();let func_19 = func_19.clone();let imports_2 = imports_2.clone();let exports_1 = exports_1.clone();let testsuite_0 = testsuite_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (_make-library name globals init (body (quote transform) func) (map (lambda (i) (i (quote transform) func)) imports) (map (lambda (e) (e (quote transform) func)) exports) (map (lambda (t) (t (quote transform) func)) testsuite))
Scm::func(__make_minus_library).invoke(&[name_36.clone(),globals_2.clone(),init_5.clone(),{
// (body (quote transform) func)
body_7.clone().invoke(&[Scm::symbol("transform"),func_19.clone()])},{
// (map (lambda (i) (i (quote transform) func)) imports)
imports::map(&[{// Closure
let func_19 = func_19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i_1 = args[0].clone();{
// (i (quote transform) func)
i_1.clone().invoke(&[Scm::symbol("transform"),func_19.clone()])}})},imports_2.clone()])},{
// (map (lambda (e) (e (quote transform) func)) exports)
imports::map(&[{// Closure
let func_19 = func_19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let e_0 = args[0].clone();{
// (e (quote transform) func)
e_0.clone().invoke(&[Scm::symbol("transform"),func_19.clone()])}})},exports_1.clone()])},{
// (map (lambda (t) (t (quote transform) func)) testsuite)
imports::map(&[{// Closure
let func_19 = func_19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let t_13 = args[0].clone();{
// (t (quote transform) func)
t_13.clone().invoke(&[Scm::symbol("transform"),func_19.clone()])}})},testsuite_0.clone()])}])}})}])}})});Scm::anything();gen_minus_exports_0.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_34 = args[0].clone();let exports_2 = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each(&[{// Closure
let module_34 = module_34.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo_0 = args[0].clone();{
// (expo (quote gen-rust) module)
expo_0.clone().invoke(&[Scm::symbol("gen-rust"),module_34.clone()])}})},exports_2.clone()])}})});Scm::anything();gen_minus_rust_17.set({// Closure
let imports_2 = imports_2.clone();let gen_minus_exports_0 = gen_minus_exports_0.clone();let exports_1 = exports_1.clone();let globals_2 = globals_2.clone();let body_7 = body_7.clone();let init_5 = init_5.clone();let testsuite_0 = testsuite_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_35 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println(&[module_35.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
imports::print(&[module_35.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block(&[module_35.clone(),{// Closure
let module_35 = module_35.clone();let imports_2 = imports_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module_35 = module_35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i_2 = args[0].clone();{
// (i (quote gen-rust) module)
i_2.clone().invoke(&[Scm::symbol("gen-rust"),module_35.clone()])}})},imports_2.clone()])}})}])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (print module "pub mod exports")
imports::print(&[module_35.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block(&[module_35.clone(),{// Closure
let gen_minus_exports_0 = gen_minus_exports_0.clone();let module_35 = module_35.clone();let exports_1 = exports_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports_0.get().invoke(&[module_35.clone(),exports_1.clone()])}})}])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module_35.clone(),globals_2.clone()])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (println module)
imports::println(&[module_35.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p(&[Scm::symbol("NOP"),{
// (body (quote kind))
body_7.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
imports::println(&[module_35.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println(&[module_35.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
imports::println(&[module_35.clone()])};{
// (println module "pub fn initialize() {")
imports::println(&[module_35.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println(&[module_35.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println(&[module_35.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
imports::println(&[module_35.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each(&[{// Closure
let module_35 = module_35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib_3 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module_35.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each(&[{// Closure
let module_35 = module_35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l_1 = args[0].clone();{
// (print module (rustify-libname l) "::")
imports::print(&[module_35.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l_1.clone()])},Scm::from("::")])}})},lib_3.clone()])};{
// (println module "initialize();")
imports::println(&[module_35.clone(),Scm::from("initialize();")])}}})},init_5.clone()])};{
// (body (quote gen-rust) module)
body_7.clone().invoke(&[Scm::symbol("gen-rust"),module_35.clone()])};{
// (println module ";}")
imports::println(&[module_35.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) testsuite)
imports::for_minus_each(&[{// Closure
let module_35 = module_35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test_0 = args[0].clone();{
// (test (quote gen-rust) module)
test_0.clone().invoke(&[Scm::symbol("gen-rust"),module_35.clone()])}})},testsuite_0.clone()])}}})});Scm::anything();self__33.set({// Closure
let repr_17 = repr_17.clone();let transform_17 = transform_17.clone();let name_36 = name_36.clone();let gen_minus_rust_17 = gen_minus_rust_17.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_17 = args[0].clone();let args__28 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_17.clone()])}).is_true() {{
// (repr)
repr_17.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_17.clone()])}).is_true() {{
// (transform (car args))
transform_17.get().invoke(&[{
// (car args)
imports::car(&[args__28.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_17.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
imports::eq_p(&[Scm::symbol("libname"),msg_17.clone()])}).is_true() {name_36.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_17.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_17.get().invoke(&[{
// (car args)
imports::car(&[args__28.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
imports::error(&[Scm::from("Unknown message LIBRARY"),msg_17.clone()])}}}})});Scm::anything();self__33.get()}}}}}}}}}}.into()
}
pub fn ast_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_15 = args[0].clone();
        {
            // (procedure? obj)
            imports::procedure_p(&[obj_15.clone()])
        }
    }
    .into()
}
pub fn free_minus_var_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name_39 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (symbol? name)
                imports::symbol_p(&[name_39.clone()])
            })
            .is_true()
            {
                name_39.clone()
            } else if ({
                // (string? name)
                imports::string_p(&[name_39.clone()])
            })
            .is_true()
            {
                {
                    // (string->symbol name)
                    imports::string_minus__g_symbol(&[name_39.clone()])
                }
            } else {
                {
                    // (error "Invalid variable name" name)
                    imports::error(&[Scm::from("Invalid variable name"), name_39.clone()])
                }
            }
        }
    }
    .into()
}
pub fn make_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let vars_1 = args[0].clone();let body_3 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (variable-name (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (variable-name (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))) self))
{let [repr_14, transform_14, free_minus_vars_15, gen_minus_rust_14, self__30, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__30 = self__30.into_boxed();{let gen_minus_rust_14 = gen_minus_rust_14.into_boxed();{let free_minus_vars_15 = free_minus_vars_15.into_boxed();{let transform_14 = transform_14.into_boxed();{let repr_14 = repr_14.into_boxed();{repr_14.set({// Closure
let vars_1 = vars_1.clone();let body_3 = body_3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ABSTRACTION) vars (body (quote repr)))
imports::list(&[Scm::symbol("ABSTRACTION"),vars_1.clone(),{
// (body (quote repr))
body_3.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_14.set({// Closure
let self__30 = self__30.clone();let vars_1 = vars_1.clone();let body_3 = body_3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_16 = args[0].clone();{
// (func self (lambda () (make-abstraction vars (body (quote transform) func))))
func_16.clone().invoke(&[self__30.get(),{// Closure
let vars_1 = vars_1.clone();let body_3 = body_3.clone();let func_16 = func_16.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-abstraction vars (body (quote transform) func))
Scm::func(make_minus_abstraction).invoke(&[vars_1.clone(),{
// (body (quote transform) func)
body_3.clone().invoke(&[Scm::symbol("transform"),func_16.clone()])}])}})}])}})});Scm::anything();free_minus_vars_15.set({// Closure
let body_3 = body_3.clone();let vars_1 = vars_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body_3.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars_1.clone()])}])}])}})});Scm::anything();gen_minus_rust_14.set({// Closure
let vars_1 = vars_1.clone();let body_3 = body_3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_30 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (variable-name (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (variable-name (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params_1 = Scm::symbol("*uninitialized*");{let gen_minus_params_1 = gen_minus_params_1.into_boxed();{gen_minus_params_1.set({// Closure
let module_30 = module_30.clone();let gen_minus_params_1 = gen_minus_params_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star__0 = args[0].clone();let k_0 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star__0.clone()])}).is_true() {{{
// (print module "let ")
imports::print(&[module_30.clone(),Scm::from("let ")])};{
// (print module (variable-name (car p*)))
imports::print(&[module_30.clone(),{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star__0.clone()])}])}])};{
// (print module " = args[")
imports::print(&[module_30.clone(),Scm::from(" = args[")])};{
// (print module k)
imports::print(&[module_30.clone(),k_0.clone()])};{
// (print module "].clone();")
imports::print(&[module_30.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params_1.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star__0.clone()])},{
// (+ k 1)
imports::_plus_(&[k_0.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module_30.clone(),{// Closure
let module_30 = module_30.clone();let vars_1 = vars_1.clone();let gen_minus_params_1 = gen_minus_params_1.clone();let body_3 = body_3.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
imports::print(&[module_30.clone(),Scm::from("if args.len() != ")])};{
// (print module (length vars))
imports::print(&[module_30.clone(),{
// (length vars)
imports::length(&[vars_1.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
imports::print(&[module_30.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params vars 0)
gen_minus_params_1.get().invoke(&[vars_1.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body_3.clone().invoke(&[Scm::symbol("gen-rust"),module_30.clone()])}}})}])}}}}}}})});Scm::anything();self__30.set({// Closure
let repr_14 = repr_14.clone();let transform_14 = transform_14.clone();let free_minus_vars_15 = free_minus_vars_15.clone();let gen_minus_rust_14 = gen_minus_rust_14.clone();let vars_1 = vars_1.clone();let body_3 = body_3.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_14 = args[0].clone();let args__25 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_14.clone()])}).is_true() {{
// (repr)
repr_14.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_14.clone()])}).is_true() {{
// (transform (car args))
transform_14.get().invoke(&[{
// (car args)
imports::car(&[args__25.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_14.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_15.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_14.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_14.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_14.get().invoke(&[{
// (car args)
imports::car(&[args__25.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg_14.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars_1.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg_14.clone()])}).is_true() {vars_1.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg_14.clone()])}).is_true() {body_3.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message ABSTRACTION"),msg_14.clone()])}}}})});Scm::anything();self__30.get()}}}}}}}}}}.into()
}
pub fn make_minus_alternative(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let condition_0 = args[0].clone();let consequent_0 = args[1].clone();let alternative_0 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr_6, transform_6, free_minus_vars_6, gen_minus_rust_6, self__22, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__22 = self__22.into_boxed();{let gen_minus_rust_6 = gen_minus_rust_6.into_boxed();{let free_minus_vars_6 = free_minus_vars_6.into_boxed();{let transform_6 = transform_6.into_boxed();{let repr_6 = repr_6.into_boxed();{repr_6.set({// Closure
let condition_0 = condition_0.clone();let consequent_0 = consequent_0.clone();let alternative_0 = alternative_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list(&[Scm::symbol("IF"),{
// (condition (quote repr))
condition_0.clone().invoke(&[Scm::symbol("repr")])},{
// (consequent (quote repr))
consequent_0.clone().invoke(&[Scm::symbol("repr")])},{
// (alternative (quote repr))
alternative_0.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_6.set({// Closure
let self__22 = self__22.clone();let condition_0 = condition_0.clone();let consequent_0 = consequent_0.clone();let alternative_0 = alternative_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_12 = args[0].clone();{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func_12.clone().invoke(&[self__22.get(),{// Closure
let condition_0 = condition_0.clone();let func_12 = func_12.clone();let consequent_0 = consequent_0.clone();let alternative_0 = alternative_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
Scm::func(make_minus_alternative).invoke(&[{
// (condition (quote transform) func)
condition_0.clone().invoke(&[Scm::symbol("transform"),func_12.clone()])},{
// (consequent (quote transform) func)
consequent_0.clone().invoke(&[Scm::symbol("transform"),func_12.clone()])},{
// (alternative (quote transform) func)
alternative_0.clone().invoke(&[Scm::symbol("transform"),func_12.clone()])}])}})}])}})});Scm::anything();free_minus_vars_6.set({// Closure
let condition_0 = condition_0.clone();let consequent_0 = consequent_0.clone();let alternative_0 = alternative_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
imports::set_minus_union(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
imports::set_minus_union(&[{
// (condition (quote free-vars))
condition_0.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent_0.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative_0.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_6.set({// Closure
let condition_0 = condition_0.clone();let consequent_0 = consequent_0.clone();let alternative_0 = alternative_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_19 = args[0].clone();{{
// (print module "if (")
imports::print(&[module_19.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition_0.clone().invoke(&[Scm::symbol("gen-rust"),module_19.clone()])};{
// (print module ").is_true() {")
imports::print(&[module_19.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent_0.clone().invoke(&[Scm::symbol("gen-rust"),module_19.clone()])};{
// (print module "} else ")
imports::print(&[module_19.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
imports::eq_p(&[{
// (alternative (quote kind))
alternative_0.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative_0.clone().invoke(&[Scm::symbol("gen-rust"),module_19.clone()])}} else {{{
// (print module "{")
imports::print(&[module_19.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative_0.clone().invoke(&[Scm::symbol("gen-rust"),module_19.clone()])};{
// (print module "}")
imports::print(&[module_19.clone(),Scm::from("}")])}}}}})});Scm::anything();self__22.set({// Closure
let repr_6 = repr_6.clone();let transform_6 = transform_6.clone();let free_minus_vars_6 = free_minus_vars_6.clone();let gen_minus_rust_6 = gen_minus_rust_6.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_6 = args[0].clone();let args__15 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_6.clone()])}).is_true() {{
// (repr)
repr_6.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_6.clone()])}).is_true() {{
// (transform (car args))
transform_6.get().invoke(&[{
// (car args)
imports::car(&[args__15.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_6.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_6.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_6.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_6.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_6.get().invoke(&[{
// (car args)
imports::car(&[args__15.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
imports::error(&[Scm::from("Unknown message ALTERNATIVE"),msg_6.clone()])}}}})});Scm::anything();self__22.get()}}}}}}}}}}.into()
}
pub fn make_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let func_14 = args[0].clone();let args__17 = args[1].clone();let tail_p_0 = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr_8, transform_8, free_minus_vars_8, gen_minus_rust_8, self__24, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__24 = self__24.into_boxed();{let gen_minus_rust_8 = gen_minus_rust_8.into_boxed();{let free_minus_vars_8 = free_minus_vars_8.into_boxed();{let transform_8 = transform_8.into_boxed();{let repr_8 = repr_8.into_boxed();{repr_8.set({// Closure
let tail_p_0 = tail_p_0.clone();let func_14 = func_14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons(&[if (tail_p_0.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
imports::cons(&[{
// (func (quote repr))
func_14.clone().invoke(&[Scm::symbol("repr")])},{
// (args (quote repr))
args__17.clone().invoke(&[Scm::symbol("repr")])}])}])}})});Scm::anything();transform_8.set({// Closure
let self__24 = self__24.clone();let func_14 = func_14.clone();let args__17 = args__17.clone();let tail_p_0 = tail_p_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc_0 = args[0].clone();{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc_0.clone().invoke(&[self__24.get(),{// Closure
let func_14 = func_14.clone();let fnc_0 = fnc_0.clone();let args__17 = args__17.clone();let tail_p_0 = tail_p_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
Scm::func(make_minus_application).invoke(&[{
// (func (quote transform) fnc)
func_14.clone().invoke(&[Scm::symbol("transform"),fnc_0.clone()])},{
// (args (quote transform) fnc)
args__17.clone().invoke(&[Scm::symbol("transform"),fnc_0.clone()])},tail_p_0.clone()])}})}])}})});Scm::anything();free_minus_vars_8.set({// Closure
let func_14 = func_14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
imports::set_minus_union(&[{
// (func (quote free-vars))
func_14.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args__17.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_8.set({// Closure
let func_14 = func_14.clone();let args__17 = args__17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_22 = args[0].clone();{{
// (func (quote gen-rust) module)
func_14.clone().invoke(&[Scm::symbol("gen-rust"),module_22.clone()])};{
// (print module ".invoke(&[")
imports::print(&[module_22.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args__17.clone().invoke(&[Scm::symbol("gen-rust"),module_22.clone()])};{
// (print module "])")
imports::print(&[module_22.clone(),Scm::from("])")])}}})});Scm::anything();self__24.set({// Closure
let repr_8 = repr_8.clone();let transform_8 = transform_8.clone();let free_minus_vars_8 = free_minus_vars_8.clone();let gen_minus_rust_8 = gen_minus_rust_8.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_8 = args[0].clone();let args__18 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_8.clone()])}).is_true() {{
// (repr)
repr_8.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_8.clone()])}).is_true() {{
// (transform (car args))
transform_8.get().invoke(&[{
// (car args)
imports::car(&[args__18.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_8.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_8.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_8.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_8.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_8.get().invoke(&[{
// (car args)
imports::car(&[args__18.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
imports::error(&[Scm::from("Unknown message APPLICATION"),msg_8.clone()])}}}})});Scm::anything();self__24.get()}}}}}}}}}}.into()
}
pub fn make_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let arg_0 = args[0].clone();
        let next_1 = args[1].clone();
        {
            // (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote ARG) (cons arg next)))) (set! transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (set! gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))) self))
                {
                    let [repr_11, transform_11, free_minus_vars_11, gen_minus_rust_11, self__27] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__27 = self__27.into_boxed();
                        {
                            let gen_minus_rust_11 = gen_minus_rust_11.into_boxed();
                            {
                                let free_minus_vars_11 = free_minus_vars_11.into_boxed();
                                {
                                    let transform_11 = transform_11.into_boxed();
                                    {
                                        let repr_11 = repr_11.into_boxed();
                                        {
                                            repr_11.set({
                                                // Closure
                                                let arg_0 = arg_0.clone();
                                                let next_1 = next_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (cons (quote ARG) (cons arg next))
                                                        imports::cons(&[Scm::symbol("ARG"), {
                                                            // (cons arg next)
                                                            imports::cons(&[
                                                                arg_0.clone(),
                                                                next_1.clone(),
                                                            ])
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            transform_11.set({
                                                // Closure
                                                let self__27 = self__27.clone();
                                                let arg_0 = arg_0.clone();
                                                let next_1 = next_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let fnc_3 = args[0].clone();
                                                    {
                                                        // (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
                                                        fnc_3.clone().invoke(&[self__27.get(), {
                                                            // Closure
                                                            let arg_0 = arg_0.clone();
                                                            let fnc_3 = fnc_3.clone();
                                                            let next_1 = next_1.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                {
                                                                    // (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
                                                                    Scm::func(make_minus_args)
                                                                        .invoke(&[
                                                                            {
                                                                                // (arg (quote transform) fnc)
                                                                                arg_0
                                                                                    .clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    fnc_3.clone(),
                                                                                ])
                                                                            },
                                                                            {
                                                                                // (next (quote transform) fnc)
                                                                                next_1
                                                                                    .clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    fnc_3.clone(),
                                                                                ])
                                                                            },
                                                                        ])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars_11.set({
                                                // Closure
                                                let arg_0 = arg_0.clone();
                                                let next_1 = next_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (set-union (arg (quote free-vars)) (next (quote free-vars)))
                                                        imports::set_minus_union(&[
                                                            {
                                                                // (arg (quote free-vars))
                                                                arg_0.clone().invoke(&[
                                                                    Scm::symbol("free-vars"),
                                                                ])
                                                            },
                                                            {
                                                                // (next (quote free-vars))
                                                                next_1.clone().invoke(&[
                                                                    Scm::symbol("free-vars"),
                                                                ])
                                                            },
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            gen_minus_rust_11.set({
                                                // Closure
                                                let arg_0 = arg_0.clone();
                                                let next_1 = next_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module_25 = args[0].clone();
                                                    {
                                                        {
                                                            // (arg (quote gen-rust) module)
                                                            arg_0.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module_25.clone(),
                                                            ])
                                                        };
                                                        if ({
                                                            // (not (eq? (quote NULL-ARG) (next (quote kind))))
                                                            imports::not(&[{
                                                                // (eq? (quote NULL-ARG) (next (quote kind)))
                                                                imports::eq_p(&[
                                                                    Scm::symbol("NULL-ARG"),
                                                                    {
                                                                        // (next (quote kind))
                                                                        next_1.clone().invoke(&[
                                                                            Scm::symbol("kind"),
                                                                        ])
                                                                    },
                                                                ])
                                                            }])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                {
                                                                    // (print module ",")
                                                                    imports::print(&[
                                                                        module_25.clone(),
                                                                        Scm::from(","),
                                                                    ])
                                                                };
                                                                {
                                                                    // (next (quote gen-rust) module)
                                                                    next_1.clone().invoke(&[
                                                                        Scm::symbol("gen-rust"),
                                                                        module_25.clone(),
                                                                    ])
                                                                }
                                                            }
                                                        } else {
                                                            Scm::symbol("*UNSPECIFIED*")
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__27.set({
                                                // Closure
                                                let repr_11 = repr_11.clone();
                                                let transform_11 = transform_11.clone();
                                                let free_minus_vars_11 = free_minus_vars_11.clone();
                                                let gen_minus_rust_11 = gen_minus_rust_11.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg_11 = args[0].clone();
                                                    let args__22 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg_11.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr_11.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg_11.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform_11.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__22.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg_11.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars_11.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg_11.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg_11.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust_11.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__22.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ARG" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message ARG",
                                                                    ),
                                                                    msg_11.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__27.get()
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
pub fn make_minus_assert(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let condition_1 = args[0].clone();
        {
            // (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ASSERT) condition))) (set! transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (set! free-vars (lambda () (condition (quote free-vars)))) (set! gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))) self))
                {
                    let [repr_24, transform_24, free_minus_vars_22, gen_minus_rust_24, self__40] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__40 = self__40.into_boxed();
                        {
                            let gen_minus_rust_24 = gen_minus_rust_24.into_boxed();
                            {
                                let free_minus_vars_22 = free_minus_vars_22.into_boxed();
                                {
                                    let transform_24 = transform_24.into_boxed();
                                    {
                                        let repr_24 = repr_24.into_boxed();
                                        {
                                            repr_24.set({
                                                // Closure
                                                let condition_1 = condition_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (list (quote ASSERT) condition)
                                                        imports::list(&[
                                                            Scm::symbol("ASSERT"),
                                                            condition_1.clone(),
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            transform_24.set({
                                                // Closure
                                                let self__40 = self__40.clone();
                                                let condition_1 = condition_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func_26 = args[0].clone();
                                                    {
                                                        // (func self (lambda () (make-assert (condition (quote transform) func))))
                                                        func_26.clone().invoke(&[self__40.get(), {
                                                            // Closure
                                                            let condition_1 = condition_1.clone();
                                                            let func_26 = func_26.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                {
                                                                    // (make-assert (condition (quote transform) func))
                                                                    Scm::func(make_minus_assert)
                                                                        .invoke(&[{
                                                                            // (condition (quote transform) func)
                                                                            condition_1
                                                                                .clone()
                                                                                .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    func_26.clone(),
                                                                                ])
                                                                        }])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars_22.set({
                                                // Closure
                                                let condition_1 = condition_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (condition (quote free-vars))
                                                        condition_1
                                                            .clone()
                                                            .invoke(&[Scm::symbol("free-vars")])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            gen_minus_rust_24.set({
                                                // Closure
                                                let condition_1 = condition_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module_45 = args[0].clone();
                                                    {
                                                        {
                                                            // (print module "assert!(")
                                                            imports::print(&[
                                                                module_45.clone(),
                                                                Scm::from("assert!("),
                                                            ])
                                                        };
                                                        {
                                                            // (condition (quote gen-rust) module)
                                                            condition_1.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module_45.clone(),
                                                            ])
                                                        };
                                                        {
                                                            // (println module ".is_true());")
                                                            imports::println(&[
                                                                module_45.clone(),
                                                                Scm::from(".is_true());"),
                                                            ])
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__40.set({
                                                // Closure
                                                let repr_24 = repr_24.clone();
                                                let transform_24 = transform_24.clone();
                                                let free_minus_vars_22 = free_minus_vars_22.clone();
                                                let gen_minus_rust_24 = gen_minus_rust_24.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg_24 = args[0].clone();
                                                    let args__35 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg_24.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr_24.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg_24.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform_24.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__35.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg_24.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars_22.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg_24.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ASSERT")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg_24.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust_24.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__35.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ASSERT" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message ASSERT",
                                                                    ),
                                                                    msg_24.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__40.get()
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
pub fn make_minus_assignment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var_16 = args[0].clone();let val_4 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module name ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module name ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr_4, transform_4, free_minus_vars_4, gen_minus_rust_4, self__20, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__20 = self__20.into_boxed();{let gen_minus_rust_4 = gen_minus_rust_4.into_boxed();{let free_minus_vars_4 = free_minus_vars_4.into_boxed();{let transform_4 = transform_4.into_boxed();{let repr_4 = repr_4.into_boxed();{repr_4.set({// Closure
let var_16 = var_16.clone();let val_4 = val_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("SET!"),{
// (variable-name var)
imports::variable_minus_name(&[var_16.clone()])},{
// (val (quote repr))
val_4.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_4.set({// Closure
let self__20 = self__20.clone();let var_16 = var_16.clone();let val_4 = val_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_10 = args[0].clone();{
// (func self (lambda () (make-assignment var (val (quote transform) func))))
func_10.clone().invoke(&[self__20.get(),{// Closure
let var_16 = var_16.clone();let val_4 = val_4.clone();let func_10 = func_10.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment var (val (quote transform) func))
Scm::func(make_minus_assignment).invoke(&[var_16.clone(),{
// (val (quote transform) func)
val_4.clone().invoke(&[Scm::symbol("transform"),func_10.clone()])}])}})}])}})});Scm::anything();free_minus_vars_4.set({// Closure
let var_16 = var_16.clone();let val_4 = val_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var_16.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var_16.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var_16.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val_4.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var_16.clone()])}])}])}}})});Scm::anything();gen_minus_rust_4.set({// Closure
let var_16 = var_16.clone();let val_4 = val_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_17 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module name ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var))))
{let name_32 = {
// (variable-name var)
imports::variable_minus_name(&[var_16.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var_16.clone()])}).is_true() {{{
// (print module name ".with(|value| value.set(")
imports::print(&[module_17.clone(),name_32.clone(),Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val_4.clone().invoke(&[Scm::symbol("gen-rust"),module_17.clone()])};{
// (print module "));")
imports::print(&[module_17.clone(),Scm::from("));")])}}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var_16.clone()])}).is_true() {{{
// (print module name ".set(")
imports::print(&[module_17.clone(),name_32.clone(),Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val_4.clone().invoke(&[Scm::symbol("gen-rust"),module_17.clone()])};{
// (print module ");")
imports::print(&[module_17.clone(),Scm::from(");")])}}} else {{
// (error "set! on unboxed variable" name var)
imports::error(&[Scm::from("set! on unboxed variable"),name_32.clone(),var_16.clone()])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module_17.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__20.set({// Closure
let repr_4 = repr_4.clone();let transform_4 = transform_4.clone();let free_minus_vars_4 = free_minus_vars_4.clone();let gen_minus_rust_4 = gen_minus_rust_4.clone();let var_16 = var_16.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_4 = args[0].clone();let args__13 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_4.clone()])}).is_true() {{
// (repr)
repr_4.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_4.clone()])}).is_true() {{
// (transform (car args))
transform_4.get().invoke(&[{
// (car args)
imports::car(&[args__13.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_4.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_4.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_4.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_4.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_4.get().invoke(&[{
// (car args)
imports::car(&[args__13.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg_4.clone()])}).is_true() {var_16.clone()} else {{
// (error "Unknown message ASSIGNMENT" msg)
imports::error(&[Scm::from("Unknown message ASSIGNMENT"),msg_4.clone()])}}}})});Scm::anything();self__20.get()}}}}}}}}}}.into()
}
pub fn make_minus_boxify(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var_19 = args[0].clone();let body_8 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote BOXIFY) var (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-boxify var (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (variable-name var)) (print module " = ") (print module (variable-name var)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote BOXIFY) var (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-boxify var (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (variable-name var)) (print module " = ") (print module (variable-name var)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr_18, transform_18, free_minus_vars_17, gen_minus_rust_18, self__34, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__34 = self__34.into_boxed();{let gen_minus_rust_18 = gen_minus_rust_18.into_boxed();{let free_minus_vars_17 = free_minus_vars_17.into_boxed();{let transform_18 = transform_18.into_boxed();{let repr_18 = repr_18.into_boxed();{repr_18.set({// Closure
let var_19 = var_19.clone();let body_8 = body_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote BOXIFY) var (body (quote repr)))
imports::list(&[Scm::symbol("BOXIFY"),var_19.clone(),{
// (body (quote repr))
body_8.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_18.set({// Closure
let self__34 = self__34.clone();let var_19 = var_19.clone();let body_8 = body_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_20 = args[0].clone();{
// (func self (lambda () (make-boxify var (body (quote transform) func))))
func_20.clone().invoke(&[self__34.get(),{// Closure
let var_19 = var_19.clone();let body_8 = body_8.clone();let func_20 = func_20.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-boxify var (body (quote transform) func))
Scm::func(make_minus_boxify).invoke(&[var_19.clone(),{
// (body (quote transform) func)
body_8.clone().invoke(&[Scm::symbol("transform"),func_20.clone()])}])}})}])}})});Scm::anything();free_minus_vars_17.set({// Closure
let body_8 = body_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (body (quote free-vars))
body_8.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust_18.set({// Closure
let var_19 = var_19.clone();let body_8 = body_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_36 = args[0].clone();{
// (rust-block module (lambda () (print module "let ") (print module (variable-name var)) (print module " = ") (print module (variable-name var)) (print module ".into_boxed();") (body (quote gen-rust) module)))
imports::rust_minus_block(&[module_36.clone(),{// Closure
let module_36 = module_36.clone();let var_19 = var_19.clone();let body_8 = body_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
imports::print(&[module_36.clone(),Scm::from("let ")])};{
// (print module (variable-name var))
imports::print(&[module_36.clone(),{
// (variable-name var)
imports::variable_minus_name(&[var_19.clone()])}])};{
// (print module " = ")
imports::print(&[module_36.clone(),Scm::from(" = ")])};{
// (print module (variable-name var))
imports::print(&[module_36.clone(),{
// (variable-name var)
imports::variable_minus_name(&[var_19.clone()])}])};{
// (print module ".into_boxed();")
imports::print(&[module_36.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body_8.clone().invoke(&[Scm::symbol("gen-rust"),module_36.clone()])}}})}])}})});Scm::anything();self__34.set({// Closure
let repr_18 = repr_18.clone();let transform_18 = transform_18.clone();let free_minus_vars_17 = free_minus_vars_17.clone();let gen_minus_rust_18 = gen_minus_rust_18.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_18 = args[0].clone();let args__29 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_18.clone()])}).is_true() {{
// (repr)
repr_18.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_18.clone()])}).is_true() {{
// (transform (car args))
transform_18.get().invoke(&[{
// (car args)
imports::car(&[args__29.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_18.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_17.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_18.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_18.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_18.get().invoke(&[{
// (car args)
imports::car(&[args__29.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
imports::error(&[Scm::from("Unknown message BOXIFY"),msg_18.clone()])}}}})});Scm::anything();self__34.get()}}}}}}}}}}.into()
}
pub fn make_minus_closure(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let function_0 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module name) (print module " = ") (print module name) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module name) (print module " = ") (print module name) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr_13, transform_13, free_minus_vars_13, prepare_minus_closure_0, gen_minus_rust_13, self__29, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__29 = self__29.into_boxed();{let gen_minus_rust_13 = gen_minus_rust_13.into_boxed();{let prepare_minus_closure_0 = prepare_minus_closure_0.into_boxed();{let free_minus_vars_13 = free_minus_vars_13.into_boxed();{let transform_13 = transform_13.into_boxed();{let repr_13 = repr_13.into_boxed();{repr_13.set({// Closure
let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
imports::list(&[Scm::symbol("CLOSURE"),function_0.clone()])}})});Scm::anything();transform_13.set({// Closure
let self__29 = self__29.clone();let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_15 = args[0].clone();{
// (func self (lambda () (make-closure (function (quote transform) func))))
func_15.clone().invoke(&[self__29.get(),{// Closure
let function_0 = function_0.clone();let func_15 = func_15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-closure (function (quote transform) func))
Scm::func(make_minus_closure).invoke(&[{
// (function (quote transform) func)
function_0.clone().invoke(&[Scm::symbol("transform"),func_15.clone()])}])}})}])}})});Scm::anything();free_minus_vars_13.set({// Closure
let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (function (quote free-vars))
function_0.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();prepare_minus_closure_0.set({// Closure
let prepare_minus_closure_0 = prepare_minus_closure_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_28 = args[0].clone();let free_minus_vars_14 = args[1].clone();if ({
// (pair? free-vars)
imports::pair_p(&[free_minus_vars_14.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module name) (print module " = ") (print module name) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name_34 = {
// (car free-vars)
imports::car(&[free_minus_vars_14.clone()])};{{
// (print module "let ")
imports::print(&[module_28.clone(),Scm::from("let ")])};{
// (print module name)
imports::print(&[module_28.clone(),name_34.clone()])};{
// (print module " = ")
imports::print(&[module_28.clone(),Scm::from(" = ")])};{
// (print module name)
imports::print(&[module_28.clone(),name_34.clone()])};{
// (print module ".clone();")
imports::print(&[module_28.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure_0.get().invoke(&[module_28.clone(),{
// (cdr free-vars)
imports::cdr(&[free_minus_vars_14.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();gen_minus_rust_13.set({// Closure
let prepare_minus_closure_0 = prepare_minus_closure_0.clone();let free_minus_vars_13 = free_minus_vars_13.clone();let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_29 = args[0].clone();{
// (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
imports::rust_minus_block(&[module_29.clone(),{// Closure
let module_29 = module_29.clone();let prepare_minus_closure_0 = prepare_minus_closure_0.clone();let free_minus_vars_13 = free_minus_vars_13.clone();let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "// Closure")
imports::println(&[module_29.clone(),Scm::from("// Closure")])};{
// (prepare-closure module (free-vars))
prepare_minus_closure_0.get().invoke(&[module_29.clone(),{
// (free-vars)
free_minus_vars_13.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
imports::print(&[module_29.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function_0.clone().invoke(&[Scm::symbol("gen-rust"),module_29.clone()])};{
// (print module ")")
imports::print(&[module_29.clone(),Scm::from(")")])}}})}])}})});Scm::anything();self__29.set({// Closure
let repr_13 = repr_13.clone();let transform_13 = transform_13.clone();let free_minus_vars_13 = free_minus_vars_13.clone();let gen_minus_rust_13 = gen_minus_rust_13.clone();let function_0 = function_0.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_13 = args[0].clone();let args__24 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_13.clone()])}).is_true() {{
// (repr)
repr_13.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_13.clone()])}).is_true() {{
// (transform (car args))
transform_13.get().invoke(&[{
// (car args)
imports::car(&[args__24.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_13.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_13.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_13.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_13.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_13.get().invoke(&[{
// (car args)
imports::car(&[args__24.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
imports::eq_p(&[Scm::symbol("inner-function"),msg_13.clone()])}).is_true() {function_0.clone()} else {{
// (error "Unknown message CLOSURE" msg)
imports::error(&[Scm::from("Unknown message CLOSURE"),msg_13.clone()])}}}})});Scm::anything();self__29.get()}}}}}}}}}}}.into()
}
pub fn make_minus_comment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let comment_0 = args[0].clone();let node_8 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr_0, transform_0, free_minus_vars_0, gen_minus_rust_0, gen_minus_rust_minus_inner_0, self__16, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__16 = self__16.into_boxed();{let gen_minus_rust_minus_inner_0 = gen_minus_rust_minus_inner_0.into_boxed();{let gen_minus_rust_0 = gen_minus_rust_0.into_boxed();{let free_minus_vars_0 = free_minus_vars_0.into_boxed();{let transform_0 = transform_0.into_boxed();{let repr_0 = repr_0.into_boxed();{repr_0.set({// Closure
let comment_0 = comment_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
imports::list(&[Scm::symbol("COMMENT"),comment_0.clone(),{
// (node (quote repr))
node_8.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_0.set({// Closure
let self__16 = self__16.clone();let comment_0 = comment_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_6 = args[0].clone();{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func_6.clone().invoke(&[self__16.get(),{// Closure
let comment_0 = comment_0.clone();let node_8 = node_8.clone();let func_6 = func_6.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-comment comment (node (quote transform) func))
Scm::func(make_minus_comment).invoke(&[comment_0.clone(),{
// (node (quote transform) func)
node_8.clone().invoke(&[Scm::symbol("transform"),func_6.clone()])}])}})}])}})});Scm::anything();free_minus_vars_0.set({// Closure
let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (node (quote free-vars))
node_8.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust_0.set({// Closure
let comment_0 = comment_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_11 = args[0].clone();{
// (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)))
imports::rust_minus_block(&[module_11.clone(),{// Closure
let module_11 = module_11.clone();let comment_0 = comment_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module_11.clone()])};{
// (print module "// ")
imports::print(&[module_11.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module_11.clone(),comment_0.clone()])};{
// (node (quote gen-rust) module)
node_8.clone().invoke(&[Scm::symbol("gen-rust"),module_11.clone()])}}})}])}})});Scm::anything();gen_minus_rust_minus_inner_0.set({// Closure
let comment_0 = comment_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_12 = args[0].clone();{{
// (println module)
imports::println(&[module_12.clone()])};{
// (print module "// ")
imports::print(&[module_12.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module_12.clone(),comment_0.clone()])};{
// (node (quote gen-rust-inner) module)
node_8.clone().invoke(&[Scm::symbol("gen-rust-inner"),module_12.clone()])}}})});Scm::anything();self__16.set({// Closure
let repr_0 = repr_0.clone();let transform_0 = transform_0.clone();let free_minus_vars_0 = free_minus_vars_0.clone();let gen_minus_rust_0 = gen_minus_rust_0.clone();let gen_minus_rust_minus_inner_0 = gen_minus_rust_minus_inner_0.clone();let node_8 = node_8.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_0 = args[0].clone();let args__9 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_0.clone()])}).is_true() {{
// (repr)
repr_0.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_0.clone()])}).is_true() {{
// (transform (car args))
transform_0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_0.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_0.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_0.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_0.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg_0.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner_0.get().invoke(&[{
// (car args)
imports::car(&[args__9.clone()])}])}} else if ({
// (eq? (quote inner) msg)
imports::eq_p(&[Scm::symbol("inner"),msg_0.clone()])}).is_true() {node_8.clone()} else {{
// (error "Unknown message COMMENT" msg)
imports::error(&[Scm::from("Unknown message COMMENT"),msg_0.clone()])}}}})});Scm::anything();self__16.get()}}}}}}}}}}}.into()
}
pub fn make_minus_constant(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let val_2 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr_2, transform_2, free_minus_vars_2, gen_minus_constant_0, gen_minus_rust_2, self__18, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__18 = self__18.into_boxed();{let gen_minus_rust_2 = gen_minus_rust_2.into_boxed();{let gen_minus_constant_0 = gen_minus_constant_0.into_boxed();{let free_minus_vars_2 = free_minus_vars_2.into_boxed();{let transform_2 = transform_2.into_boxed();{let repr_2 = repr_2.into_boxed();{repr_2.set({// Closure
let val_2 = val_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
imports::cons(&[Scm::symbol("CONSTANT"),val_2.clone()])}})});Scm::anything();transform_2.set({// Closure
let self__18 = self__18.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_8 = args[0].clone();{
// (func self (lambda () self))
func_8.clone().invoke(&[self__18.get(),{// Closure
let self__18 = self__18.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__18.get()})}])}})});Scm::anything();free_minus_vars_2.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_constant_0.set({// Closure
let gen_minus_constant_0 = gen_minus_constant_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_14 = args[0].clone();let val_3 = args[1].clone();{
// (cond ...)
if ({
// (null? val)
imports::null_p(&[val_3.clone()])}).is_true() {{
// (print module "Scm::Nil")
imports::print(&[module_14.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
imports::eq_p(&[val_3.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
imports::print(&[module_14.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
imports::eq_p(&[val_3.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
imports::print(&[module_14.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
imports::symbol_p(&[val_3.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
imports::print(&[module_14.clone(),Scm::from("Scm::symbol(\""),val_3.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
imports::eq_p(&[val_3.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
imports::print(&[module_14.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
imports::char_p(&[val_3.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
imports::print(&[module_14.clone(),Scm::from("Scm::char('"),val_3.clone(),Scm::from("')")])}} else if ({
// (pair? val)
imports::pair_p(&[val_3.clone()])}).is_true() {{{
// (print module "Scm::pair(")
imports::print(&[module_14.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant_0.get().invoke(&[module_14.clone(),{
// (car val)
imports::car(&[val_3.clone()])}])};{
// (print module ", ")
imports::print(&[module_14.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant_0.get().invoke(&[module_14.clone(),{
// (cdr val)
imports::cdr(&[val_3.clone()])}])};{
// (print module ")")
imports::print(&[module_14.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
imports::print(&[module_14.clone(),Scm::from("Scm::from(")])};{
// (show module val)
imports::show(&[module_14.clone(),val_3.clone()])};{
// (print module ")")
imports::print(&[module_14.clone(),Scm::from(")")])}}}}})});Scm::anything();gen_minus_rust_2.set({// Closure
let gen_minus_constant_0 = gen_minus_constant_0.clone();let val_2 = val_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_15 = args[0].clone();{
// (gen-constant module val)
gen_minus_constant_0.get().invoke(&[module_15.clone(),val_2.clone()])}})});Scm::anything();self__18.set({// Closure
let repr_2 = repr_2.clone();let transform_2 = transform_2.clone();let free_minus_vars_2 = free_minus_vars_2.clone();let gen_minus_rust_2 = gen_minus_rust_2.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_2 = args[0].clone();let args__11 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_2.clone()])}).is_true() {{
// (repr)
repr_2.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_2.clone()])}).is_true() {{
// (transform (car args))
transform_2.get().invoke(&[{
// (car args)
imports::car(&[args__11.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_2.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_2.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_2.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_2.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_2.get().invoke(&[{
// (car args)
imports::car(&[args__11.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
imports::error(&[Scm::from("Unknown message CONSTANT"),msg_2.clone()])}}}})});Scm::anything();self__18.get()}}}}}}}}}}}.into()
}
pub fn make_minus_definition(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var_17 = args[0].clone();let val_5 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr_5, transform_5, free_minus_vars_5, gen_minus_rust_5, self__21, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__21 = self__21.into_boxed();{let gen_minus_rust_5 = gen_minus_rust_5.into_boxed();{let free_minus_vars_5 = free_minus_vars_5.into_boxed();{let transform_5 = transform_5.into_boxed();{let repr_5 = repr_5.into_boxed();{repr_5.set({// Closure
let var_17 = var_17.clone();let val_5 = val_5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("DEFINE"),{
// (variable-name var)
imports::variable_minus_name(&[var_17.clone()])},{
// (val (quote repr))
val_5.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_5.set({// Closure
let self__21 = self__21.clone();let var_17 = var_17.clone();let val_5 = val_5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_11 = args[0].clone();{
// (func self (lambda () (make-definition var (val (quote transform) func))))
func_11.clone().invoke(&[self__21.get(),{// Closure
let var_17 = var_17.clone();let val_5 = val_5.clone();let func_11 = func_11.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition var (val (quote transform) func))
Scm::func(make_minus_definition).invoke(&[var_17.clone(),{
// (val (quote transform) func)
val_5.clone().invoke(&[Scm::symbol("transform"),func_11.clone()])}])}})}])}})});Scm::anything();free_minus_vars_5.set({// Closure
let val_5 = val_5.clone();let var_17 = var_17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val_5.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var_17.clone()])}])}])}})});Scm::anything();gen_minus_rust_5.set({// Closure
let var_17 = var_17.clone();let val_5 = val_5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_18 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module name ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable"))))
{let name_33 = {
// (variable-name var)
imports::variable_minus_name(&[var_17.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var_17.clone()])}).is_true() {{{
// (print module name ".with(|value| value.set(")
imports::print(&[module_18.clone(),name_33.clone(),Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val_5.clone().invoke(&[Scm::symbol("gen-rust"),module_18.clone()])};{
// (print module "));")
imports::print(&[module_18.clone(),Scm::from("));")])}}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var_17.clone()])}).is_true() {{{
// (print module name ".with(|value| value.set(")
imports::print(&[module_18.clone(),name_33.clone(),Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val_5.clone().invoke(&[Scm::symbol("gen-rust"),module_18.clone()])};{
// (print module "));")
imports::print(&[module_18.clone(),Scm::from("));")])}}} else {{
// (error "definition! of non-global variable")
imports::error(&[Scm::from("definition! of non-global variable")])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module_18.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__21.set({// Closure
let repr_5 = repr_5.clone();let transform_5 = transform_5.clone();let free_minus_vars_5 = free_minus_vars_5.clone();let gen_minus_rust_5 = gen_minus_rust_5.clone();let var_17 = var_17.clone();let val_5 = val_5.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_5 = args[0].clone();let args__14 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_5.clone()])}).is_true() {{
// (repr)
repr_5.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_5.clone()])}).is_true() {{
// (transform (car args))
transform_5.get().invoke(&[{
// (car args)
imports::car(&[args__14.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_5.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_5.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_5.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_5.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_5.get().invoke(&[{
// (car args)
imports::car(&[args__14.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg_5.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var_17.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg_5.clone()])}).is_true() {var_17.clone()} else if ({
// (eq? (quote get-val) msg)
imports::eq_p(&[Scm::symbol("get-val"),msg_5.clone()])}).is_true() {val_5.clone()} else {{
// (error "Unknown message DEFINITION" msg)
imports::error(&[Scm::from("Unknown message DEFINITION"),msg_5.clone()])}}}})});Scm::anything();self__21.get()}}}}}}}}}}.into()
}
pub fn make_minus_export(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var_20 = args[0].clone();let exname_0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote EXPORT) var (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module name " as " (rustify-identifier exname) ";")))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message EXPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) var (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module name " as " (rustify-identifier exname) ";")))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message EXPORT" msg))))) self))
{let [repr_19, transform_19, gen_minus_rust_19, self__35, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__35 = self__35.into_boxed();{let gen_minus_rust_19 = gen_minus_rust_19.into_boxed();{let transform_19 = transform_19.into_boxed();{let repr_19 = repr_19.into_boxed();{repr_19.set({// Closure
let var_20 = var_20.clone();let exname_0 = exname_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote EXPORT) var (quote AS) exname)
imports::list(&[Scm::symbol("EXPORT"),var_20.clone(),Scm::symbol("AS"),exname_0.clone()])}})});Scm::anything();transform_19.set({// Closure
let self__35 = self__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_21 = args[0].clone();{
// (func self (lambda () self))
func_21.clone().invoke(&[self__35.get(),{// Closure
let self__35 = self__35.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__35.get()})}])}})});Scm::anything();gen_minus_rust_19.set({// Closure
let var_20 = var_20.clone();let exname_0 = exname_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_37 = args[0].clone();{{
// (print module "pub use super::")
imports::print(&[module_37.clone(),Scm::from("pub use super::")])};{
// (let* ((name (variable-name var))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module name " as " (rustify-identifier exname) ";"))
{
// (let ((name (variable-name var))) (begin (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))) (println module name " as " (rustify-identifier exname) ";")))
{let name_37 = {
// (variable-name var)
imports::variable_minus_name(&[var_20.clone()])};{{
// (cond ...)
if ({
// (not var)
imports::not(&[var_20.clone()])}).is_true() {{
// (error "undefined export" name)
imports::error(&[Scm::from("undefined export"),name_37.clone()])}} else if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var_20.clone()])}).is_true() {{
// (print module "")
imports::print(&[module_37.clone(),Scm::from("")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var_20.clone()])}).is_true() {{
// (print module "")
imports::print(&[module_37.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var_20.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module_37.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
imports::error(&[Scm::from("invalid export variable"),var_20.clone(),name_37.clone()])}}};{
// (println module name " as " (rustify-identifier exname) ";")
imports::println(&[module_37.clone(),name_37.clone(),Scm::from(" as "),{
// (rustify-identifier exname)
imports::rustify_minus_identifier(&[exname_0.clone()])},Scm::from(";")])}}}}}}})});Scm::anything();self__35.set({// Closure
let repr_19 = repr_19.clone();let transform_19 = transform_19.clone();let gen_minus_rust_19 = gen_minus_rust_19.clone();let var_20 = var_20.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_19 = args[0].clone();let args__30 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_19.clone()])}).is_true() {{
// (repr)
repr_19.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_19.clone()])}).is_true() {{
// (transform (car args))
transform_19.get().invoke(&[{
// (car args)
imports::car(&[args__30.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_19.clone()])}).is_true() {Scm::symbol("EXPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_19.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_19.get().invoke(&[{
// (car args)
imports::car(&[args__30.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg_19.clone()])}).is_true() {var_20.clone()} else {{
// (error "Unknown message EXPORT" msg)
imports::error(&[Scm::from("Unknown message EXPORT"),msg_19.clone()])}}}})});Scm::anything();self__35.get()}}}}}}}}}.into()
}
pub fn make_minus_fixlet(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars_0 = args[0].clone();let args__23 = args[1].clone();let body_2 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (variable-name (car v*)) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (variable-name (car vars))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (variable-name (car v*)) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (variable-name (car vars))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr_12, transform_12, free_minus_vars_12, gen_minus_rust_minus_inner_2, gen_minus_rust_12, self__28, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__28 = self__28.into_boxed();{let gen_minus_rust_12 = gen_minus_rust_12.into_boxed();{let gen_minus_rust_minus_inner_2 = gen_minus_rust_minus_inner_2.into_boxed();{let free_minus_vars_12 = free_minus_vars_12.into_boxed();{let transform_12 = transform_12.into_boxed();{let repr_12 = repr_12.into_boxed();{repr_12.set({// Closure
let vars_0 = vars_0.clone();let args__23 = args__23.clone();let body_2 = body_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) vars (args (quote repr)) (body (quote repr)))
imports::list(&[Scm::symbol("FIXLET"),vars_0.clone(),{
// (args (quote repr))
args__23.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body_2.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_12.set({// Closure
let self__28 = self__28.clone();let vars_0 = vars_0.clone();let args__23 = args__23.clone();let body_2 = body_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc_4 = args[0].clone();{
// (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc_4.clone().invoke(&[self__28.get(),{// Closure
let vars_0 = vars_0.clone();let args__23 = args__23.clone();let fnc_4 = fnc_4.clone();let body_2 = body_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))
Scm::func(make_minus_fixlet).invoke(&[vars_0.clone(),{
// (args (quote transform) fnc)
args__23.clone().invoke(&[Scm::symbol("transform"),fnc_4.clone()])},{
// (body (quote transform) fnc)
body_2.clone().invoke(&[Scm::symbol("transform"),fnc_4.clone()])}])}})}])}})});Scm::anything();free_minus_vars_12.set({// Closure
let body_2 = body_2.clone();let vars_0 = vars_0.clone();let args__23 = args__23.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars)))
imports::set_minus_union(&[{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body_2.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars_0.clone()])}])}])},{
// (args (quote free-vars))
args__23.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner_2.set({// Closure
let vars_0 = vars_0.clone();let args__23 = args__23.clone();let body_2 = body_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_26 = args[0].clone();{
// (letrec ((gen-params (lambda (v*) (if (pair? v*) (begin (print module (variable-name (car v*)) ", ") (gen-params (cdr v*))))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (variable-name (car vars))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (v*) (if (pair? v*) (begin (print module (variable-name (car v*)) ", ") (gen-params (cdr v*)))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (variable-name (car vars))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params_0 = Scm::symbol("*uninitialized*");{let gen_minus_params_0 = gen_minus_params_0.into_boxed();{gen_minus_params_0.set({// Closure
let module_26 = module_26.clone();let gen_minus_params_0 = gen_minus_params_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let v_star__0 = args[0].clone();if ({
// (pair? v*)
imports::pair_p(&[v_star__0.clone()])}).is_true() {{{
// (print module (variable-name (car v*)) ", ")
imports::print(&[module_26.clone(),{
// (variable-name (car v*))
imports::variable_minus_name(&[{
// (car v*)
imports::car(&[v_star__0.clone()])}])},Scm::from(", ")])};{
// (gen-params (cdr v*))
gen_minus_params_0.get().invoke(&[{
// (cdr v*)
imports::cdr(&[v_star__0.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (cond ...)
if ({
// (= 0 (length vars))
imports::_e_(&[Scm::from(0),{
// (length vars)
imports::length(&[vars_0.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length vars))
imports::_e_(&[Scm::from(1),{
// (length vars)
imports::length(&[vars_0.clone()])}])}).is_true() {{{
// (print module "let ")
imports::print(&[module_26.clone(),Scm::from("let ")])};{
// (print module (variable-name (car vars)))
imports::print(&[module_26.clone(),{
// (variable-name (car vars))
imports::variable_minus_name(&[{
// (car vars)
imports::car(&[vars_0.clone()])}])}])};{
// (print module " = ")
imports::print(&[module_26.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args__23.clone().invoke(&[Scm::symbol("gen-rust"),module_26.clone()])};{
// (print module ";")
imports::print(&[module_26.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
imports::print(&[module_26.clone(),Scm::from("let [")])};{
// (gen-params vars)
gen_minus_params_0.get().invoke(&[vars_0.clone()])};{
// (print module "] = [")
imports::print(&[module_26.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args__23.clone().invoke(&[Scm::symbol("gen-rust"),module_26.clone()])};{
// (print module "];")
imports::print(&[module_26.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body_2.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body_2.clone().invoke(&[Scm::symbol("gen-rust-inner"),module_26.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
imports::eq_p(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body_2.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// ((body (quote inner)) (quote kind))
{
// (body (quote inner))
body_2.clone().invoke(&[Scm::symbol("inner")])}.invoke(&[Scm::symbol("kind")])}])}} else {Scm::False}}).is_true() {{
// (body (quote gen-rust-inner) module)
body_2.clone().invoke(&[Scm::symbol("gen-rust-inner"),module_26.clone()])}} else {{
// (body (quote gen-rust) module)
body_2.clone().invoke(&[Scm::symbol("gen-rust"),module_26.clone()])}}}}}}}})});Scm::anything();gen_minus_rust_12.set({// Closure
let gen_minus_rust_minus_inner_2 = gen_minus_rust_minus_inner_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_27 = args[0].clone();{
// (rust-block module (lambda () (gen-rust-inner module)))
imports::rust_minus_block(&[module_27.clone(),{// Closure
let gen_minus_rust_minus_inner_2 = gen_minus_rust_minus_inner_2.clone();let module_27 = module_27.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner_2.get().invoke(&[module_27.clone()])}})}])}})});Scm::anything();self__28.set({// Closure
let repr_12 = repr_12.clone();let transform_12 = transform_12.clone();let free_minus_vars_12 = free_minus_vars_12.clone();let vars_0 = vars_0.clone();let args__23 = args__23.clone();let body_2 = body_2.clone();let gen_minus_rust_12 = gen_minus_rust_12.clone();let gen_minus_rust_minus_inner_2 = gen_minus_rust_minus_inner_2.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_12 = args[0].clone();let arg_star__0 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_12.clone()])}).is_true() {{
// (repr)
repr_12.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_12.clone()])}).is_true() {{
// (transform (car arg*))
transform_12.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star__0.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_12.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_12.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_12.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg_12.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars_0.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg_12.clone()])}).is_true() {vars_0.clone()} else if ({
// (eq? (quote get-args) msg)
imports::eq_p(&[Scm::symbol("get-args"),msg_12.clone()])}).is_true() {args__23.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg_12.clone()])}).is_true() {body_2.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_12.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust_12.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star__0.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg_12.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner_2.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star__0.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
imports::error(&[Scm::from("Unknown message FIXLET"),msg_12.clone()])}}}})});Scm::anything();self__28.get()}}}}}}}}}}}.into()
}
pub fn make_minus_function_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let var_18 = args[0].clone();let args__19 = args[1].clone();let tail_p_1 = args[2].clone();{
// (letrec ((repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (free-vars (lambda () (args (quote free-vars)))) (gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (variable-name var) "(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message FN-APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (args (quote free-vars)))) (set! gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (variable-name var) "(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message FN-APPLICATION" msg))))) self))
{let [repr_9, transform_9, free_minus_vars_9, gen_minus_rust_9, self__25, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__25 = self__25.into_boxed();{let gen_minus_rust_9 = gen_minus_rust_9.into_boxed();{let free_minus_vars_9 = free_minus_vars_9.into_boxed();{let transform_9 = transform_9.into_boxed();{let repr_9 = repr_9.into_boxed();{repr_9.set({// Closure
let tail_p_1 = tail_p_1.clone();let var_18 = var_18.clone();let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr)))
imports::list(&[if (tail_p_1.clone()).is_true() {Scm::symbol("FN-APPLY-TC")} else {Scm::symbol("FN-APPLY")},{
// (variable-name var)
imports::variable_minus_name(&[var_18.clone()])},{
// (args (quote repr))
args__19.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_9.set({// Closure
let self__25 = self__25.clone();let var_18 = var_18.clone();let args__19 = args__19.clone();let tail_p_1 = tail_p_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc_1 = args[0].clone();{
// (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?)))
fnc_1.clone().invoke(&[self__25.get(),{// Closure
let var_18 = var_18.clone();let args__19 = args__19.clone();let fnc_1 = fnc_1.clone();let tail_p_1 = tail_p_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-function-application var (args (quote transform) fnc) tail?)
Scm::func(make_minus_function_minus_application).invoke(&[var_18.clone(),{
// (args (quote transform) fnc)
args__19.clone().invoke(&[Scm::symbol("transform"),fnc_1.clone()])},tail_p_1.clone()])}})}])}})});Scm::anything();free_minus_vars_9.set({// Closure
let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (args (quote free-vars))
args__19.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust_9.set({// Closure
let var_18 = var_18.clone();let args__19 = args__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_23 = args[0].clone();{{
// (cond ...)
if ({
// (global-function? var)
imports::global_minus_function_p(&[var_18.clone()])}).is_true() {{
// (print module "")
imports::print(&[module_23.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var_18.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module_23.clone(),Scm::from("imports::")])}} else {{
// (error "invalid function application" var)
imports::error(&[Scm::from("invalid function application"),var_18.clone()])}}};{
// (print module (variable-name var) "(&[")
imports::print(&[module_23.clone(),{
// (variable-name var)
imports::variable_minus_name(&[var_18.clone()])},Scm::from("(&[")])};{
// (args (quote gen-rust) module)
args__19.clone().invoke(&[Scm::symbol("gen-rust"),module_23.clone()])};{
// (print module "])")
imports::print(&[module_23.clone(),Scm::from("])")])}}})});Scm::anything();self__25.set({// Closure
let repr_9 = repr_9.clone();let transform_9 = transform_9.clone();let free_minus_vars_9 = free_minus_vars_9.clone();let gen_minus_rust_9 = gen_minus_rust_9.clone();let var_18 = var_18.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_9 = args[0].clone();let args__20 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_9.clone()])}).is_true() {{
// (repr)
repr_9.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_9.clone()])}).is_true() {{
// (transform (car args))
transform_9.get().invoke(&[{
// (car args)
imports::car(&[args__20.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_9.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_9.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_9.clone()])}).is_true() {Scm::symbol("FN-APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_9.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_9.get().invoke(&[{
// (car args)
imports::car(&[args__20.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg_9.clone()])}).is_true() {var_18.clone()} else {{
// (error "Unknown message FN-APPLICATION" msg)
imports::error(&[Scm::from("Unknown message FN-APPLICATION"),msg_9.clone()])}}}})});Scm::anything();self__25.get()}}}}}}}}}}.into()
}
pub fn make_minus_import(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let lib_4 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr_20, transform_20, free_minus_vars_18, gen_minus_libname_0, gen_minus_rust_20, self__36, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__36 = self__36.into_boxed();{let gen_minus_rust_20 = gen_minus_rust_20.into_boxed();{let gen_minus_libname_0 = gen_minus_libname_0.into_boxed();{let free_minus_vars_18 = free_minus_vars_18.into_boxed();{let transform_20 = transform_20.into_boxed();{let repr_20 = repr_20.into_boxed();{repr_20.set({// Closure
let lib_4 = lib_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
imports::cons(&[Scm::symbol("IMPORT"),lib_4.clone()])}})});Scm::anything();transform_20.set({// Closure
let self__36 = self__36.clone();let lib_4 = lib_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_22 = args[0].clone();{
// (func self (lambda () (make-import lib)))
func_22.clone().invoke(&[self__36.get(),{// Closure
let lib_4 = lib_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
Scm::func(make_minus_import).invoke(&[lib_4.clone()])}})}])}})});Scm::anything();free_minus_vars_18.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname_0.set({// Closure
let gen_minus_libname_0 = gen_minus_libname_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_38 = args[0].clone();let lib_5 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib_5.clone()])}).is_true() {{
// (print module "")
imports::print(&[module_38.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module_38.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib_5.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib_5.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module_38.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module_38.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname_0.get().invoke(&[module_38.clone(),{
// (cdr lib)
imports::cdr(&[lib_5.clone()])}])}}}})});Scm::anything();gen_minus_rust_20.set({// Closure
let gen_minus_libname_0 = gen_minus_libname_0.clone();let lib_4 = lib_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_39 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module_39.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname_0.get().invoke(&[module_39.clone(),lib_4.clone()])};{
// (print module "::exports::*;")
imports::print(&[module_39.clone(),Scm::from("::exports::*;")])};{
// (println module)
imports::println(&[module_39.clone()])}}})});Scm::anything();self__36.set({// Closure
let repr_20 = repr_20.clone();let transform_20 = transform_20.clone();let free_minus_vars_18 = free_minus_vars_18.clone();let gen_minus_rust_20 = gen_minus_rust_20.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_20 = args[0].clone();let args__31 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_20.clone()])}).is_true() {{
// (repr)
repr_20.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_20.clone()])}).is_true() {{
// (transform (car args))
transform_20.get().invoke(&[{
// (car args)
imports::car(&[args__31.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_20.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_18.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_20.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_20.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_20.get().invoke(&[{
// (car args)
imports::car(&[args__31.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg_20.clone()])}}}})});Scm::anything();self__36.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import_minus_only(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let lib_6 = args[0].clone();let names_0 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr_21, transform_21, free_minus_vars_19, gen_minus_libname_1, gen_minus_imports_1, gen_minus_rust_21, self__37, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__37 = self__37.into_boxed();{let gen_minus_rust_21 = gen_minus_rust_21.into_boxed();{let gen_minus_imports_1 = gen_minus_imports_1.into_boxed();{let gen_minus_libname_1 = gen_minus_libname_1.into_boxed();{let free_minus_vars_19 = free_minus_vars_19.into_boxed();{let transform_21 = transform_21.into_boxed();{let repr_21 = repr_21.into_boxed();{repr_21.set({// Closure
let lib_6 = lib_6.clone();let names_0 = names_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
imports::cons(&[lib_6.clone(),names_0.clone()])}])}})});Scm::anything();transform_21.set({// Closure
let self__37 = self__37.clone();let lib_6 = lib_6.clone();let names_0 = names_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_23 = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func_23.clone().invoke(&[self__37.get(),{// Closure
let lib_6 = lib_6.clone();let names_0 = names_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
Scm::func(make_minus_import_minus_only).invoke(&[lib_6.clone(),names_0.clone()])}})}])}})});Scm::anything();free_minus_vars_19.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname_1.set({// Closure
let gen_minus_libname_1 = gen_minus_libname_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_40 = args[0].clone();let lib_7 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib_7.clone()])}).is_true() {{
// (print module "")
imports::print(&[module_40.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module_40.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib_7.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib_7.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module_40.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module_40.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname_1.get().invoke(&[module_40.clone(),{
// (cdr lib)
imports::cdr(&[lib_7.clone()])}])}}}})});Scm::anything();gen_minus_imports_1.set({// Closure
let gen_minus_imports_1 = gen_minus_imports_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module_41 = args[0].clone();let names_1 = args[1].clone();if ({
// (null? names)
imports::null_p(&[names_1.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
imports::print(&[module_41.clone(),{
// (rustify-identifier (car names))
imports::rustify_minus_identifier(&[{
// (car names)
imports::car(&[names_1.clone()])}])}])};{
// (print module ", ")
imports::print(&[module_41.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports_1.get().invoke(&[module_41.clone(),{
// (cdr names)
imports::cdr(&[names_1.clone()])}])}}}})});Scm::anything();gen_minus_rust_21.set({// Closure
let gen_minus_libname_1 = gen_minus_libname_1.clone();let lib_6 = lib_6.clone();let gen_minus_imports_1 = gen_minus_imports_1.clone();let names_0 = names_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_42 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module_42.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname_1.get().invoke(&[module_42.clone(),lib_6.clone()])};{
// (print module "::exports::{")
imports::print(&[module_42.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports_1.get().invoke(&[module_42.clone(),names_0.clone()])};{
// (print module "};")
imports::print(&[module_42.clone(),Scm::from("};")])};{
// (println module)
imports::println(&[module_42.clone()])}}})});Scm::anything();self__37.set({// Closure
let repr_21 = repr_21.clone();let transform_21 = transform_21.clone();let free_minus_vars_19 = free_minus_vars_19.clone();let gen_minus_rust_21 = gen_minus_rust_21.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_21 = args[0].clone();let args__32 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_21.clone()])}).is_true() {{
// (repr)
repr_21.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_21.clone()])}).is_true() {{
// (transform (car args))
transform_21.get().invoke(&[{
// (car args)
imports::car(&[args__32.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_21.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_19.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_21.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_21.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_21.get().invoke(&[{
// (car args)
imports::car(&[args__32.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg_21.clone()])}}}})});Scm::anything();self__37.get()}}}}}}}}}}}}.into()
}
pub fn make_minus_library(args: &[Scm]) -> Scm {
    {
        if args.len() != 6 {
            panic!("invalid arity")
        }
        let name_35 = args[0].clone();
        let globals_1 = args[1].clone();
        let init_4 = args[2].clone();
        let body_6 = args[3].clone();
        let imports_1 = args[4].clone();
        let exports_0 = args[5].clone();
        {
            // (let* ((tests (list (quote dummy))) (new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (_make-library name globals init new-body imports exports (cdr tests)))
            {
                // (let ((tests (list (quote dummy)))) (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests)))))
                {
                    let tests_0 = {
                        // (list (quote dummy))
                        imports::list(&[Scm::symbol("dummy")])
                    };
                    // (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests))))
                    let new_minus_body_0 = {
                        // (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
                        body_6.clone().invoke(&[Scm::symbol("transform"), {
                            // Closure
                            let tests_0 = tests_0.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node_9 = args[0].clone();
                                let ignore_0 = args[1].clone();
                                if ({
                                    // (eq? (node (quote kind)) (quote TESTSUITE))
                                    imports::eq_p(&[
                                        {
                                            // (node (quote kind))
                                            node_9.clone().invoke(&[Scm::symbol("kind")])
                                        },
                                        Scm::symbol("TESTSUITE"),
                                    ])
                                })
                                .is_true()
                                {
                                    {
                                        {
                                            // (set-cdr! tests (cons node (cdr tests)))
                                            imports::set_minus_cdr_i(&[tests_0.clone(), {
                                                // (cons node (cdr tests))
                                                imports::cons(&[node_9.clone(), {
                                                    // (cdr tests)
                                                    imports::cdr(&[tests_0.clone()])
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
                                        ignore_0.clone().invoke(&[])
                                    }
                                }
                            })
                        }])
                    };
                    {
                        // (_make-library name globals init new-body imports exports (cdr tests))
                        Scm::func(__make_minus_library).invoke(&[
                            name_35.clone(),
                            globals_1.clone(),
                            init_4.clone(),
                            new_minus_body_0.clone(),
                            imports_1.clone(),
                            exports_0.clone(),
                            {
                                // (cdr tests)
                                imports::cdr(&[tests_0.clone()])
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
                    let [repr_1, transform_1, free_minus_vars_1, gen_minus_rust_1, self__17] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__17 = self__17.into_boxed();
                        {
                            let gen_minus_rust_1 = gen_minus_rust_1.into_boxed();
                            {
                                let free_minus_vars_1 = free_minus_vars_1.into_boxed();
                                {
                                    let transform_1 = transform_1.into_boxed();
                                    {
                                        let repr_1 = repr_1.into_boxed();
                                        {
                                            repr_1.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    Scm::pair(Scm::symbol("NOP"), Scm::Nil)
                                                })
                                            });
                                            Scm::anything();
                                            transform_1.set({
                                                // Closure
                                                let self__17 = self__17.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func_7 = args[0].clone();
                                                    {
                                                        // (func self (lambda () self))
                                                        func_7.clone().invoke(&[self__17.get(), {
                                                            // Closure
                                                            let self__17 = self__17.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                self__17.get()
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars_1.set({
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
                                            gen_minus_rust_1.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module_13 = args[0].clone();
                                                    {
                                                        // (print module "(/*NOP*/)")
                                                        imports::print(&[
                                                            module_13.clone(),
                                                            Scm::from("(/*NOP*/)"),
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__17.set({
                                                // Closure
                                                let repr_1 = repr_1.clone();
                                                let transform_1 = transform_1.clone();
                                                let free_minus_vars_1 = free_minus_vars_1.clone();
                                                let gen_minus_rust_1 = gen_minus_rust_1.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg_1 = args[0].clone();
                                                    let args__10 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg_1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr_1.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg_1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform_1.get().invoke(&[{
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
                                                                msg_1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars_1.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg_1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NOP")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg_1.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust_1.get().invoke(&[{
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
                                                                    msg_1.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__17.get()
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
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        {
            // (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module ""))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote NULL-ARG)))) (set! transform (lambda (fnc) (fnc self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module ""))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))) self))
                {
                    let [repr_10, transform_10, free_minus_vars_10, gen_minus_rust_10, self__26] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__26 = self__26.into_boxed();
                        {
                            let gen_minus_rust_10 = gen_minus_rust_10.into_boxed();
                            {
                                let free_minus_vars_10 = free_minus_vars_10.into_boxed();
                                {
                                    let transform_10 = transform_10.into_boxed();
                                    {
                                        let repr_10 = repr_10.into_boxed();
                                        {
                                            repr_10.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (list (quote NULL-ARG))
                                                        imports::list(&[Scm::symbol("NULL-ARG")])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            transform_10.set({
                                                // Closure
                                                let self__26 = self__26.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let fnc_2 = args[0].clone();
                                                    {
                                                        // (fnc self (lambda () self))
                                                        fnc_2.clone().invoke(&[self__26.get(), {
                                                            // Closure
                                                            let self__26 = self__26.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                self__26.get()
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars_10.set({
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
                                            gen_minus_rust_10.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module_24 = args[0].clone();
                                                    {
                                                        // (print module "")
                                                        imports::print(&[
                                                            module_24.clone(),
                                                            Scm::from(""),
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__26.set({
                                                // Closure
                                                let repr_10 = repr_10.clone();
                                                let transform_10 = transform_10.clone();
                                                let free_minus_vars_10 = free_minus_vars_10.clone();
                                                let gen_minus_rust_10 = gen_minus_rust_10.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg_10 = args[0].clone();
                                                    let args__21 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg_10.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr_10.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg_10.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform_10.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__21.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg_10.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars_10.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg_10.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NULL-ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg_10.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust_10.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__21.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NULL-ARG" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message NULL-ARG",
                                                                    ),
                                                                    msg_10.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__26.get()
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
pub fn make_minus_program(args: &[Scm]) -> Scm {
    {if args.len() != 5{panic!("invalid arity")}let globals_0 = args[0].clone();let imports_0 = args[1].clone();let init_3 = args[2].clone();let body_5 = args[3].clone();let libraries_0 = args[4].clone();{
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (set! transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (set! gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))) self))
{let [repr_16, transform_16, gen_minus_imports_0, gen_minus_rust_16, self__32, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__32 = self__32.into_boxed();{let gen_minus_rust_16 = gen_minus_rust_16.into_boxed();{let gen_minus_imports_0 = gen_minus_imports_0.into_boxed();{let transform_16 = transform_16.into_boxed();{let repr_16 = repr_16.into_boxed();{repr_16.set({// Closure
let globals_0 = globals_0.clone();let imports_0 = imports_0.clone();let body_5 = body_5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
imports::cons(&[globals_0.clone(),{
// (cons imports (body (quote repr)))
imports::cons(&[imports_0.clone(),{
// (body (quote repr))
body_5.clone().invoke(&[Scm::symbol("repr")])}])}])}])}})});Scm::anything();transform_16.set({// Closure
let self__32 = self__32.clone();let globals_0 = globals_0.clone();let imports_0 = imports_0.clone();let init_3 = init_3.clone();let body_5 = body_5.clone();let libraries_0 = libraries_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_18 = args[0].clone();{
// (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries)))
func_18.clone().invoke(&[self__32.get(),{// Closure
let globals_0 = globals_0.clone();let imports_0 = imports_0.clone();let init_3 = init_3.clone();let body_5 = body_5.clone();let func_18 = func_18.clone();let libraries_0 = libraries_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-program globals imports init (body (quote transform) func) libraries)
Scm::func(make_minus_program).invoke(&[globals_0.clone(),imports_0.clone(),init_3.clone(),{
// (body (quote transform) func)
body_5.clone().invoke(&[Scm::symbol("transform"),func_18.clone()])},libraries_0.clone()])}})}])}})});Scm::anything();gen_minus_imports_0.set({// Closure
let imports_0 = imports_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_32 = args[0].clone();{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module_32 = module_32.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i_0 = args[0].clone();{
// (i (quote gen-rust) module)
i_0.clone().invoke(&[Scm::symbol("gen-rust"),module_32.clone()])}})},imports_0.clone()])}})});Scm::anything();gen_minus_rust_16.set({// Closure
let gen_minus_imports_0 = gen_minus_imports_0.clone();let globals_0 = globals_0.clone();let init_3 = init_3.clone();let body_5 = body_5.clone();let libraries_0 = libraries_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_33 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println(&[module_33.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
imports::print(&[module_33.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block(&[module_33.clone(),{// Closure
let gen_minus_imports_0 = gen_minus_imports_0.clone();let module_33 = module_33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports_0.get().invoke(&[module_33.clone()])}})}])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module_33.clone(),globals_0.clone()])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (print module "pub fn main()")
imports::print(&[module_33.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block(&[module_33.clone(),{// Closure
let module_33 = module_33.clone();let init_3 = init_3.clone();let body_5 = body_5.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module_33.clone()])};{
// (println module "eprintln!(\"built with\");")
imports::println(&[module_33.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println(&[module_33.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each(&[{// Closure
let module_33 = module_33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib_2 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module_33.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each(&[{// Closure
let module_33 = module_33.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l_0 = args[0].clone();{{
// (print module (rustify-libname l))
imports::print(&[module_33.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l_0.clone()])}])};{
// (print module "::")
imports::print(&[module_33.clone(),Scm::from("::")])}}})},lib_2.clone()])};{
// (print module "initialize();")
imports::print(&[module_33.clone(),Scm::from("initialize();")])};{
// (println module)
imports::println(&[module_33.clone()])}}})},init_3.clone()])};{
// (body (quote gen-rust) module)
body_5.clone().invoke(&[Scm::symbol("gen-rust"),module_33.clone()])};{
// (println module ";")
imports::println(&[module_33.clone(),Scm::from(";")])}}})}])};{
// (println module)
imports::println(&[module_33.clone()])};{
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules(&[module_33.clone(),libraries_0.clone()])}}})});Scm::anything();self__32.set({// Closure
let repr_16 = repr_16.clone();let transform_16 = transform_16.clone();let gen_minus_rust_16 = gen_minus_rust_16.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_16 = args[0].clone();let args__27 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_16.clone()])}).is_true() {{
// (repr)
repr_16.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_16.clone()])}).is_true() {{
// (transform (car args))
transform_16.get().invoke(&[{
// (car args)
imports::car(&[args__27.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_16.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_16.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_16.get().invoke(&[{
// (car args)
imports::car(&[args__27.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
imports::error(&[Scm::from("Unknown message PROGRAM"),msg_16.clone()])}}}})});Scm::anything();self__32.get()}}}}}}}}}}.into()
}
pub fn make_minus_reference(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let var_15 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote GET) (variable-name var)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" name ")")) ((import-variable? var) (print module "Scm::func(imports::" name ")")) ((boxed-variable? var) (print module name ".get()")) (else (print module name ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) (variable-name var)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" name ")")) ((import-variable? var) (print module "Scm::func(imports::" name ")")) ((boxed-variable? var) (print module name ".get()")) (else (print module name ".clone()")))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr_3, transform_3, free_minus_vars_3, gen_minus_rust_3, self__19, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__19 = self__19.into_boxed();{let gen_minus_rust_3 = gen_minus_rust_3.into_boxed();{let free_minus_vars_3 = free_minus_vars_3.into_boxed();{let transform_3 = transform_3.into_boxed();{let repr_3 = repr_3.into_boxed();{repr_3.set({// Closure
let var_15 = var_15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) (variable-name var))
imports::list(&[Scm::symbol("GET"),{
// (variable-name var)
imports::variable_minus_name(&[var_15.clone()])}])}})});Scm::anything();transform_3.set({// Closure
let self__19 = self__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_9 = args[0].clone();{
// (func self (lambda () self))
func_9.clone().invoke(&[self__19.get(),{// Closure
let self__19 = self__19.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__19.get()})}])}})});Scm::anything();free_minus_vars_3.set({// Closure
let var_15 = var_15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var_15.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var_15.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var_15.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (make-set) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (make-set)
imports::make_minus_set(&[])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var_15.clone()])}])}])}}})});Scm::anything();gen_minus_rust_3.set({// Closure
let var_15 = var_15.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_16 = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module name ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" name ")")) ((import-variable? var) (print module "Scm::func(imports::" name ")")) ((boxed-variable? var) (print module name ".get()")) (else (print module name ".clone()"))))
{let name_31 = {
// (variable-name var)
imports::variable_minus_name(&[var_15.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var_15.clone()])}).is_true() {{
// (print module name ".with(|value| value.get())")
imports::print(&[module_16.clone(),name_31.clone(),Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var_15.clone()])}).is_true() {{
// (print module "Scm::func(" name ")")
imports::print(&[module_16.clone(),Scm::from("Scm::func("),name_31.clone(),Scm::from(")")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var_15.clone()])}).is_true() {{
// (print module "Scm::func(imports::" name ")")
imports::print(&[module_16.clone(),Scm::from("Scm::func(imports::"),name_31.clone(),Scm::from(")")])}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var_15.clone()])}).is_true() {{
// (print module name ".get()")
imports::print(&[module_16.clone(),name_31.clone(),Scm::from(".get()")])}} else {{
// (print module name ".clone()")
imports::print(&[module_16.clone(),name_31.clone(),Scm::from(".clone()")])}}}}}})});Scm::anything();self__19.set({// Closure
let repr_3 = repr_3.clone();let transform_3 = transform_3.clone();let free_minus_vars_3 = free_minus_vars_3.clone();let gen_minus_rust_3 = gen_minus_rust_3.clone();let var_15 = var_15.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_3 = args[0].clone();let args__12 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_3.clone()])}).is_true() {{
// (repr)
repr_3.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_3.clone()])}).is_true() {{
// (transform (car args))
transform_3.get().invoke(&[{
// (car args)
imports::car(&[args__12.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_3.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_3.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_3.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_3.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_3.get().invoke(&[{
// (car args)
imports::car(&[args__12.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg_3.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var_15.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg_3.clone()])}).is_true() {var_15.clone()} else {{
// (error "Unknown message REFERENCE" msg)
imports::error(&[Scm::from("Unknown message REFERENCE"),msg_3.clone()])}}}})});Scm::anything();self__19.get()}}}}}}}}}}.into()
}
pub fn make_minus_sequence(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let first_1 = args[0].clone();let next_0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr_7, transform_7, free_minus_vars_7, gen_minus_rust_minus_inner_1, gen_minus_rust_7, self__23, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__23 = self__23.into_boxed();{let gen_minus_rust_7 = gen_minus_rust_7.into_boxed();{let gen_minus_rust_minus_inner_1 = gen_minus_rust_minus_inner_1.into_boxed();{let free_minus_vars_7 = free_minus_vars_7.into_boxed();{let transform_7 = transform_7.into_boxed();{let repr_7 = repr_7.into_boxed();{repr_7.set({// Closure
let first_1 = first_1.clone();let next_0 = next_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list(&[Scm::symbol("SEQUENCE"),{
// (first (quote repr))
first_1.clone().invoke(&[Scm::symbol("repr")])},{
// (next (quote repr))
next_0.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_7.set({// Closure
let self__23 = self__23.clone();let next_0 = next_0.clone();let first_1 = first_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_13 = args[0].clone();{
// (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))))
func_13.clone().invoke(&[self__23.get(),{// Closure
let next_0 = next_0.clone();let func_13 = func_13.clone();let first_1 = first_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))
{
// (let ((a (first (quote transform) func))) (let ((b (next (quote transform) func))) (begin (make-sequence a b))))
{let a_5 = {
// (first (quote transform) func)
first_1.clone().invoke(&[Scm::symbol("transform"),func_13.clone()])};
// (let ((b (next (quote transform) func))) (begin (make-sequence a b)))
let b_1 = {
// (next (quote transform) func)
next_0.clone().invoke(&[Scm::symbol("transform"),func_13.clone()])};{
// (make-sequence a b)
Scm::func(make_minus_sequence).invoke(&[a_5.clone(),b_1.clone()])}}}}})}])}})});Scm::anything();free_minus_vars_7.set({// Closure
let first_1 = first_1.clone();let next_0 = next_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (first (quote free-vars))
first_1.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next_0.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner_1.set({// Closure
let first_1 = first_1.clone();let next_0 = next_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_20 = args[0].clone();{{
// (first (quote gen-rust) module)
first_1.clone().invoke(&[Scm::symbol("gen-rust"),module_20.clone()])};{
// (print module ";")
imports::print(&[module_20.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next_0.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next_0.clone().invoke(&[Scm::symbol("gen-rust-inner"),module_20.clone()])}} else {{
// (next (quote gen-rust) module)
next_0.clone().invoke(&[Scm::symbol("gen-rust"),module_20.clone()])}}}})});Scm::anything();gen_minus_rust_7.set({// Closure
let gen_minus_rust_minus_inner_1 = gen_minus_rust_minus_inner_1.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_21 = args[0].clone();{{
// (print module "{")
imports::print(&[module_21.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner_1.get().invoke(&[module_21.clone()])};{
// (print module "}")
imports::print(&[module_21.clone(),Scm::from("}")])}}})});Scm::anything();self__23.set({// Closure
let repr_7 = repr_7.clone();let transform_7 = transform_7.clone();let free_minus_vars_7 = free_minus_vars_7.clone();let gen_minus_rust_7 = gen_minus_rust_7.clone();let gen_minus_rust_minus_inner_1 = gen_minus_rust_minus_inner_1.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_7 = args[0].clone();let args__16 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_7.clone()])}).is_true() {{
// (repr)
repr_7.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_7.clone()])}).is_true() {{
// (transform (car args))
transform_7.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_7.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_7.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_7.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_7.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_7.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg_7.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner_1.get().invoke(&[{
// (car args)
imports::car(&[args__16.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
imports::error(&[Scm::from("Unknown message SEQUENCE"),msg_7.clone()])}}}})});Scm::anything();self__23.get()}}}}}}}}}}}.into()
}
pub fn make_minus_testcase(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let description_0 = args[0].clone();
        let body_9 = args[1].clone();
        {
            // (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
                {
                    let [repr_22, transform_22, free_minus_vars_20, gen_minus_rust_22, self__38] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__38 = self__38.into_boxed();
                        {
                            let gen_minus_rust_22 = gen_minus_rust_22.into_boxed();
                            {
                                let free_minus_vars_20 = free_minus_vars_20.into_boxed();
                                {
                                    let transform_22 = transform_22.into_boxed();
                                    {
                                        let repr_22 = repr_22.into_boxed();
                                        {
                                            repr_22.set({
                                                // Closure
                                                let description_0 = description_0.clone();
                                                let body_9 = body_9.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (list (quote TESTCASE) description body)
                                                        imports::list(&[
                                                            Scm::symbol("TESTCASE"),
                                                            description_0.clone(),
                                                            body_9.clone(),
                                                        ])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            transform_22.set({
                                                // Closure
                                                let self__38 = self__38.clone();
                                                let description_0 = description_0.clone();
                                                let body_9 = body_9.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func_24 = args[0].clone();
                                                    {
                                                        // (func self (lambda () (make-testcase description (body (quote transform) func))))
                                                        func_24.clone().invoke(&[self__38.get(), {
                                                            // Closure
                                                            let description_0 =
                                                                description_0.clone();
                                                            let body_9 = body_9.clone();
                                                            let func_24 = func_24.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                {
                                                                    // (make-testcase description (body (quote transform) func))
                                                                    Scm::func(make_minus_testcase)
                                                                        .invoke(&[
                                                                            description_0.clone(),
                                                                            {
                                                                                // (body (quote transform) func)
                                                                                body_9
                                                                                    .clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    func_24.clone(),
                                                                                ])
                                                                            },
                                                                        ])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            free_minus_vars_20.set({
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
                                            gen_minus_rust_22.set({
                                                // Closure
                                                let description_0 = description_0.clone();
                                                let body_9 = body_9.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module_43 = args[0].clone();
                                                    {
                                                        {
                                                            // (println module "#[test]")
                                                            imports::println(&[
                                                                module_43.clone(),
                                                                Scm::from("#[test]"),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "fn " (rustify-testname description) "() {")
                                                            imports::println(&[
                                                                module_43.clone(),
                                                                Scm::from("fn "),
                                                                {
                                                                    // (rustify-testname description)
                                                                    imports::rustify_minus_testname(
                                                                        &[description_0.clone()],
                                                                    )
                                                                },
                                                                Scm::from("() {"),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "super::initialize();")
                                                            imports::println(&[
                                                                module_43.clone(),
                                                                Scm::from("super::initialize();"),
                                                            ])
                                                        };
                                                        {
                                                            // (body (quote gen-rust) module)
                                                            body_9.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module_43.clone(),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "}")
                                                            imports::println(&[
                                                                module_43.clone(),
                                                                Scm::from("}"),
                                                            ])
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__38.set({
                                                // Closure
                                                let repr_22 = repr_22.clone();
                                                let transform_22 = transform_22.clone();
                                                let free_minus_vars_20 = free_minus_vars_20.clone();
                                                let gen_minus_rust_22 = gen_minus_rust_22.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg_22 = args[0].clone();
                                                    let args__33 = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg_22.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr_22.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg_22.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform_22.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__33.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg_22.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars_20.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg_22.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("TESTCASE")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg_22.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust_22.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(
                                                                        &[args__33.clone()],
                                                                    )
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message TESTCASE" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message TESTCASE",
                                                                    ),
                                                                    msg_22.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            Scm::anything();
                                            self__38.get()
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
pub fn make_minus_testsuite(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name_38 = args[0].clone();let cases_0 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr_23, transform_23, free_minus_vars_21, gen_minus_rust_23, self__39, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__39 = self__39.into_boxed();{let gen_minus_rust_23 = gen_minus_rust_23.into_boxed();{let free_minus_vars_21 = free_minus_vars_21.into_boxed();{let transform_23 = transform_23.into_boxed();{let repr_23 = repr_23.into_boxed();{repr_23.set({// Closure
let name_38 = name_38.clone();let cases_0 = cases_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
imports::list(&[Scm::symbol("TESTSUITE"),name_38.clone(),cases_0.clone()])}})});Scm::anything();transform_23.set({// Closure
let self__39 = self__39.clone();let name_38 = name_38.clone();let cases_0 = cases_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_25 = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func_25.clone().invoke(&[self__39.get(),{// Closure
let name_38 = name_38.clone();let func_25 = func_25.clone();let cases_0 = cases_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
Scm::func(make_minus_testsuite).invoke(&[name_38.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
imports::map(&[{// Closure
let func_25 = func_25.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c_0 = args[0].clone();{
// (c (quote transform) func)
c_0.clone().invoke(&[Scm::symbol("transform"),func_25.clone()])}})},cases_0.clone()])}])}})}])}})});Scm::anything();free_minus_vars_21.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust_23.set({// Closure
let cases_0 = cases_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_44 = args[0].clone();{{
// (println module "#[cfg(test)]")
imports::println(&[module_44.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
imports::println(&[module_44.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
imports::println(&[module_44.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
imports::for_minus_each(&[{// Closure
let module_44 = module_44.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c_1 = args[0].clone();{
// (c (quote gen-rust) module)
c_1.clone().invoke(&[Scm::symbol("gen-rust"),module_44.clone()])}})},cases_0.clone()])};{
// (println module "}")
imports::println(&[module_44.clone(),Scm::from("}")])}}})});Scm::anything();self__39.set({// Closure
let repr_23 = repr_23.clone();let transform_23 = transform_23.clone();let free_minus_vars_21 = free_minus_vars_21.clone();let gen_minus_rust_23 = gen_minus_rust_23.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_23 = args[0].clone();let args__34 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_23.clone()])}).is_true() {{
// (repr)
repr_23.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_23.clone()])}).is_true() {{
// (transform (car args))
transform_23.get().invoke(&[{
// (car args)
imports::car(&[args__34.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_23.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_21.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_23.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_23.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_23.get().invoke(&[{
// (car args)
imports::car(&[args__34.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
imports::error(&[Scm::from("Unknown message TESTSUITE"),msg_23.clone()])}}}})});Scm::anything();self__39.get()}}}}}}}}}}.into()
}
pub fn make_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars_2 = args[0].clone();let varvar_0 = args[1].clone();let body_4 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (variable-name (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (variable-name varvar) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (variable-name (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (variable-name varvar) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr_15, transform_15, free_minus_vars_16, gen_minus_rust_15, self__31, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__31 = self__31.into_boxed();{let gen_minus_rust_15 = gen_minus_rust_15.into_boxed();{let free_minus_vars_16 = free_minus_vars_16.into_boxed();{let transform_15 = transform_15.into_boxed();{let repr_15 = repr_15.into_boxed();{repr_15.set({// Closure
let vars_2 = vars_2.clone();let varvar_0 = varvar_0.clone();let body_4 = body_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr)))
imports::list(&[Scm::symbol("VARARG-ABSTRACTION"),vars_2.clone(),varvar_0.clone(),{
// (body (quote repr))
body_4.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform_15.set({// Closure
let self__31 = self__31.clone();let vars_2 = vars_2.clone();let varvar_0 = varvar_0.clone();let body_4 = body_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func_17 = args[0].clone();{
// (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func))))
func_17.clone().invoke(&[self__31.get(),{// Closure
let vars_2 = vars_2.clone();let varvar_0 = varvar_0.clone();let body_4 = body_4.clone();let func_17 = func_17.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-vararg-abstraction vars varvar (body (quote transform) func))
Scm::func(make_minus_vararg_minus_abstraction).invoke(&[vars_2.clone(),varvar_0.clone(),{
// (body (quote transform) func)
body_4.clone().invoke(&[Scm::symbol("transform"),func_17.clone()])}])}})}])}})});Scm::anything();free_minus_vars_16.set({// Closure
let body_4 = body_4.clone();let varvar_0 = varvar_0.clone();let vars_2 = vars_2.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars))))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body_4.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name (cons varvar vars)))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name (cons varvar vars))
imports::map(&[Scm::func(imports::variable_minus_name),{
// (cons varvar vars)
imports::cons(&[varvar_0.clone(),vars_2.clone()])}])}])}])}})});Scm::anything();gen_minus_rust_15.set({// Closure
let varvar_0 = varvar_0.clone();let vars_2 = vars_2.clone();let body_4 = body_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module_31 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (variable-name (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (variable-name varvar) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (variable-name (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (variable-name varvar) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params_2 = Scm::symbol("*uninitialized*");{let gen_minus_params_2 = gen_minus_params_2.into_boxed();{gen_minus_params_2.set({// Closure
let module_31 = module_31.clone();let gen_minus_params_2 = gen_minus_params_2.clone();let varvar_0 = varvar_0.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star__1 = args[0].clone();let k_1 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star__1.clone()])}).is_true() {{{
// (print module "let " (variable-name (car p*)) " = args[" k "].clone();")
imports::print(&[module_31.clone(),Scm::from("let "),{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star__1.clone()])}])},Scm::from(" = args["),k_1.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params_2.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star__1.clone()])},{
// (+ k 1)
imports::_plus_(&[k_1.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (variable-name varvar) " = Scm::list(&args[" k "..]);")
imports::print(&[module_31.clone(),Scm::from("let "),{
// (variable-name varvar)
imports::variable_minus_name(&[varvar_0.clone()])},Scm::from(" = Scm::list(&args["),k_1.clone(),Scm::from("..]);")])}}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module_31.clone(),{// Closure
let module_31 = module_31.clone();let vars_2 = vars_2.clone();let gen_minus_params_2 = gen_minus_params_2.clone();let body_4 = body_4.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}")
imports::print(&[module_31.clone(),Scm::from("if args.len() < "),{
// (length vars)
imports::length(&[vars_2.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params vars 0)
gen_minus_params_2.get().invoke(&[vars_2.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body_4.clone().invoke(&[Scm::symbol("gen-rust"),module_31.clone()])}}})}])}}}}}}})});Scm::anything();self__31.set({// Closure
let repr_15 = repr_15.clone();let transform_15 = transform_15.clone();let free_minus_vars_16 = free_minus_vars_16.clone();let gen_minus_rust_15 = gen_minus_rust_15.clone();let vars_2 = vars_2.clone();let varvar_0 = varvar_0.clone();let body_4 = body_4.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg_15 = args[0].clone();let args__26 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg_15.clone()])}).is_true() {{
// (repr)
repr_15.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg_15.clone()])}).is_true() {{
// (transform (car args))
transform_15.get().invoke(&[{
// (car args)
imports::car(&[args__26.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg_15.clone()])}).is_true() {{
// (free-vars)
free_minus_vars_16.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg_15.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg_15.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust_15.get().invoke(&[{
// (car args)
imports::car(&[args__26.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg_15.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars_2.clone()])}} else if ({
// (eq? (quote get-vararg) msg)
imports::eq_p(&[Scm::symbol("get-vararg"),msg_15.clone()])}).is_true() {{
// (variable-name varvar)
imports::variable_minus_name(&[varvar_0.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg_15.clone()])}).is_true() {vars_2.clone()} else if ({
// (eq? (quote get-varvar) msg)
imports::eq_p(&[Scm::symbol("get-varvar"),msg_15.clone()])}).is_true() {varvar_0.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg_15.clone()])}).is_true() {body_4.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg_15.clone()])}}}})});Scm::anything();self__31.get()}}}}}}}}}}.into()
}
pub fn procedure_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj_16 = args[0].clone();
        if ({
            // (eq? (obj (quote kind)) (quote ABSTRACTION))
            imports::eq_p(&[
                {
                    // (obj (quote kind))
                    obj_16.clone().invoke(&[Scm::symbol("kind")])
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
                        obj_16.clone().invoke(&[Scm::symbol("kind")])
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
