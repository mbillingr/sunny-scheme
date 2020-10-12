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
    {if args.len() != 7{panic!("invalid arity")}let name__571 = args[0].clone();let globals__568 = args[1].clone();let init__576 = args[2].clone();let body__567 = args[3].clone();let imports__569 = args[4].clone();let exports__570 = args[5].clone();let testsuite__573 = args[6].clone();{
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-exports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (set! gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))) self))
{let [repr__566, transform__572, gen_minus_exports__578, gen_minus_rust__582, self__577, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__577 = self__577.into_boxed();{let gen_minus_rust__582 = gen_minus_rust__582.into_boxed();{let gen_minus_exports__578 = gen_minus_exports__578.into_boxed();{let transform__572 = transform__572.into_boxed();{let repr__566 = repr__566.into_boxed();{repr__566.set({// Closure
let name__571 = name__571.clone();let exports__570 = exports__570.clone();let imports__569 = imports__569.clone();let globals__568 = globals__568.clone();let body__567 = body__567.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
imports::append(&[Scm::symbol("LIBRARY"),name__571.clone(),exports__570.clone(),imports__569.clone(),globals__568.clone(),{
// (body (quote repr))
body__567.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__572.set({// Closure
let self__577 = self__577.clone();let name__571 = name__571.clone();let globals__568 = globals__568.clone();let init__576 = init__576.clone();let body__567 = body__567.clone();let imports__569 = imports__569.clone();let exports__570 = exports__570.clone();let testsuite__573 = testsuite__573.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__574 = args[0].clone();{
// (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))))
func__574.clone().invoke(&[self__577.get(),{// Closure
let name__571 = name__571.clone();let globals__568 = globals__568.clone();let init__576 = init__576.clone();let body__567 = body__567.clone();let func__574 = func__574.clone();let imports__569 = imports__569.clone();let exports__570 = exports__570.clone();let testsuite__573 = testsuite__573.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))
Scm::func(__make_minus_library).invoke(&[name__571.clone(),globals__568.clone(),init__576.clone(),{
// (body (quote transform) func)
body__567.clone().invoke(&[Scm::symbol("transform"),func__574.clone()])},imports__569.clone(),exports__570.clone(),{
// (map (lambda (t) (t (quote transform) func)) testsuite)
imports::map(&[{// Closure
let func__574 = func__574.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let t__575 = args[0].clone();{
// (t (quote transform) func)
t__575.clone().invoke(&[Scm::symbol("transform"),func__574.clone()])}})},testsuite__573.clone()])}])}})}])}})});Scm::anything();gen_minus_exports__578.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__580 = args[0].clone();let exports__579 = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each(&[{// Closure
let module__580 = module__580.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo__581 = args[0].clone();{
// (expo (quote gen-rust) module)
expo__581.clone().invoke(&[Scm::symbol("gen-rust"),module__580.clone()])}})},exports__579.clone()])}})});Scm::anything();gen_minus_rust__582.set({// Closure
let imports__569 = imports__569.clone();let gen_minus_exports__578 = gen_minus_exports__578.clone();let exports__570 = exports__570.clone();let globals__568 = globals__568.clone();let body__567 = body__567.clone();let init__576 = init__576.clone();let testsuite__573 = testsuite__573.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__583 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println(&[module__583.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
imports::print(&[module__583.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block(&[module__583.clone(),{// Closure
let module__583 = module__583.clone();let imports__569 = imports__569.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module__583 = module__583.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i__584 = args[0].clone();{
// (i (quote gen-rust) module)
i__584.clone().invoke(&[Scm::symbol("gen-rust"),module__583.clone()])}})},imports__569.clone()])}})}])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (print module "pub mod exports")
imports::print(&[module__583.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block(&[module__583.clone(),{// Closure
let gen_minus_exports__578 = gen_minus_exports__578.clone();let module__583 = module__583.clone();let exports__570 = exports__570.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports__578.get().invoke(&[module__583.clone(),exports__570.clone()])}})}])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module__583.clone(),globals__568.clone()])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (println module)
imports::println(&[module__583.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p(&[Scm::symbol("NOP"),{
// (body (quote kind))
body__567.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
imports::println(&[module__583.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println(&[module__583.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
imports::println(&[module__583.clone()])};{
// (println module "pub fn initialize() {")
imports::println(&[module__583.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println(&[module__583.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println(&[module__583.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
imports::println(&[module__583.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each(&[{// Closure
let module__583 = module__583.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib__585 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module__583.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each(&[{// Closure
let module__583 = module__583.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l__586 = args[0].clone();{
// (print module (rustify-libname l) "::")
imports::print(&[module__583.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l__586.clone()])},Scm::from("::")])}})},lib__585.clone()])};{
// (println module "initialize();")
imports::println(&[module__583.clone(),Scm::from("initialize();")])}}})},init__576.clone()])};{
// (body (quote gen-rust) module)
body__567.clone().invoke(&[Scm::symbol("gen-rust"),module__583.clone()])};{
// (println module ";}")
imports::println(&[module__583.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) testsuite)
imports::for_minus_each(&[{// Closure
let module__583 = module__583.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test__587 = args[0].clone();{
// (test (quote gen-rust) module)
test__587.clone().invoke(&[Scm::symbol("gen-rust"),module__583.clone()])}})},testsuite__573.clone()])}}})});Scm::anything();self__577.set({// Closure
let repr__566 = repr__566.clone();let transform__572 = transform__572.clone();let name__571 = name__571.clone();let gen_minus_rust__582 = gen_minus_rust__582.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__588 = args[0].clone();let args__589 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__588.clone()])}).is_true() {{
// (repr)
repr__566.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__588.clone()])}).is_true() {{
// (transform (car args))
transform__572.get().invoke(&[{
// (car args)
imports::car(&[args__589.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__588.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
imports::eq_p(&[Scm::symbol("libname"),msg__588.clone()])}).is_true() {name__571.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__588.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__582.get().invoke(&[{
// (car args)
imports::car(&[args__589.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
imports::error(&[Scm::from("Unknown message LIBRARY"),msg__588.clone()])}}}})});Scm::anything();self__577.get()}}}}}}}}}}.into()
}
pub fn ast_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__336 = args[0].clone();
        {
            // (procedure? obj)
            imports::procedure_p(&[obj__336.clone()])
        }
    }
    .into()
}
pub fn free_minus_var_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name__677 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (symbol? name)
                imports::symbol_p(&[name__677.clone()])
            })
            .is_true()
            {
                name__677.clone()
            } else if ({
                // (string? name)
                imports::string_p(&[name__677.clone()])
            })
            .is_true()
            {
                {
                    // (string->symbol name)
                    imports::string_minus__g_symbol(&[name__677.clone()])
                }
            } else {
                {
                    // (error "Invalid variable name" name)
                    imports::error(&[Scm::from("Invalid variable name"), name__677.clone()])
                }
            }
        }
    }
    .into()
}
pub fn make_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let vars__511 = args[0].clone();let body__510 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ABSTRACTION) vars (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-abstraction vars (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))) self))
{let [repr__509, transform__512, free_minus_vars__515, gen_minus_rust__516, self__514, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__514 = self__514.into_boxed();{let gen_minus_rust__516 = gen_minus_rust__516.into_boxed();{let free_minus_vars__515 = free_minus_vars__515.into_boxed();{let transform__512 = transform__512.into_boxed();{let repr__509 = repr__509.into_boxed();{repr__509.set({// Closure
let vars__511 = vars__511.clone();let body__510 = body__510.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ABSTRACTION) vars (body (quote repr)))
imports::list(&[Scm::symbol("ABSTRACTION"),vars__511.clone(),{
// (body (quote repr))
body__510.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__512.set({// Closure
let self__514 = self__514.clone();let vars__511 = vars__511.clone();let body__510 = body__510.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__513 = args[0].clone();{
// (func self (lambda () (make-abstraction vars (body (quote transform) func))))
func__513.clone().invoke(&[self__514.get(),{// Closure
let vars__511 = vars__511.clone();let body__510 = body__510.clone();let func__513 = func__513.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-abstraction vars (body (quote transform) func))
Scm::func(make_minus_abstraction).invoke(&[vars__511.clone(),{
// (body (quote transform) func)
body__510.clone().invoke(&[Scm::symbol("transform"),func__513.clone()])}])}})}])}})});Scm::anything();free_minus_vars__515.set({// Closure
let body__510 = body__510.clone();let vars__511 = vars__511.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__510.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__511.clone()])}])}])}})});Scm::anything();gen_minus_rust__516.set({// Closure
let vars__511 = vars__511.clone();let body__510 = body__510.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__518 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (variable-name (car p*)))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params__517 = Scm::symbol("*uninitialized*");{let gen_minus_params__517 = gen_minus_params__517.into_boxed();{gen_minus_params__517.set({// Closure
let module__518 = module__518.clone();let gen_minus_params__517 = gen_minus_params__517.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star___519 = args[0].clone();let k__520 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star___519.clone()])}).is_true() {{{
// (print module "let ")
imports::print(&[module__518.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name (car p*))))
imports::print(&[module__518.clone(),{
// (rustify-identifier (variable-name (car p*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star___519.clone()])}])}])}])};{
// (print module " = args[")
imports::print(&[module__518.clone(),Scm::from(" = args[")])};{
// (print module k)
imports::print(&[module__518.clone(),k__520.clone()])};{
// (print module "].clone();")
imports::print(&[module__518.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params__517.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star___519.clone()])},{
// (+ k 1)
imports::_plus_(&[k__520.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length vars)) (print module "{panic!(\"invalid arity\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__518.clone(),{// Closure
let module__518 = module__518.clone();let vars__511 = vars__511.clone();let gen_minus_params__517 = gen_minus_params__517.clone();let body__510 = body__510.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
imports::print(&[module__518.clone(),Scm::from("if args.len() != ")])};{
// (print module (length vars))
imports::print(&[module__518.clone(),{
// (length vars)
imports::length(&[vars__511.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
imports::print(&[module__518.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params vars 0)
gen_minus_params__517.get().invoke(&[vars__511.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body__510.clone().invoke(&[Scm::symbol("gen-rust"),module__518.clone()])}}})}])}}}}}}})});Scm::anything();self__514.set({// Closure
let repr__509 = repr__509.clone();let transform__512 = transform__512.clone();let free_minus_vars__515 = free_minus_vars__515.clone();let gen_minus_rust__516 = gen_minus_rust__516.clone();let vars__511 = vars__511.clone();let body__510 = body__510.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__521 = args[0].clone();let args__522 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__521.clone()])}).is_true() {{
// (repr)
repr__509.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__521.clone()])}).is_true() {{
// (transform (car args))
transform__512.get().invoke(&[{
// (car args)
imports::car(&[args__522.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__521.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__515.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__521.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__521.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__516.get().invoke(&[{
// (car args)
imports::car(&[args__522.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__521.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__511.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__521.clone()])}).is_true() {vars__511.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__521.clone()])}).is_true() {body__510.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message ABSTRACTION"),msg__521.clone()])}}}})});Scm::anything();self__514.get()}}}}}}}}}}.into()
}
pub fn make_minus_alternative(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let condition__411 = args[0].clone();let consequent__410 = args[1].clone();let alternative__409 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr__408, transform__412, free_minus_vars__415, gen_minus_rust__416, self__414, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__414 = self__414.into_boxed();{let gen_minus_rust__416 = gen_minus_rust__416.into_boxed();{let free_minus_vars__415 = free_minus_vars__415.into_boxed();{let transform__412 = transform__412.into_boxed();{let repr__408 = repr__408.into_boxed();{repr__408.set({// Closure
let condition__411 = condition__411.clone();let consequent__410 = consequent__410.clone();let alternative__409 = alternative__409.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list(&[Scm::symbol("IF"),{
// (condition (quote repr))
condition__411.clone().invoke(&[Scm::symbol("repr")])},{
// (consequent (quote repr))
consequent__410.clone().invoke(&[Scm::symbol("repr")])},{
// (alternative (quote repr))
alternative__409.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__412.set({// Closure
let self__414 = self__414.clone();let condition__411 = condition__411.clone();let consequent__410 = consequent__410.clone();let alternative__409 = alternative__409.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__413 = args[0].clone();{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func__413.clone().invoke(&[self__414.get(),{// Closure
let condition__411 = condition__411.clone();let func__413 = func__413.clone();let consequent__410 = consequent__410.clone();let alternative__409 = alternative__409.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
Scm::func(make_minus_alternative).invoke(&[{
// (condition (quote transform) func)
condition__411.clone().invoke(&[Scm::symbol("transform"),func__413.clone()])},{
// (consequent (quote transform) func)
consequent__410.clone().invoke(&[Scm::symbol("transform"),func__413.clone()])},{
// (alternative (quote transform) func)
alternative__409.clone().invoke(&[Scm::symbol("transform"),func__413.clone()])}])}})}])}})});Scm::anything();free_minus_vars__415.set({// Closure
let condition__411 = condition__411.clone();let consequent__410 = consequent__410.clone();let alternative__409 = alternative__409.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
imports::set_minus_union(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
imports::set_minus_union(&[{
// (condition (quote free-vars))
condition__411.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent__410.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative__409.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__416.set({// Closure
let condition__411 = condition__411.clone();let consequent__410 = consequent__410.clone();let alternative__409 = alternative__409.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__417 = args[0].clone();{{
// (print module "if (")
imports::print(&[module__417.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition__411.clone().invoke(&[Scm::symbol("gen-rust"),module__417.clone()])};{
// (print module ").is_true() {")
imports::print(&[module__417.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent__410.clone().invoke(&[Scm::symbol("gen-rust"),module__417.clone()])};{
// (print module "} else ")
imports::print(&[module__417.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
imports::eq_p(&[{
// (alternative (quote kind))
alternative__409.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative__409.clone().invoke(&[Scm::symbol("gen-rust"),module__417.clone()])}} else {{{
// (print module "{")
imports::print(&[module__417.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative__409.clone().invoke(&[Scm::symbol("gen-rust"),module__417.clone()])};{
// (print module "}")
imports::print(&[module__417.clone(),Scm::from("}")])}}}}})});Scm::anything();self__414.set({// Closure
let repr__408 = repr__408.clone();let transform__412 = transform__412.clone();let free_minus_vars__415 = free_minus_vars__415.clone();let gen_minus_rust__416 = gen_minus_rust__416.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__418 = args[0].clone();let args__419 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__418.clone()])}).is_true() {{
// (repr)
repr__408.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__418.clone()])}).is_true() {{
// (transform (car args))
transform__412.get().invoke(&[{
// (car args)
imports::car(&[args__419.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__418.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__415.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__418.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__418.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__416.get().invoke(&[{
// (car args)
imports::car(&[args__419.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
imports::error(&[Scm::from("Unknown message ALTERNATIVE"),msg__418.clone()])}}}})});Scm::anything();self__414.get()}}}}}}}}}}.into()
}
pub fn make_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let func__437 = args[0].clone();let args__436 = args[1].clone();let tail_p__438 = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr__435, transform__439, free_minus_vars__442, gen_minus_rust__443, self__441, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__441 = self__441.into_boxed();{let gen_minus_rust__443 = gen_minus_rust__443.into_boxed();{let free_minus_vars__442 = free_minus_vars__442.into_boxed();{let transform__439 = transform__439.into_boxed();{let repr__435 = repr__435.into_boxed();{repr__435.set({// Closure
let tail_p__438 = tail_p__438.clone();let func__437 = func__437.clone();let args__436 = args__436.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons(&[if (tail_p__438.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
imports::cons(&[{
// (func (quote repr))
func__437.clone().invoke(&[Scm::symbol("repr")])},{
// (args (quote repr))
args__436.clone().invoke(&[Scm::symbol("repr")])}])}])}})});Scm::anything();transform__439.set({// Closure
let self__441 = self__441.clone();let func__437 = func__437.clone();let args__436 = args__436.clone();let tail_p__438 = tail_p__438.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__440 = args[0].clone();{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc__440.clone().invoke(&[self__441.get(),{// Closure
let func__437 = func__437.clone();let fnc__440 = fnc__440.clone();let args__436 = args__436.clone();let tail_p__438 = tail_p__438.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
Scm::func(make_minus_application).invoke(&[{
// (func (quote transform) fnc)
func__437.clone().invoke(&[Scm::symbol("transform"),fnc__440.clone()])},{
// (args (quote transform) fnc)
args__436.clone().invoke(&[Scm::symbol("transform"),fnc__440.clone()])},tail_p__438.clone()])}})}])}})});Scm::anything();free_minus_vars__442.set({// Closure
let func__437 = func__437.clone();let args__436 = args__436.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
imports::set_minus_union(&[{
// (func (quote free-vars))
func__437.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args__436.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__443.set({// Closure
let func__437 = func__437.clone();let args__436 = args__436.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__444 = args[0].clone();{{
// (func (quote gen-rust) module)
func__437.clone().invoke(&[Scm::symbol("gen-rust"),module__444.clone()])};{
// (print module ".invoke(&[")
imports::print(&[module__444.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args__436.clone().invoke(&[Scm::symbol("gen-rust"),module__444.clone()])};{
// (print module "])")
imports::print(&[module__444.clone(),Scm::from("])")])}}})});Scm::anything();self__441.set({// Closure
let repr__435 = repr__435.clone();let transform__439 = transform__439.clone();let free_minus_vars__442 = free_minus_vars__442.clone();let gen_minus_rust__443 = gen_minus_rust__443.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__445 = args[0].clone();let args__446 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__445.clone()])}).is_true() {{
// (repr)
repr__435.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__445.clone()])}).is_true() {{
// (transform (car args))
transform__439.get().invoke(&[{
// (car args)
imports::car(&[args__446.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__445.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__442.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__445.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__445.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__443.get().invoke(&[{
// (car args)
imports::car(&[args__446.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
imports::error(&[Scm::from("Unknown message APPLICATION"),msg__445.clone()])}}}})});Scm::anything();self__441.get()}}}}}}}}}}.into()
}
pub fn make_minus_args(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let arg__470 = args[0].clone();let next__469 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote ARG) (cons arg next)))) (set! transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (set! gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))) self))
{let [repr__468, transform__471, free_minus_vars__474, gen_minus_rust__475, self__473, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__473 = self__473.into_boxed();{let gen_minus_rust__475 = gen_minus_rust__475.into_boxed();{let free_minus_vars__474 = free_minus_vars__474.into_boxed();{let transform__471 = transform__471.into_boxed();{let repr__468 = repr__468.into_boxed();{repr__468.set({// Closure
let arg__470 = arg__470.clone();let next__469 = next__469.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote ARG) (cons arg next))
imports::cons(&[Scm::symbol("ARG"),{
// (cons arg next)
imports::cons(&[arg__470.clone(),next__469.clone()])}])}})});Scm::anything();transform__471.set({// Closure
let self__473 = self__473.clone();let arg__470 = arg__470.clone();let next__469 = next__469.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__472 = args[0].clone();{
// (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
fnc__472.clone().invoke(&[self__473.get(),{// Closure
let arg__470 = arg__470.clone();let fnc__472 = fnc__472.clone();let next__469 = next__469.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
Scm::func(make_minus_args).invoke(&[{
// (arg (quote transform) fnc)
arg__470.clone().invoke(&[Scm::symbol("transform"),fnc__472.clone()])},{
// (next (quote transform) fnc)
next__469.clone().invoke(&[Scm::symbol("transform"),fnc__472.clone()])}])}})}])}})});Scm::anything();free_minus_vars__474.set({// Closure
let arg__470 = arg__470.clone();let next__469 = next__469.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (arg (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (arg (quote free-vars))
arg__470.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next__469.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust__475.set({// Closure
let arg__470 = arg__470.clone();let next__469 = next__469.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__476 = args[0].clone();{{
// (arg (quote gen-rust) module)
arg__470.clone().invoke(&[Scm::symbol("gen-rust"),module__476.clone()])};if ({
// (not (eq? (quote NULL-ARG) (next (quote kind))))
imports::not(&[{
// (eq? (quote NULL-ARG) (next (quote kind)))
imports::eq_p(&[Scm::symbol("NULL-ARG"),{
// (next (quote kind))
next__469.clone().invoke(&[Scm::symbol("kind")])}])}])}).is_true() {{{
// (print module ",")
imports::print(&[module__476.clone(),Scm::from(",")])};{
// (next (quote gen-rust) module)
next__469.clone().invoke(&[Scm::symbol("gen-rust"),module__476.clone()])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});Scm::anything();self__473.set({// Closure
let repr__468 = repr__468.clone();let transform__471 = transform__471.clone();let free_minus_vars__474 = free_minus_vars__474.clone();let gen_minus_rust__475 = gen_minus_rust__475.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__477 = args[0].clone();let args__478 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__477.clone()])}).is_true() {{
// (repr)
repr__468.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__477.clone()])}).is_true() {{
// (transform (car args))
transform__471.get().invoke(&[{
// (car args)
imports::car(&[args__478.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__477.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__474.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__477.clone()])}).is_true() {Scm::symbol("ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__477.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__475.get().invoke(&[{
// (car args)
imports::car(&[args__478.clone()])}])}} else {{
// (error "Unknown message ARG" msg)
imports::error(&[Scm::from("Unknown message ARG"),msg__477.clone()])}}}})});Scm::anything();self__473.get()}}}}}}}}}}.into()
}
pub fn make_minus_assert(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let condition__668 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ASSERT) condition))) (set! transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (set! free-vars (lambda () (condition (quote free-vars)))) (set! gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))) self))
{let [repr__667, transform__669, free_minus_vars__672, gen_minus_rust__673, self__671, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__671 = self__671.into_boxed();{let gen_minus_rust__673 = gen_minus_rust__673.into_boxed();{let free_minus_vars__672 = free_minus_vars__672.into_boxed();{let transform__669 = transform__669.into_boxed();{let repr__667 = repr__667.into_boxed();{repr__667.set({// Closure
let condition__668 = condition__668.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ASSERT) condition)
imports::list(&[Scm::symbol("ASSERT"),condition__668.clone()])}})});Scm::anything();transform__669.set({// Closure
let self__671 = self__671.clone();let condition__668 = condition__668.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__670 = args[0].clone();{
// (func self (lambda () (make-assert (condition (quote transform) func))))
func__670.clone().invoke(&[self__671.get(),{// Closure
let condition__668 = condition__668.clone();let func__670 = func__670.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assert (condition (quote transform) func))
Scm::func(make_minus_assert).invoke(&[{
// (condition (quote transform) func)
condition__668.clone().invoke(&[Scm::symbol("transform"),func__670.clone()])}])}})}])}})});Scm::anything();free_minus_vars__672.set({// Closure
let condition__668 = condition__668.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (condition (quote free-vars))
condition__668.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__673.set({// Closure
let condition__668 = condition__668.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__674 = args[0].clone();{{
// (print module "assert!(")
imports::print(&[module__674.clone(),Scm::from("assert!(")])};{
// (condition (quote gen-rust) module)
condition__668.clone().invoke(&[Scm::symbol("gen-rust"),module__674.clone()])};{
// (println module ".is_true());")
imports::println(&[module__674.clone(),Scm::from(".is_true());")])}}})});Scm::anything();self__671.set({// Closure
let repr__667 = repr__667.clone();let transform__669 = transform__669.clone();let free_minus_vars__672 = free_minus_vars__672.clone();let gen_minus_rust__673 = gen_minus_rust__673.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__675 = args[0].clone();let args__676 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__675.clone()])}).is_true() {{
// (repr)
repr__667.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__675.clone()])}).is_true() {{
// (transform (car args))
transform__669.get().invoke(&[{
// (car args)
imports::car(&[args__676.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__675.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__672.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__675.clone()])}).is_true() {Scm::symbol("ASSERT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__675.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__673.get().invoke(&[{
// (car args)
imports::car(&[args__676.clone()])}])}} else {{
// (error "Unknown message ASSERT" msg)
imports::error(&[Scm::from("Unknown message ASSERT"),msg__675.clone()])}}}})});Scm::anything();self__671.get()}}}}}}}}}}.into()
}
pub fn make_minus_assignment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__386 = args[0].clone();let val__385 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (val (quote free-vars)) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var)))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr__384, transform__387, free_minus_vars__390, gen_minus_rust__391, self__389, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__389 = self__389.into_boxed();{let gen_minus_rust__391 = gen_minus_rust__391.into_boxed();{let free_minus_vars__390 = free_minus_vars__390.into_boxed();{let transform__387 = transform__387.into_boxed();{let repr__384 = repr__384.into_boxed();{repr__384.set({// Closure
let var__386 = var__386.clone();let val__385 = val__385.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("SET!"),{
// (variable-name var)
imports::variable_minus_name(&[var__386.clone()])},{
// (val (quote repr))
val__385.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__387.set({// Closure
let self__389 = self__389.clone();let var__386 = var__386.clone();let val__385 = val__385.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__388 = args[0].clone();{
// (func self (lambda () (make-assignment var (val (quote transform) func))))
func__388.clone().invoke(&[self__389.get(),{// Closure
let var__386 = var__386.clone();let val__385 = val__385.clone();let func__388 = func__388.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment var (val (quote transform) func))
Scm::func(make_minus_assignment).invoke(&[var__386.clone(),{
// (val (quote transform) func)
val__385.clone().invoke(&[Scm::symbol("transform"),func__388.clone()])}])}})}])}})});Scm::anything();free_minus_vars__390.set({// Closure
let var__386 = var__386.clone();let val__385 = val__385.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var__386.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var__386.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var__386.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val__385.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__386.clone()])}])}])}}})});Scm::anything();gen_minus_rust__391.set({// Closure
let var__386 = var__386.clone();let val__385 = val__385.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__393 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ");")) (else (error "set! on unboxed variable" name var))))
{let name__392 = {
// (variable-name var)
imports::variable_minus_name(&[var__386.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__386.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__393.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__392.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__385.clone().invoke(&[Scm::symbol("gen-rust"),module__393.clone()])};{
// (print module "));")
imports::print(&[module__393.clone(),Scm::from("));")])}}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var__386.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".set(")
imports::print(&[module__393.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__392.clone()])},Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val__385.clone().invoke(&[Scm::symbol("gen-rust"),module__393.clone()])};{
// (print module ");")
imports::print(&[module__393.clone(),Scm::from(");")])}}} else {{
// (error "set! on unboxed variable" name var)
imports::error(&[Scm::from("set! on unboxed variable"),name__392.clone(),var__386.clone()])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module__393.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__389.set({// Closure
let repr__384 = repr__384.clone();let transform__387 = transform__387.clone();let free_minus_vars__390 = free_minus_vars__390.clone();let gen_minus_rust__391 = gen_minus_rust__391.clone();let var__386 = var__386.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__394 = args[0].clone();let args__395 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__394.clone()])}).is_true() {{
// (repr)
repr__384.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__394.clone()])}).is_true() {{
// (transform (car args))
transform__387.get().invoke(&[{
// (car args)
imports::car(&[args__395.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__394.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__390.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__394.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__394.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__391.get().invoke(&[{
// (car args)
imports::car(&[args__395.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__394.clone()])}).is_true() {var__386.clone()} else {{
// (error "Unknown message ASSIGNMENT" msg)
imports::error(&[Scm::from("Unknown message ASSIGNMENT"),msg__394.clone()])}}}})});Scm::anything();self__389.get()}}}}}}}}}}.into()
}
pub fn make_minus_boxify(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name__592 = args[0].clone();let body__591 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (set! transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr__590, transform__593, free_minus_vars__596, gen_minus_rust__597, self__595, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__595 = self__595.into_boxed();{let gen_minus_rust__597 = gen_minus_rust__597.into_boxed();{let free_minus_vars__596 = free_minus_vars__596.into_boxed();{let transform__593 = transform__593.into_boxed();{let repr__590 = repr__590.into_boxed();{repr__590.set({// Closure
let name__592 = name__592.clone();let body__591 = body__591.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote BOXIFY) (cons name (body (quote repr))))
imports::cons(&[Scm::symbol("BOXIFY"),{
// (cons name (body (quote repr)))
imports::cons(&[name__592.clone(),{
// (body (quote repr))
body__591.clone().invoke(&[Scm::symbol("repr")])}])}])}})});Scm::anything();transform__593.set({// Closure
let self__595 = self__595.clone();let name__592 = name__592.clone();let body__591 = body__591.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__594 = args[0].clone();{
// (func self (lambda () (make-boxify name (body (quote transform) func))))
func__594.clone().invoke(&[self__595.get(),{// Closure
let name__592 = name__592.clone();let body__591 = body__591.clone();let func__594 = func__594.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-boxify name (body (quote transform) func))
Scm::func(make_minus_boxify).invoke(&[name__592.clone(),{
// (body (quote transform) func)
body__591.clone().invoke(&[Scm::symbol("transform"),func__594.clone()])}])}})}])}})});Scm::anything();free_minus_vars__596.set({// Closure
let body__591 = body__591.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (body (quote free-vars))
body__591.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__597.set({// Closure
let name__592 = name__592.clone();let body__591 = body__591.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__598 = args[0].clone();{
// (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__598.clone(),{// Closure
let module__598 = module__598.clone();let name__592 = name__592.clone();let body__591 = body__591.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
imports::print(&[module__598.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__598.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__592.clone()])}])};{
// (print module " = ")
imports::print(&[module__598.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__598.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__592.clone()])}])};{
// (print module ".into_boxed();")
imports::print(&[module__598.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body__591.clone().invoke(&[Scm::symbol("gen-rust"),module__598.clone()])}}})}])}})});Scm::anything();self__595.set({// Closure
let repr__590 = repr__590.clone();let transform__593 = transform__593.clone();let free_minus_vars__596 = free_minus_vars__596.clone();let gen_minus_rust__597 = gen_minus_rust__597.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__599 = args[0].clone();let args__600 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__599.clone()])}).is_true() {{
// (repr)
repr__590.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__599.clone()])}).is_true() {{
// (transform (car args))
transform__593.get().invoke(&[{
// (car args)
imports::car(&[args__600.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__599.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__596.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__599.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__599.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__597.get().invoke(&[{
// (car args)
imports::car(&[args__600.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
imports::error(&[Scm::from("Unknown message BOXIFY"),msg__599.clone()])}}}})});Scm::anything();self__595.get()}}}}}}}}}}.into()
}
pub fn make_minus_closure(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let function__496 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr__495, transform__497, free_minus_vars__500, prepare_minus_closure__501, gen_minus_rust__505, self__499, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__499 = self__499.into_boxed();{let gen_minus_rust__505 = gen_minus_rust__505.into_boxed();{let prepare_minus_closure__501 = prepare_minus_closure__501.into_boxed();{let free_minus_vars__500 = free_minus_vars__500.into_boxed();{let transform__497 = transform__497.into_boxed();{let repr__495 = repr__495.into_boxed();{repr__495.set({// Closure
let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
imports::list(&[Scm::symbol("CLOSURE"),function__496.clone()])}})});Scm::anything();transform__497.set({// Closure
let self__499 = self__499.clone();let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__498 = args[0].clone();{
// (func self (lambda () (make-closure (function (quote transform) func))))
func__498.clone().invoke(&[self__499.get(),{// Closure
let function__496 = function__496.clone();let func__498 = func__498.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-closure (function (quote transform) func))
Scm::func(make_minus_closure).invoke(&[{
// (function (quote transform) func)
function__496.clone().invoke(&[Scm::symbol("transform"),func__498.clone()])}])}})}])}})});Scm::anything();free_minus_vars__500.set({// Closure
let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (function (quote free-vars))
function__496.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();prepare_minus_closure__501.set({// Closure
let prepare_minus_closure__501 = prepare_minus_closure__501.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__502 = args[0].clone();let free_minus_vars__504 = args[1].clone();if ({
// (pair? free-vars)
imports::pair_p(&[free_minus_vars__504.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name__503 = {
// (car free-vars)
imports::car(&[free_minus_vars__504.clone()])};{{
// (print module "let ")
imports::print(&[module__502.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__502.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__503.clone()])}])};{
// (print module " = ")
imports::print(&[module__502.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print(&[module__502.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__503.clone()])}])};{
// (print module ".clone();")
imports::print(&[module__502.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure__501.get().invoke(&[module__502.clone(),{
// (cdr free-vars)
imports::cdr(&[free_minus_vars__504.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();gen_minus_rust__505.set({// Closure
let prepare_minus_closure__501 = prepare_minus_closure__501.clone();let free_minus_vars__500 = free_minus_vars__500.clone();let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__506 = args[0].clone();{
// (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
imports::rust_minus_block(&[module__506.clone(),{// Closure
let module__506 = module__506.clone();let prepare_minus_closure__501 = prepare_minus_closure__501.clone();let free_minus_vars__500 = free_minus_vars__500.clone();let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "// Closure")
imports::println(&[module__506.clone(),Scm::from("// Closure")])};{
// (prepare-closure module (free-vars))
prepare_minus_closure__501.get().invoke(&[module__506.clone(),{
// (free-vars)
free_minus_vars__500.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
imports::print(&[module__506.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function__496.clone().invoke(&[Scm::symbol("gen-rust"),module__506.clone()])};{
// (print module ")")
imports::print(&[module__506.clone(),Scm::from(")")])}}})}])}})});Scm::anything();self__499.set({// Closure
let repr__495 = repr__495.clone();let transform__497 = transform__497.clone();let free_minus_vars__500 = free_minus_vars__500.clone();let gen_minus_rust__505 = gen_minus_rust__505.clone();let function__496 = function__496.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__507 = args[0].clone();let args__508 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__507.clone()])}).is_true() {{
// (repr)
repr__495.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__507.clone()])}).is_true() {{
// (transform (car args))
transform__497.get().invoke(&[{
// (car args)
imports::car(&[args__508.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__507.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__500.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__507.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__507.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__505.get().invoke(&[{
// (car args)
imports::car(&[args__508.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
imports::eq_p(&[Scm::symbol("inner-function"),msg__507.clone()])}).is_true() {function__496.clone()} else {{
// (error "Unknown message CLOSURE" msg)
imports::error(&[Scm::from("Unknown message CLOSURE"),msg__507.clone()])}}}})});Scm::anything();self__499.get()}}}}}}}}}}}.into()
}
pub fn make_minus_comment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let comment__340 = args[0].clone();let node__339 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr__338, transform__341, free_minus_vars__344, gen_minus_rust__345, gen_minus_rust_minus_inner__347, self__343, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__343 = self__343.into_boxed();{let gen_minus_rust_minus_inner__347 = gen_minus_rust_minus_inner__347.into_boxed();{let gen_minus_rust__345 = gen_minus_rust__345.into_boxed();{let free_minus_vars__344 = free_minus_vars__344.into_boxed();{let transform__341 = transform__341.into_boxed();{let repr__338 = repr__338.into_boxed();{repr__338.set({// Closure
let comment__340 = comment__340.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
imports::list(&[Scm::symbol("COMMENT"),comment__340.clone(),{
// (node (quote repr))
node__339.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__341.set({// Closure
let self__343 = self__343.clone();let comment__340 = comment__340.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__342 = args[0].clone();{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func__342.clone().invoke(&[self__343.get(),{// Closure
let comment__340 = comment__340.clone();let node__339 = node__339.clone();let func__342 = func__342.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-comment comment (node (quote transform) func))
Scm::func(make_minus_comment).invoke(&[comment__340.clone(),{
// (node (quote transform) func)
node__339.clone().invoke(&[Scm::symbol("transform"),func__342.clone()])}])}})}])}})});Scm::anything();free_minus_vars__344.set({// Closure
let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (node (quote free-vars))
node__339.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__345.set({// Closure
let comment__340 = comment__340.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__346 = args[0].clone();{
// (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)))
imports::rust_minus_block(&[module__346.clone(),{// Closure
let module__346 = module__346.clone();let comment__340 = comment__340.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module__346.clone()])};{
// (print module "// ")
imports::print(&[module__346.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module__346.clone(),comment__340.clone()])};{
// (node (quote gen-rust) module)
node__339.clone().invoke(&[Scm::symbol("gen-rust"),module__346.clone()])}}})}])}})});Scm::anything();gen_minus_rust_minus_inner__347.set({// Closure
let comment__340 = comment__340.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__348 = args[0].clone();{{
// (println module)
imports::println(&[module__348.clone()])};{
// (print module "// ")
imports::print(&[module__348.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module__348.clone(),comment__340.clone()])};{
// (node (quote gen-rust-inner) module)
node__339.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__348.clone()])}}})});Scm::anything();self__343.set({// Closure
let repr__338 = repr__338.clone();let transform__341 = transform__341.clone();let free_minus_vars__344 = free_minus_vars__344.clone();let gen_minus_rust__345 = gen_minus_rust__345.clone();let gen_minus_rust_minus_inner__347 = gen_minus_rust_minus_inner__347.clone();let node__339 = node__339.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__349 = args[0].clone();let args__350 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__349.clone()])}).is_true() {{
// (repr)
repr__338.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__349.clone()])}).is_true() {{
// (transform (car args))
transform__341.get().invoke(&[{
// (car args)
imports::car(&[args__350.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__349.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__344.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__349.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__349.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__345.get().invoke(&[{
// (car args)
imports::car(&[args__350.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__349.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner__347.get().invoke(&[{
// (car args)
imports::car(&[args__350.clone()])}])}} else if ({
// (eq? (quote inner) msg)
imports::eq_p(&[Scm::symbol("inner"),msg__349.clone()])}).is_true() {node__339.clone()} else {{
// (error "Unknown message COMMENT" msg)
imports::error(&[Scm::from("Unknown message COMMENT"),msg__349.clone()])}}}})});Scm::anything();self__343.get()}}}}}}}}}}}.into()
}
pub fn make_minus_constant(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let val__361 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr__360, transform__362, free_minus_vars__365, gen_minus_constant__366, gen_minus_rust__369, self__363, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__363 = self__363.into_boxed();{let gen_minus_rust__369 = gen_minus_rust__369.into_boxed();{let gen_minus_constant__366 = gen_minus_constant__366.into_boxed();{let free_minus_vars__365 = free_minus_vars__365.into_boxed();{let transform__362 = transform__362.into_boxed();{let repr__360 = repr__360.into_boxed();{repr__360.set({// Closure
let val__361 = val__361.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
imports::cons(&[Scm::symbol("CONSTANT"),val__361.clone()])}})});Scm::anything();transform__362.set({// Closure
let self__363 = self__363.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__364 = args[0].clone();{
// (func self (lambda () self))
func__364.clone().invoke(&[self__363.get(),{// Closure
let self__363 = self__363.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__363.get()})}])}})});Scm::anything();free_minus_vars__365.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_constant__366.set({// Closure
let gen_minus_constant__366 = gen_minus_constant__366.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__367 = args[0].clone();let val__368 = args[1].clone();{
// (cond ...)
if ({
// (null? val)
imports::null_p(&[val__368.clone()])}).is_true() {{
// (print module "Scm::Nil")
imports::print(&[module__367.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
imports::eq_p(&[val__368.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
imports::print(&[module__367.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
imports::eq_p(&[val__368.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
imports::print(&[module__367.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
imports::symbol_p(&[val__368.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
imports::print(&[module__367.clone(),Scm::from("Scm::symbol(\""),val__368.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
imports::eq_p(&[val__368.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
imports::print(&[module__367.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
imports::char_p(&[val__368.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
imports::print(&[module__367.clone(),Scm::from("Scm::char('"),val__368.clone(),Scm::from("')")])}} else if ({
// (pair? val)
imports::pair_p(&[val__368.clone()])}).is_true() {{{
// (print module "Scm::pair(")
imports::print(&[module__367.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant__366.get().invoke(&[module__367.clone(),{
// (car val)
imports::car(&[val__368.clone()])}])};{
// (print module ", ")
imports::print(&[module__367.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant__366.get().invoke(&[module__367.clone(),{
// (cdr val)
imports::cdr(&[val__368.clone()])}])};{
// (print module ")")
imports::print(&[module__367.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
imports::print(&[module__367.clone(),Scm::from("Scm::from(")])};{
// (show module val)
imports::show(&[module__367.clone(),val__368.clone()])};{
// (print module ")")
imports::print(&[module__367.clone(),Scm::from(")")])}}}}})});Scm::anything();gen_minus_rust__369.set({// Closure
let gen_minus_constant__366 = gen_minus_constant__366.clone();let val__361 = val__361.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__370 = args[0].clone();{
// (gen-constant module val)
gen_minus_constant__366.get().invoke(&[module__370.clone(),val__361.clone()])}})});Scm::anything();self__363.set({// Closure
let repr__360 = repr__360.clone();let transform__362 = transform__362.clone();let free_minus_vars__365 = free_minus_vars__365.clone();let gen_minus_rust__369 = gen_minus_rust__369.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__371 = args[0].clone();let args__372 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__371.clone()])}).is_true() {{
// (repr)
repr__360.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__371.clone()])}).is_true() {{
// (transform (car args))
transform__362.get().invoke(&[{
// (car args)
imports::car(&[args__372.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__371.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__365.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__371.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__371.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__369.get().invoke(&[{
// (car args)
imports::car(&[args__372.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
imports::error(&[Scm::from("Unknown message CONSTANT"),msg__371.clone()])}}}})});Scm::anything();self__363.get()}}}}}}}}}}}.into()
}
pub fn make_minus_definition(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var__398 = args[0].clone();let val__397 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable")))) (print module "Scm::anything()"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr__396, transform__399, free_minus_vars__402, gen_minus_rust__403, self__401, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__401 = self__401.into_boxed();{let gen_minus_rust__403 = gen_minus_rust__403.into_boxed();{let free_minus_vars__402 = free_minus_vars__402.into_boxed();{let transform__399 = transform__399.into_boxed();{let repr__396 = repr__396.into_boxed();{repr__396.set({// Closure
let var__398 = var__398.clone();let val__397 = val__397.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("DEFINE"),{
// (variable-name var)
imports::variable_minus_name(&[var__398.clone()])},{
// (val (quote repr))
val__397.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__399.set({// Closure
let self__401 = self__401.clone();let var__398 = var__398.clone();let val__397 = val__397.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__400 = args[0].clone();{
// (func self (lambda () (make-definition var (val (quote transform) func))))
func__400.clone().invoke(&[self__401.get(),{// Closure
let var__398 = var__398.clone();let val__397 = val__397.clone();let func__400 = func__400.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition var (val (quote transform) func))
Scm::func(make_minus_definition).invoke(&[var__398.clone(),{
// (val (quote transform) func)
val__397.clone().invoke(&[Scm::symbol("transform"),func__400.clone()])}])}})}])}})});Scm::anything();free_minus_vars__402.set({// Closure
let val__397 = val__397.clone();let var__398 = var__398.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val__397.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__398.clone()])}])}])}})});Scm::anything();gen_minus_rust__403.set({// Closure
let var__398 = var__398.clone();let val__397 = val__397.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__405 = args[0].clone();{{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "));")) (else (error "definition! of non-global variable"))))
{let name__404 = {
// (variable-name var)
imports::variable_minus_name(&[var__398.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__398.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__405.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__404.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__397.clone().invoke(&[Scm::symbol("gen-rust"),module__405.clone()])};{
// (print module "));")
imports::print(&[module__405.clone(),Scm::from("));")])}}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__398.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module__405.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__404.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val__397.clone().invoke(&[Scm::symbol("gen-rust"),module__405.clone()])};{
// (print module "));")
imports::print(&[module__405.clone(),Scm::from("));")])}}} else {{
// (error "definition! of non-global variable")
imports::error(&[Scm::from("definition! of non-global variable")])}}}}};{
// (print module "Scm::anything()")
imports::print(&[module__405.clone(),Scm::from("Scm::anything()")])}}})});Scm::anything();self__401.set({// Closure
let repr__396 = repr__396.clone();let transform__399 = transform__399.clone();let free_minus_vars__402 = free_minus_vars__402.clone();let gen_minus_rust__403 = gen_minus_rust__403.clone();let var__398 = var__398.clone();let val__397 = val__397.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__406 = args[0].clone();let args__407 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__406.clone()])}).is_true() {{
// (repr)
repr__396.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__406.clone()])}).is_true() {{
// (transform (car args))
transform__399.get().invoke(&[{
// (car args)
imports::car(&[args__407.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__406.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__402.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__406.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__406.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__403.get().invoke(&[{
// (car args)
imports::car(&[args__407.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg__406.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var__398.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__406.clone()])}).is_true() {var__398.clone()} else if ({
// (eq? (quote get-val) msg)
imports::eq_p(&[Scm::symbol("get-val"),msg__406.clone()])}).is_true() {val__397.clone()} else {{
// (error "Unknown message DEFINITION" msg)
imports::error(&[Scm::from("Unknown message DEFINITION"),msg__406.clone()])}}}})});Scm::anything();self__401.get()}}}}}}}}}}.into()
}
pub fn make_minus_export(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let env__610 = args[0].clone();
        let name__603 = args[1].clone();
        let exname__602 = args[2].clone();
        {
            // (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))) self))
                {
                    let [repr__601, transform__604, gen_minus_rust__607, self__605] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self__605 = self__605.into_boxed();
                        {
                            let gen_minus_rust__607 = gen_minus_rust__607.into_boxed();
                            {
                                let transform__604 = transform__604.into_boxed();
                                {
                                    let repr__601 = repr__601.into_boxed();
                                    {
                                        repr__601.set({
                                            // Closure
                                            let name__603 = name__603.clone();
                                            let exname__602 = exname__602.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 0 {
                                                    panic!("invalid arity")
                                                }
                                                {
                                                    // (list (quote EXPORT) name (quote AS) exname)
                                                    imports::list(&[
                                                        Scm::symbol("EXPORT"),
                                                        name__603.clone(),
                                                        Scm::symbol("AS"),
                                                        exname__602.clone(),
                                                    ])
                                                }
                                            })
                                        });
                                        Scm::anything();
                                        transform__604.set({
                                            // Closure
                                            let self__605 = self__605.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let func__606 = args[0].clone();
                                                {
                                                    // (func self (lambda () self))
                                                    func__606.clone().invoke(&[self__605.get(), {
                                                        // Closure
                                                        let self__605 = self__605.clone();
                                                        Scm::func(move |args: &[Scm]| {
                                                            if args.len() != 0 {
                                                                panic!("invalid arity")
                                                            }
                                                            self__605.get()
                                                        })
                                                    }])
                                                }
                                            })
                                        });
                                        Scm::anything();
                                        gen_minus_rust__607.set({
                                            // Closure
                                            let name__603 = name__603.clone();
                                            let env__610 = env__610.clone();
                                            let exname__602 = exname__602.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let module__608 = args[0].clone();
                                                {
                                                    {
                                                        // (print module "pub use super::")
                                                        imports::print(&[
                                                            module__608.clone(),
                                                            Scm::from("pub use super::"),
                                                        ])
                                                    };
                                                    {
                                                        // (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))))
                                                        {
                                                            let var__609 = {
                                                                // (lookup name env)
                                                                imports::lookup(&[
                                                                    name__603.clone(),
                                                                    env__610.clone(),
                                                                ])
                                                            };
                                                            {
                                                                // (cond ...)
                                                                if ({
// (not var)
imports::not(&[var__609.clone()])}).is_true() {{
// (error "undefined export" name)
imports::error(&[Scm::from("undefined export"),name__603.clone()])}} else if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__609.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__608.clone(),Scm::from("")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__609.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__608.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__609.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module__608.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
imports::error(&[Scm::from("invalid export variable"),var__609.clone(),name__603.clone()])}}
                                                            }
                                                        }
                                                    };
                                                    {
                                                        // (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
                                                        imports::println(&[
                                                            module__608.clone(),
                                                            {
                                                                // (rustify-identifier name)
                                                                imports::rustify_minus_identifier(
                                                                    &[name__603.clone()],
                                                                )
                                                            },
                                                            Scm::from(" as "),
                                                            {
                                                                // (rustify-identifier exname)
                                                                imports::rustify_minus_identifier(
                                                                    &[exname__602.clone()],
                                                                )
                                                            },
                                                            Scm::from(";"),
                                                        ])
                                                    }
                                                }
                                            })
                                        });
                                        Scm::anything();
                                        self__605.set({
                                            // Closure
                                            let repr__601 = repr__601.clone();
                                            let transform__604 = transform__604.clone();
                                            let gen_minus_rust__607 = gen_minus_rust__607.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() < 1 {
                                                    panic!("not enough args")
                                                }
                                                let msg__611 = args[0].clone();
                                                let args__612 = Scm::list(&args[1..]);
                                                {
                                                    // (cond ...)
                                                    if ({
                                                        // (eq? (quote repr) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("repr"),
                                                            msg__611.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (repr)
                                                            repr__601.get().invoke(&[])
                                                        }
                                                    } else if ({
                                                        // (eq? (quote transform) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("transform"),
                                                            msg__611.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (transform (car args))
                                                            transform__604.get().invoke(&[{
                                                                // (car args)
                                                                imports::car(&[args__612.clone()])
                                                            }])
                                                        }
                                                    } else if ({
                                                        // (eq? (quote kind) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("kind"),
                                                            msg__611.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        Scm::symbol("EXPORT")
                                                    } else if ({
                                                        // (eq? (quote gen-rust) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("gen-rust"),
                                                            msg__611.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (gen-rust (car args))
                                                            gen_minus_rust__607.get().invoke(&[{
                                                                // (car args)
                                                                imports::car(&[args__612.clone()])
                                                            }])
                                                        }
                                                    } else {
                                                        {
                                                            // (error "Unknown message EXPORT" msg)
                                                            imports::error(&[
                                                                Scm::from("Unknown message EXPORT"),
                                                                msg__611.clone(),
                                                            ])
                                                        }
                                                    }
                                                }
                                            })
                                        });
                                        Scm::anything();
                                        self__605.get()
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
pub fn make_minus_fixlet(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars__482 = args[0].clone();let args__481 = args[1].clone();let body__480 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr__479, transform__483, free_minus_vars__486, gen_minus_rust_minus_inner__487, gen_minus_rust__491, self__485, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__485 = self__485.into_boxed();{let gen_minus_rust__491 = gen_minus_rust__491.into_boxed();{let gen_minus_rust_minus_inner__487 = gen_minus_rust_minus_inner__487.into_boxed();{let free_minus_vars__486 = free_minus_vars__486.into_boxed();{let transform__483 = transform__483.into_boxed();{let repr__479 = repr__479.into_boxed();{repr__479.set({// Closure
let vars__482 = vars__482.clone();let args__481 = args__481.clone();let body__480 = body__480.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) vars (args (quote repr)) (body (quote repr)))
imports::list(&[Scm::symbol("FIXLET"),vars__482.clone(),{
// (args (quote repr))
args__481.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body__480.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__483.set({// Closure
let self__485 = self__485.clone();let vars__482 = vars__482.clone();let args__481 = args__481.clone();let body__480 = body__480.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__484 = args[0].clone();{
// (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc__484.clone().invoke(&[self__485.get(),{// Closure
let vars__482 = vars__482.clone();let args__481 = args__481.clone();let fnc__484 = fnc__484.clone();let body__480 = body__480.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))
Scm::func(make_minus_fixlet).invoke(&[vars__482.clone(),{
// (args (quote transform) fnc)
args__481.clone().invoke(&[Scm::symbol("transform"),fnc__484.clone()])},{
// (body (quote transform) fnc)
body__480.clone().invoke(&[Scm::symbol("transform"),fnc__484.clone()])}])}})}])}})});Scm::anything();free_minus_vars__486.set({// Closure
let body__480 = body__480.clone();let vars__482 = vars__482.clone();let args__481 = args__481.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars)))
imports::set_minus_union(&[{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__480.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__482.clone()])}])}])},{
// (args (quote free-vars))
args__481.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner__487.set({// Closure
let vars__482 = vars__482.clone();let args__481 = args__481.clone();let body__480 = body__480.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__490 = args[0].clone();{
// (letrec ((gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*)))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params__488 = Scm::symbol("*uninitialized*");{let gen_minus_params__488 = gen_minus_params__488.into_boxed();{gen_minus_params__488.set({// Closure
let module__490 = module__490.clone();let gen_minus_params__488 = gen_minus_params__488.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let v_star___489 = args[0].clone();if ({
// (pair? v*)
imports::pair_p(&[v_star___489.clone()])}).is_true() {{{
// (print module (rustify-identifier (variable-name (car v*))) ", ")
imports::print(&[module__490.clone(),{
// (rustify-identifier (variable-name (car v*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car v*))
imports::variable_minus_name(&[{
// (car v*)
imports::car(&[v_star___489.clone()])}])}])},Scm::from(", ")])};{
// (gen-params (cdr v*))
gen_minus_params__488.get().invoke(&[{
// (cdr v*)
imports::cdr(&[v_star___489.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});Scm::anything();{
// (cond ...)
if ({
// (= 0 (length vars))
imports::_e_(&[Scm::from(0),{
// (length vars)
imports::length(&[vars__482.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length vars))
imports::_e_(&[Scm::from(1),{
// (length vars)
imports::length(&[vars__482.clone()])}])}).is_true() {{{
// (print module "let ")
imports::print(&[module__490.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name (car vars))))
imports::print(&[module__490.clone(),{
// (rustify-identifier (variable-name (car vars)))
imports::rustify_minus_identifier(&[{
// (variable-name (car vars))
imports::variable_minus_name(&[{
// (car vars)
imports::car(&[vars__482.clone()])}])}])}])};{
// (print module " = ")
imports::print(&[module__490.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args__481.clone().invoke(&[Scm::symbol("gen-rust"),module__490.clone()])};{
// (print module ";")
imports::print(&[module__490.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
imports::print(&[module__490.clone(),Scm::from("let [")])};{
// (gen-params vars)
gen_minus_params__488.get().invoke(&[vars__482.clone()])};{
// (print module "] = [")
imports::print(&[module__490.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args__481.clone().invoke(&[Scm::symbol("gen-rust"),module__490.clone()])};{
// (print module "];")
imports::print(&[module__490.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body__480.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body__480.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__490.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
imports::eq_p(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body__480.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// ((body (quote inner)) (quote kind))
{
// (body (quote inner))
body__480.clone().invoke(&[Scm::symbol("inner")])}.invoke(&[Scm::symbol("kind")])}])}} else {Scm::False}}).is_true() {{
// (body (quote gen-rust-inner) module)
body__480.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__490.clone()])}} else {{
// (body (quote gen-rust) module)
body__480.clone().invoke(&[Scm::symbol("gen-rust"),module__490.clone()])}}}}}}}})});Scm::anything();gen_minus_rust__491.set({// Closure
let gen_minus_rust_minus_inner__487 = gen_minus_rust_minus_inner__487.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__492 = args[0].clone();{
// (rust-block module (lambda () (gen-rust-inner module)))
imports::rust_minus_block(&[module__492.clone(),{// Closure
let gen_minus_rust_minus_inner__487 = gen_minus_rust_minus_inner__487.clone();let module__492 = module__492.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner__487.get().invoke(&[module__492.clone()])}})}])}})});Scm::anything();self__485.set({// Closure
let repr__479 = repr__479.clone();let transform__483 = transform__483.clone();let free_minus_vars__486 = free_minus_vars__486.clone();let vars__482 = vars__482.clone();let args__481 = args__481.clone();let body__480 = body__480.clone();let gen_minus_rust__491 = gen_minus_rust__491.clone();let gen_minus_rust_minus_inner__487 = gen_minus_rust_minus_inner__487.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__493 = args[0].clone();let arg_star___494 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__493.clone()])}).is_true() {{
// (repr)
repr__479.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__493.clone()])}).is_true() {{
// (transform (car arg*))
transform__483.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___494.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__493.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__486.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__493.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__493.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__482.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__493.clone()])}).is_true() {vars__482.clone()} else if ({
// (eq? (quote get-args) msg)
imports::eq_p(&[Scm::symbol("get-args"),msg__493.clone()])}).is_true() {args__481.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__493.clone()])}).is_true() {body__480.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__493.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust__491.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___494.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__493.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner__487.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star___494.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
imports::error(&[Scm::from("Unknown message FIXLET"),msg__493.clone()])}}}})});Scm::anything();self__485.get()}}}}}}}}}}}.into()
}
pub fn make_minus_function_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let var__449 = args[0].clone();let args__448 = args[1].clone();let tail_p__450 = args[2].clone();{
// (letrec ((repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (free-vars (lambda () (args (quote free-vars)))) (gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FN-APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (args (quote free-vars)))) (set! gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FN-APPLICATION" msg))))) self))
{let [repr__447, transform__451, free_minus_vars__454, gen_minus_rust__455, self__453, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__453 = self__453.into_boxed();{let gen_minus_rust__455 = gen_minus_rust__455.into_boxed();{let free_minus_vars__454 = free_minus_vars__454.into_boxed();{let transform__451 = transform__451.into_boxed();{let repr__447 = repr__447.into_boxed();{repr__447.set({// Closure
let tail_p__450 = tail_p__450.clone();let var__449 = var__449.clone();let args__448 = args__448.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr)))
imports::list(&[if (tail_p__450.clone()).is_true() {Scm::symbol("FN-APPLY-TC")} else {Scm::symbol("FN-APPLY")},{
// (variable-name var)
imports::variable_minus_name(&[var__449.clone()])},{
// (args (quote repr))
args__448.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__451.set({// Closure
let self__453 = self__453.clone();let var__449 = var__449.clone();let args__448 = args__448.clone();let tail_p__450 = tail_p__450.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__452 = args[0].clone();{
// (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?)))
fnc__452.clone().invoke(&[self__453.get(),{// Closure
let var__449 = var__449.clone();let args__448 = args__448.clone();let fnc__452 = fnc__452.clone();let tail_p__450 = tail_p__450.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-function-application var (args (quote transform) fnc) tail?)
Scm::func(make_minus_function_minus_application).invoke(&[var__449.clone(),{
// (args (quote transform) fnc)
args__448.clone().invoke(&[Scm::symbol("transform"),fnc__452.clone()])},tail_p__450.clone()])}})}])}})});Scm::anything();free_minus_vars__454.set({// Closure
let args__448 = args__448.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (args (quote free-vars))
args__448.clone().invoke(&[Scm::symbol("free-vars")])}})});Scm::anything();gen_minus_rust__455.set({// Closure
let var__449 = var__449.clone();let args__448 = args__448.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__456 = args[0].clone();{{
// (cond ...)
if ({
// (global-function? var)
imports::global_minus_function_p(&[var__449.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__456.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__449.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module__456.clone(),Scm::from("imports::")])}} else {{
// (error "invalid function application" var)
imports::error(&[Scm::from("invalid function application"),var__449.clone()])}}};{
// (print module (rustify-identifier (variable-name var)) "(&[")
imports::print(&[module__456.clone(),{
// (rustify-identifier (variable-name var))
imports::rustify_minus_identifier(&[{
// (variable-name var)
imports::variable_minus_name(&[var__449.clone()])}])},Scm::from("(&[")])};{
// (args (quote gen-rust) module)
args__448.clone().invoke(&[Scm::symbol("gen-rust"),module__456.clone()])};{
// (print module "])")
imports::print(&[module__456.clone(),Scm::from("])")])}}})});Scm::anything();self__453.set({// Closure
let repr__447 = repr__447.clone();let transform__451 = transform__451.clone();let free_minus_vars__454 = free_minus_vars__454.clone();let gen_minus_rust__455 = gen_minus_rust__455.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__457 = args[0].clone();let args__458 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__457.clone()])}).is_true() {{
// (repr)
repr__447.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__457.clone()])}).is_true() {{
// (transform (car args))
transform__451.get().invoke(&[{
// (car args)
imports::car(&[args__458.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__457.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__454.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__457.clone()])}).is_true() {Scm::symbol("FN-APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__457.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__455.get().invoke(&[{
// (car args)
imports::car(&[args__458.clone()])}])}} else {{
// (error "Unknown message FN-APPLICATION" msg)
imports::error(&[Scm::from("Unknown message FN-APPLICATION"),msg__457.clone()])}}}})});Scm::anything();self__453.get()}}}}}}}}}}.into()
}
pub fn make_minus_import(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let lib__614 = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr__613, transform__615, free_minus_vars__618, gen_minus_libname__619, gen_minus_rust__622, self__616, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__616 = self__616.into_boxed();{let gen_minus_rust__622 = gen_minus_rust__622.into_boxed();{let gen_minus_libname__619 = gen_minus_libname__619.into_boxed();{let free_minus_vars__618 = free_minus_vars__618.into_boxed();{let transform__615 = transform__615.into_boxed();{let repr__613 = repr__613.into_boxed();{repr__613.set({// Closure
let lib__614 = lib__614.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
imports::cons(&[Scm::symbol("IMPORT"),lib__614.clone()])}})});Scm::anything();transform__615.set({// Closure
let self__616 = self__616.clone();let lib__614 = lib__614.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__617 = args[0].clone();{
// (func self (lambda () (make-import lib)))
func__617.clone().invoke(&[self__616.get(),{// Closure
let lib__614 = lib__614.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
Scm::func(make_minus_import).invoke(&[lib__614.clone()])}})}])}})});Scm::anything();free_minus_vars__618.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname__619.set({// Closure
let gen_minus_libname__619 = gen_minus_libname__619.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__621 = args[0].clone();let lib__620 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib__620.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__621.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module__621.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib__620.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib__620.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module__621.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module__621.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname__619.get().invoke(&[module__621.clone(),{
// (cdr lib)
imports::cdr(&[lib__620.clone()])}])}}}})});Scm::anything();gen_minus_rust__622.set({// Closure
let gen_minus_libname__619 = gen_minus_libname__619.clone();let lib__614 = lib__614.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__623 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module__623.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname__619.get().invoke(&[module__623.clone(),lib__614.clone()])};{
// (print module "::exports::*;")
imports::print(&[module__623.clone(),Scm::from("::exports::*;")])};{
// (println module)
imports::println(&[module__623.clone()])}}})});Scm::anything();self__616.set({// Closure
let repr__613 = repr__613.clone();let transform__615 = transform__615.clone();let free_minus_vars__618 = free_minus_vars__618.clone();let gen_minus_rust__622 = gen_minus_rust__622.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__624 = args[0].clone();let args__625 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__624.clone()])}).is_true() {{
// (repr)
repr__613.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__624.clone()])}).is_true() {{
// (transform (car args))
transform__615.get().invoke(&[{
// (car args)
imports::car(&[args__625.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__624.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__618.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__624.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__624.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__622.get().invoke(&[{
// (car args)
imports::car(&[args__625.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg__624.clone()])}}}})});Scm::anything();self__616.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import_minus_only(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let lib__628 = args[0].clone();let names__627 = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr__626, transform__629, free_minus_vars__632, gen_minus_libname__633, gen_minus_imports__636, gen_minus_rust__639, self__630, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__630 = self__630.into_boxed();{let gen_minus_rust__639 = gen_minus_rust__639.into_boxed();{let gen_minus_imports__636 = gen_minus_imports__636.into_boxed();{let gen_minus_libname__633 = gen_minus_libname__633.into_boxed();{let free_minus_vars__632 = free_minus_vars__632.into_boxed();{let transform__629 = transform__629.into_boxed();{let repr__626 = repr__626.into_boxed();{repr__626.set({// Closure
let lib__628 = lib__628.clone();let names__627 = names__627.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
imports::cons(&[lib__628.clone(),names__627.clone()])}])}})});Scm::anything();transform__629.set({// Closure
let self__630 = self__630.clone();let lib__628 = lib__628.clone();let names__627 = names__627.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__631 = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func__631.clone().invoke(&[self__630.get(),{// Closure
let lib__628 = lib__628.clone();let names__627 = names__627.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
Scm::func(make_minus_import_minus_only).invoke(&[lib__628.clone(),names__627.clone()])}})}])}})});Scm::anything();free_minus_vars__632.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_libname__633.set({// Closure
let gen_minus_libname__633 = gen_minus_libname__633.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__635 = args[0].clone();let lib__634 = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib__634.clone()])}).is_true() {{
// (print module "")
imports::print(&[module__635.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module__635.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib__634.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib__634.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module__635.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module__635.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname__633.get().invoke(&[module__635.clone(),{
// (cdr lib)
imports::cdr(&[lib__634.clone()])}])}}}})});Scm::anything();gen_minus_imports__636.set({// Closure
let gen_minus_imports__636 = gen_minus_imports__636.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module__638 = args[0].clone();let names__637 = args[1].clone();if ({
// (null? names)
imports::null_p(&[names__637.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
imports::print(&[module__638.clone(),{
// (rustify-identifier (car names))
imports::rustify_minus_identifier(&[{
// (car names)
imports::car(&[names__637.clone()])}])}])};{
// (print module ", ")
imports::print(&[module__638.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports__636.get().invoke(&[module__638.clone(),{
// (cdr names)
imports::cdr(&[names__637.clone()])}])}}}})});Scm::anything();gen_minus_rust__639.set({// Closure
let gen_minus_libname__633 = gen_minus_libname__633.clone();let lib__628 = lib__628.clone();let gen_minus_imports__636 = gen_minus_imports__636.clone();let names__627 = names__627.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__640 = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module__640.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname__633.get().invoke(&[module__640.clone(),lib__628.clone()])};{
// (print module "::exports::{")
imports::print(&[module__640.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports__636.get().invoke(&[module__640.clone(),names__627.clone()])};{
// (print module "};")
imports::print(&[module__640.clone(),Scm::from("};")])};{
// (println module)
imports::println(&[module__640.clone()])}}})});Scm::anything();self__630.set({// Closure
let repr__626 = repr__626.clone();let transform__629 = transform__629.clone();let free_minus_vars__632 = free_minus_vars__632.clone();let gen_minus_rust__639 = gen_minus_rust__639.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__641 = args[0].clone();let args__642 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__641.clone()])}).is_true() {{
// (repr)
repr__626.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__641.clone()])}).is_true() {{
// (transform (car args))
transform__629.get().invoke(&[{
// (car args)
imports::car(&[args__642.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__641.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__632.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__641.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__641.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__639.get().invoke(&[{
// (car args)
imports::car(&[args__642.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg__641.clone()])}}}})});Scm::anything();self__630.get()}}}}}}}}}}}}.into()
}
pub fn make_minus_library(args: &[Scm]) -> Scm {
    {
        if args.len() != 6 {
            panic!("invalid arity")
        }
        let name__562 = args[0].clone();
        let globals__561 = args[1].clone();
        let init__560 = args[2].clone();
        let body__565 = args[3].clone();
        let imports__558 = args[4].clone();
        let exports__557 = args[5].clone();
        {
            // (let* ((tests (list (quote dummy))) (new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (_make-library name globals init new-body imports exports (cdr tests)))
            {
                // (let ((tests (list (quote dummy)))) (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests)))))
                {
                    let tests__556 = {
                        // (list (quote dummy))
                        imports::list(&[Scm::symbol("dummy")])
                    };
                    // (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests))))
                    let new_minus_body__559 = {
                        // (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
                        body__565.clone().invoke(&[Scm::symbol("transform"), {
                            // Closure
                            let tests__556 = tests__556.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node__564 = args[0].clone();
                                let ignore__563 = args[1].clone();
                                if ({
                                    // (eq? (node (quote kind)) (quote TESTSUITE))
                                    imports::eq_p(&[
                                        {
                                            // (node (quote kind))
                                            node__564.clone().invoke(&[Scm::symbol("kind")])
                                        },
                                        Scm::symbol("TESTSUITE"),
                                    ])
                                })
                                .is_true()
                                {
                                    {
                                        {
                                            // (set-cdr! tests (cons node (cdr tests)))
                                            imports::set_minus_cdr_i(&[tests__556.clone(), {
                                                // (cons node (cdr tests))
                                                imports::cons(&[node__564.clone(), {
                                                    // (cdr tests)
                                                    imports::cdr(&[tests__556.clone()])
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
                                        ignore__563.clone().invoke(&[])
                                    }
                                }
                            })
                        }])
                    };
                    {
                        // (_make-library name globals init new-body imports exports (cdr tests))
                        Scm::func(__make_minus_library).invoke(&[
                            name__562.clone(),
                            globals__561.clone(),
                            init__560.clone(),
                            new_minus_body__559.clone(),
                            imports__558.clone(),
                            exports__557.clone(),
                            {
                                // (cdr tests)
                                imports::cdr(&[tests__556.clone()])
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
    {if args.len() != 0{panic!("invalid arity")}{
// (letrec ((repr (lambda () (quote (NOP)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module "(/*NOP*/)"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (quote (NOP)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module "(/*NOP*/)"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))) self))
{let [repr__351, transform__352, free_minus_vars__355, gen_minus_rust__356, self__353, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__353 = self__353.into_boxed();{let gen_minus_rust__356 = gen_minus_rust__356.into_boxed();{let free_minus_vars__355 = free_minus_vars__355.into_boxed();{let transform__352 = transform__352.into_boxed();{let repr__351 = repr__351.into_boxed();{repr__351.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}Scm::pair(Scm::symbol("NOP"), Scm::Nil)})});Scm::anything();transform__352.set({// Closure
let self__353 = self__353.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__354 = args[0].clone();{
// (func self (lambda () self))
func__354.clone().invoke(&[self__353.get(),{// Closure
let self__353 = self__353.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__353.get()})}])}})});Scm::anything();free_minus_vars__355.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__356.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__357 = args[0].clone();{
// (print module "(/*NOP*/)")
imports::print(&[module__357.clone(),Scm::from("(/*NOP*/)")])}})});Scm::anything();self__353.set({// Closure
let repr__351 = repr__351.clone();let transform__352 = transform__352.clone();let free_minus_vars__355 = free_minus_vars__355.clone();let gen_minus_rust__356 = gen_minus_rust__356.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__358 = args[0].clone();let args__359 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__358.clone()])}).is_true() {{
// (repr)
repr__351.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__358.clone()])}).is_true() {{
// (transform (car args))
transform__352.get().invoke(&[{
// (car args)
imports::car(&[args__359.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__358.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__355.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__358.clone()])}).is_true() {Scm::symbol("NOP")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__358.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__356.get().invoke(&[{
// (car args)
imports::car(&[args__359.clone()])}])}} else {{
// (error "Unknown message NOP" msg)
imports::error(&[Scm::from("Unknown message NOP"),msg__358.clone()])}}}})});Scm::anything();self__353.get()}}}}}}}}}}.into()
}
pub fn make_minus_null_minus_arg(args: &[Scm]) -> Scm {
    {if args.len() != 0{panic!("invalid arity")}{
// (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (print module ""))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote NULL-ARG)))) (set! transform (lambda (fnc) (fnc self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (print module ""))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))) self))
{let [repr__459, transform__460, free_minus_vars__463, gen_minus_rust__464, self__461, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__461 = self__461.into_boxed();{let gen_minus_rust__464 = gen_minus_rust__464.into_boxed();{let free_minus_vars__463 = free_minus_vars__463.into_boxed();{let transform__460 = transform__460.into_boxed();{let repr__459 = repr__459.into_boxed();{repr__459.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote NULL-ARG))
imports::list(&[Scm::symbol("NULL-ARG")])}})});Scm::anything();transform__460.set({// Closure
let self__461 = self__461.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc__462 = args[0].clone();{
// (fnc self (lambda () self))
fnc__462.clone().invoke(&[self__461.get(),{// Closure
let self__461 = self__461.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__461.get()})}])}})});Scm::anything();free_minus_vars__463.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__464.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__465 = args[0].clone();{
// (print module "")
imports::print(&[module__465.clone(),Scm::from("")])}})});Scm::anything();self__461.set({// Closure
let repr__459 = repr__459.clone();let transform__460 = transform__460.clone();let free_minus_vars__463 = free_minus_vars__463.clone();let gen_minus_rust__464 = gen_minus_rust__464.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__466 = args[0].clone();let args__467 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__466.clone()])}).is_true() {{
// (repr)
repr__459.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__466.clone()])}).is_true() {{
// (transform (car args))
transform__460.get().invoke(&[{
// (car args)
imports::car(&[args__467.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__466.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__463.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__466.clone()])}).is_true() {Scm::symbol("NULL-ARG")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__466.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__464.get().invoke(&[{
// (car args)
imports::car(&[args__467.clone()])}])}} else {{
// (error "Unknown message NULL-ARG" msg)
imports::error(&[Scm::from("Unknown message NULL-ARG"),msg__466.clone()])}}}})});Scm::anything();self__461.get()}}}}}}}}}}.into()
}
pub fn make_minus_program(args: &[Scm]) -> Scm {
    {if args.len() != 5{panic!("invalid arity")}let globals__541 = args[0].clone();let imports__540 = args[1].clone();let init__545 = args[2].clone();let body__539 = args[3].clone();let libraries__543 = args[4].clone();{
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (set! transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries))))) (set! gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))) self))
{let [repr__538, transform__542, gen_minus_imports__547, gen_minus_rust__550, self__546, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__546 = self__546.into_boxed();{let gen_minus_rust__550 = gen_minus_rust__550.into_boxed();{let gen_minus_imports__547 = gen_minus_imports__547.into_boxed();{let transform__542 = transform__542.into_boxed();{let repr__538 = repr__538.into_boxed();{repr__538.set({// Closure
let globals__541 = globals__541.clone();let imports__540 = imports__540.clone();let body__539 = body__539.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
imports::cons(&[globals__541.clone(),{
// (cons imports (body (quote repr)))
imports::cons(&[imports__540.clone(),{
// (body (quote repr))
body__539.clone().invoke(&[Scm::symbol("repr")])}])}])}])}})});Scm::anything();transform__542.set({// Closure
let self__546 = self__546.clone();let globals__541 = globals__541.clone();let imports__540 = imports__540.clone();let init__545 = init__545.clone();let body__539 = body__539.clone();let libraries__543 = libraries__543.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__544 = args[0].clone();{
// (func self (lambda () (make-program globals imports init (body (quote transform) func) libraries)))
func__544.clone().invoke(&[self__546.get(),{// Closure
let globals__541 = globals__541.clone();let imports__540 = imports__540.clone();let init__545 = init__545.clone();let body__539 = body__539.clone();let func__544 = func__544.clone();let libraries__543 = libraries__543.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-program globals imports init (body (quote transform) func) libraries)
Scm::func(make_minus_program).invoke(&[globals__541.clone(),imports__540.clone(),init__545.clone(),{
// (body (quote transform) func)
body__539.clone().invoke(&[Scm::symbol("transform"),func__544.clone()])},libraries__543.clone()])}})}])}})});Scm::anything();gen_minus_imports__547.set({// Closure
let imports__540 = imports__540.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__548 = args[0].clone();{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module__548 = module__548.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i__549 = args[0].clone();{
// (i (quote gen-rust) module)
i__549.clone().invoke(&[Scm::symbol("gen-rust"),module__548.clone()])}})},imports__540.clone()])}})});Scm::anything();gen_minus_rust__550.set({// Closure
let gen_minus_imports__547 = gen_minus_imports__547.clone();let globals__541 = globals__541.clone();let init__545 = init__545.clone();let body__539 = body__539.clone();let libraries__543 = libraries__543.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__551 = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println(&[module__551.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
imports::print(&[module__551.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block(&[module__551.clone(),{// Closure
let gen_minus_imports__547 = gen_minus_imports__547.clone();let module__551 = module__551.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports__547.get().invoke(&[module__551.clone()])}})}])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module__551.clone(),globals__541.clone()])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (print module "pub fn main()")
imports::print(&[module__551.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block(&[module__551.clone(),{// Closure
let module__551 = module__551.clone();let init__545 = init__545.clone();let body__539 = body__539.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module__551.clone()])};{
// (println module "eprintln!(\"built with\");")
imports::println(&[module__551.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println(&[module__551.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each(&[{// Closure
let module__551 = module__551.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib__552 = args[0].clone();{{
// (print module "crate::")
imports::print(&[module__551.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each(&[{// Closure
let module__551 = module__551.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l__553 = args[0].clone();{{
// (print module (rustify-libname l))
imports::print(&[module__551.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l__553.clone()])}])};{
// (print module "::")
imports::print(&[module__551.clone(),Scm::from("::")])}}})},lib__552.clone()])};{
// (print module "initialize();")
imports::print(&[module__551.clone(),Scm::from("initialize();")])};{
// (println module)
imports::println(&[module__551.clone()])}}})},init__545.clone()])};{
// (body (quote gen-rust) module)
body__539.clone().invoke(&[Scm::symbol("gen-rust"),module__551.clone()])};{
// (println module ";")
imports::println(&[module__551.clone(),Scm::from(";")])}}})}])};{
// (println module)
imports::println(&[module__551.clone()])};{
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules(&[module__551.clone(),libraries__543.clone()])}}})});Scm::anything();self__546.set({// Closure
let repr__538 = repr__538.clone();let transform__542 = transform__542.clone();let gen_minus_rust__550 = gen_minus_rust__550.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__554 = args[0].clone();let args__555 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__554.clone()])}).is_true() {{
// (repr)
repr__538.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__554.clone()])}).is_true() {{
// (transform (car args))
transform__542.get().invoke(&[{
// (car args)
imports::car(&[args__555.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__554.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__554.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__550.get().invoke(&[{
// (car args)
imports::car(&[args__555.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
imports::error(&[Scm::from("Unknown message PROGRAM"),msg__554.clone()])}}}})});Scm::anything();self__546.get()}}}}}}}}}}.into()
}
pub fn make_minus_reference(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let var__374 = args[0].clone();{
// (letrec ((repr (lambda () (list (quote GET) (variable-name var)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) (variable-name var)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr__373, transform__375, free_minus_vars__378, gen_minus_rust__379, self__376, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__376 = self__376.into_boxed();{let gen_minus_rust__379 = gen_minus_rust__379.into_boxed();{let free_minus_vars__378 = free_minus_vars__378.into_boxed();{let transform__375 = transform__375.into_boxed();{let repr__373 = repr__373.into_boxed();{repr__373.set({// Closure
let var__374 = var__374.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) (variable-name var))
imports::list(&[Scm::symbol("GET"),{
// (variable-name var)
imports::variable_minus_name(&[var__374.clone()])}])}})});Scm::anything();transform__375.set({// Closure
let self__376 = self__376.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__377 = args[0].clone();{
// (func self (lambda () self))
func__377.clone().invoke(&[self__376.get(),{// Closure
let self__376 = self__376.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self__376.get()})}])}})});Scm::anything();free_minus_vars__378.set({// Closure
let var__374 = var__374.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var__374.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var__374.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var__374.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (make-set) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (make-set)
imports::make_minus_set(&[])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var__374.clone()])}])}])}}})});Scm::anything();gen_minus_rust__379.set({// Closure
let var__374 = var__374.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__381 = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))
{let name__380 = {
// (variable-name var)
imports::variable_minus_name(&[var__374.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var__374.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".with(|value| value.get())")
imports::print(&[module__381.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__380.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var__374.clone()])}).is_true() {{
// (print module "Scm::func(" (rustify-identifier name) ")")
imports::print(&[module__381.clone(),Scm::from("Scm::func("),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__380.clone()])},Scm::from(")")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var__374.clone()])}).is_true() {{
// (print module "Scm::func(imports::" (rustify-identifier name) ")")
imports::print(&[module__381.clone(),Scm::from("Scm::func(imports::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__380.clone()])},Scm::from(")")])}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var__374.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".get()")
imports::print(&[module__381.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__380.clone()])},Scm::from(".get()")])}} else {{
// (print module (rustify-identifier name) ".clone()")
imports::print(&[module__381.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name__380.clone()])},Scm::from(".clone()")])}}}}}})});Scm::anything();self__376.set({// Closure
let repr__373 = repr__373.clone();let transform__375 = transform__375.clone();let free_minus_vars__378 = free_minus_vars__378.clone();let gen_minus_rust__379 = gen_minus_rust__379.clone();let var__374 = var__374.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__382 = args[0].clone();let args__383 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__382.clone()])}).is_true() {{
// (repr)
repr__373.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__382.clone()])}).is_true() {{
// (transform (car args))
transform__375.get().invoke(&[{
// (car args)
imports::car(&[args__383.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__382.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__378.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__382.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__382.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__379.get().invoke(&[{
// (car args)
imports::car(&[args__383.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg__382.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var__374.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg__382.clone()])}).is_true() {var__374.clone()} else {{
// (error "Unknown message REFERENCE" msg)
imports::error(&[Scm::from("Unknown message REFERENCE"),msg__382.clone()])}}}})});Scm::anything();self__376.get()}}}}}}}}}}.into()
}
pub fn make_minus_sequence(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let first__422 = args[0].clone();let next__421 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr__420, transform__423, free_minus_vars__428, gen_minus_rust_minus_inner__429, gen_minus_rust__431, self__427, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__427 = self__427.into_boxed();{let gen_minus_rust__431 = gen_minus_rust__431.into_boxed();{let gen_minus_rust_minus_inner__429 = gen_minus_rust_minus_inner__429.into_boxed();{let free_minus_vars__428 = free_minus_vars__428.into_boxed();{let transform__423 = transform__423.into_boxed();{let repr__420 = repr__420.into_boxed();{repr__420.set({// Closure
let first__422 = first__422.clone();let next__421 = next__421.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list(&[Scm::symbol("SEQUENCE"),{
// (first (quote repr))
first__422.clone().invoke(&[Scm::symbol("repr")])},{
// (next (quote repr))
next__421.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__423.set({// Closure
let self__427 = self__427.clone();let next__421 = next__421.clone();let first__422 = first__422.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__426 = args[0].clone();{
// (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))))
func__426.clone().invoke(&[self__427.get(),{// Closure
let next__421 = next__421.clone();let func__426 = func__426.clone();let first__422 = first__422.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))
{
// (let ((a (first (quote transform) func))) (let ((b (next (quote transform) func))) (begin (make-sequence a b))))
{let a__425 = {
// (first (quote transform) func)
first__422.clone().invoke(&[Scm::symbol("transform"),func__426.clone()])};
// (let ((b (next (quote transform) func))) (begin (make-sequence a b)))
let b__424 = {
// (next (quote transform) func)
next__421.clone().invoke(&[Scm::symbol("transform"),func__426.clone()])};{
// (make-sequence a b)
Scm::func(make_minus_sequence).invoke(&[a__425.clone(),b__424.clone()])}}}}})}])}})});Scm::anything();free_minus_vars__428.set({// Closure
let first__422 = first__422.clone();let next__421 = next__421.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (first (quote free-vars))
first__422.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next__421.clone().invoke(&[Scm::symbol("free-vars")])}])}})});Scm::anything();gen_minus_rust_minus_inner__429.set({// Closure
let first__422 = first__422.clone();let next__421 = next__421.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__430 = args[0].clone();{{
// (first (quote gen-rust) module)
first__422.clone().invoke(&[Scm::symbol("gen-rust"),module__430.clone()])};{
// (print module ";")
imports::print(&[module__430.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next__421.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next__421.clone().invoke(&[Scm::symbol("gen-rust-inner"),module__430.clone()])}} else {{
// (next (quote gen-rust) module)
next__421.clone().invoke(&[Scm::symbol("gen-rust"),module__430.clone()])}}}})});Scm::anything();gen_minus_rust__431.set({// Closure
let gen_minus_rust_minus_inner__429 = gen_minus_rust_minus_inner__429.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__432 = args[0].clone();{{
// (print module "{")
imports::print(&[module__432.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner__429.get().invoke(&[module__432.clone()])};{
// (print module "}")
imports::print(&[module__432.clone(),Scm::from("}")])}}})});Scm::anything();self__427.set({// Closure
let repr__420 = repr__420.clone();let transform__423 = transform__423.clone();let free_minus_vars__428 = free_minus_vars__428.clone();let gen_minus_rust__431 = gen_minus_rust__431.clone();let gen_minus_rust_minus_inner__429 = gen_minus_rust_minus_inner__429.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__433 = args[0].clone();let args__434 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__433.clone()])}).is_true() {{
// (repr)
repr__420.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__433.clone()])}).is_true() {{
// (transform (car args))
transform__423.get().invoke(&[{
// (car args)
imports::car(&[args__434.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__433.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__428.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__433.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__433.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__431.get().invoke(&[{
// (car args)
imports::car(&[args__434.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg__433.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner__429.get().invoke(&[{
// (car args)
imports::car(&[args__434.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
imports::error(&[Scm::from("Unknown message SEQUENCE"),msg__433.clone()])}}}})});Scm::anything();self__427.get()}}}}}}}}}}}.into()
}
pub fn make_minus_testcase(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let description__645 = args[0].clone();let body__644 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
{let [repr__643, transform__646, free_minus_vars__649, gen_minus_rust__650, self__648, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__648 = self__648.into_boxed();{let gen_minus_rust__650 = gen_minus_rust__650.into_boxed();{let free_minus_vars__649 = free_minus_vars__649.into_boxed();{let transform__646 = transform__646.into_boxed();{let repr__643 = repr__643.into_boxed();{repr__643.set({// Closure
let description__645 = description__645.clone();let body__644 = body__644.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTCASE) description body)
imports::list(&[Scm::symbol("TESTCASE"),description__645.clone(),body__644.clone()])}})});Scm::anything();transform__646.set({// Closure
let self__648 = self__648.clone();let description__645 = description__645.clone();let body__644 = body__644.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__647 = args[0].clone();{
// (func self (lambda () (make-testcase description (body (quote transform) func))))
func__647.clone().invoke(&[self__648.get(),{// Closure
let description__645 = description__645.clone();let body__644 = body__644.clone();let func__647 = func__647.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testcase description (body (quote transform) func))
Scm::func(make_minus_testcase).invoke(&[description__645.clone(),{
// (body (quote transform) func)
body__644.clone().invoke(&[Scm::symbol("transform"),func__647.clone()])}])}})}])}})});Scm::anything();free_minus_vars__649.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__650.set({// Closure
let description__645 = description__645.clone();let body__644 = body__644.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__651 = args[0].clone();{{
// (println module "#[test]")
imports::println(&[module__651.clone(),Scm::from("#[test]")])};{
// (println module "fn " (rustify-testname description) "() {")
imports::println(&[module__651.clone(),Scm::from("fn "),{
// (rustify-testname description)
imports::rustify_minus_testname(&[description__645.clone()])},Scm::from("() {")])};{
// (println module "super::initialize();")
imports::println(&[module__651.clone(),Scm::from("super::initialize();")])};{
// (body (quote gen-rust) module)
body__644.clone().invoke(&[Scm::symbol("gen-rust"),module__651.clone()])};{
// (println module "}")
imports::println(&[module__651.clone(),Scm::from("}")])}}})});Scm::anything();self__648.set({// Closure
let repr__643 = repr__643.clone();let transform__646 = transform__646.clone();let free_minus_vars__649 = free_minus_vars__649.clone();let gen_minus_rust__650 = gen_minus_rust__650.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__652 = args[0].clone();let args__653 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__652.clone()])}).is_true() {{
// (repr)
repr__643.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__652.clone()])}).is_true() {{
// (transform (car args))
transform__646.get().invoke(&[{
// (car args)
imports::car(&[args__653.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__652.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__649.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__652.clone()])}).is_true() {Scm::symbol("TESTCASE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__652.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__650.get().invoke(&[{
// (car args)
imports::car(&[args__653.clone()])}])}} else {{
// (error "Unknown message TESTCASE" msg)
imports::error(&[Scm::from("Unknown message TESTCASE"),msg__652.clone()])}}}})});Scm::anything();self__648.get()}}}}}}}}}}.into()
}
pub fn make_minus_testsuite(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name__656 = args[0].clone();let cases__655 = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr__654, transform__657, free_minus_vars__661, gen_minus_rust__662, self__660, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__660 = self__660.into_boxed();{let gen_minus_rust__662 = gen_minus_rust__662.into_boxed();{let free_minus_vars__661 = free_minus_vars__661.into_boxed();{let transform__657 = transform__657.into_boxed();{let repr__654 = repr__654.into_boxed();{repr__654.set({// Closure
let name__656 = name__656.clone();let cases__655 = cases__655.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
imports::list(&[Scm::symbol("TESTSUITE"),name__656.clone(),cases__655.clone()])}})});Scm::anything();transform__657.set({// Closure
let self__660 = self__660.clone();let name__656 = name__656.clone();let cases__655 = cases__655.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__658 = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func__658.clone().invoke(&[self__660.get(),{// Closure
let name__656 = name__656.clone();let func__658 = func__658.clone();let cases__655 = cases__655.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
Scm::func(make_minus_testsuite).invoke(&[name__656.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
imports::map(&[{// Closure
let func__658 = func__658.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c__659 = args[0].clone();{
// (c (quote transform) func)
c__659.clone().invoke(&[Scm::symbol("transform"),func__658.clone()])}})},cases__655.clone()])}])}})}])}})});Scm::anything();free_minus_vars__661.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});Scm::anything();gen_minus_rust__662.set({// Closure
let cases__655 = cases__655.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__663 = args[0].clone();{{
// (println module "#[cfg(test)]")
imports::println(&[module__663.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
imports::println(&[module__663.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
imports::println(&[module__663.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
imports::for_minus_each(&[{// Closure
let module__663 = module__663.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c__664 = args[0].clone();{
// (c (quote gen-rust) module)
c__664.clone().invoke(&[Scm::symbol("gen-rust"),module__663.clone()])}})},cases__655.clone()])};{
// (println module "}")
imports::println(&[module__663.clone(),Scm::from("}")])}}})});Scm::anything();self__660.set({// Closure
let repr__654 = repr__654.clone();let transform__657 = transform__657.clone();let free_minus_vars__661 = free_minus_vars__661.clone();let gen_minus_rust__662 = gen_minus_rust__662.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__665 = args[0].clone();let args__666 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__665.clone()])}).is_true() {{
// (repr)
repr__654.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__665.clone()])}).is_true() {{
// (transform (car args))
transform__657.get().invoke(&[{
// (car args)
imports::car(&[args__666.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__665.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__661.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__665.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__665.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__662.get().invoke(&[{
// (car args)
imports::car(&[args__666.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
imports::error(&[Scm::from("Unknown message TESTSUITE"),msg__665.clone()])}}}})});Scm::anything();self__660.get()}}}}}}}}}}.into()
}
pub fn make_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let vars__526 = args[0].clone();let varvar__525 = args[1].clone();let body__524 = args[2].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars)))))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vararg) msg) (variable-name varvar)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr__523, transform__527, free_minus_vars__530, gen_minus_rust__531, self__529, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self__529 = self__529.into_boxed();{let gen_minus_rust__531 = gen_minus_rust__531.into_boxed();{let free_minus_vars__530 = free_minus_vars__530.into_boxed();{let transform__527 = transform__527.into_boxed();{let repr__523 = repr__523.into_boxed();{repr__523.set({// Closure
let vars__526 = vars__526.clone();let varvar__525 = varvar__525.clone();let body__524 = body__524.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) vars varvar (body (quote repr)))
imports::list(&[Scm::symbol("VARARG-ABSTRACTION"),vars__526.clone(),varvar__525.clone(),{
// (body (quote repr))
body__524.clone().invoke(&[Scm::symbol("repr")])}])}})});Scm::anything();transform__527.set({// Closure
let self__529 = self__529.clone();let vars__526 = vars__526.clone();let varvar__525 = varvar__525.clone();let body__524 = body__524.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func__528 = args[0].clone();{
// (func self (lambda () (make-vararg-abstraction vars varvar (body (quote transform) func))))
func__528.clone().invoke(&[self__529.get(),{// Closure
let vars__526 = vars__526.clone();let varvar__525 = varvar__525.clone();let body__524 = body__524.clone();let func__528 = func__528.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-vararg-abstraction vars varvar (body (quote transform) func))
Scm::func(make_minus_vararg_minus_abstraction).invoke(&[vars__526.clone(),varvar__525.clone(),{
// (body (quote transform) func)
body__524.clone().invoke(&[Scm::symbol("transform"),func__528.clone()])}])}})}])}})});Scm::anything();free_minus_vars__530.set({// Closure
let body__524 = body__524.clone();let varvar__525 = varvar__525.clone();let vars__526 = vars__526.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name (cons varvar vars))))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body__524.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name (cons varvar vars)))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name (cons varvar vars))
imports::map(&[Scm::func(imports::variable_minus_name),{
// (cons varvar vars)
imports::cons(&[varvar__525.clone(),vars__526.clone()])}])}])}])}})});Scm::anything();gen_minus_rust__531.set({// Closure
let varvar__525 = varvar__525.clone();let vars__526 = vars__526.clone();let body__524 = body__524.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module__534 = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))))
{let gen_minus_params__532 = Scm::symbol("*uninitialized*");{let gen_minus_params__532 = gen_minus_params__532.into_boxed();{gen_minus_params__532.set({// Closure
let module__534 = module__534.clone();let gen_minus_params__532 = gen_minus_params__532.clone();let varvar__525 = varvar__525.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star___535 = args[0].clone();let k__533 = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star___535.clone()])}).is_true() {{{
// (print module "let " (rustify-identifier (variable-name (car p*))) " = args[" k "].clone();")
imports::print(&[module__534.clone(),Scm::from("let "),{
// (rustify-identifier (variable-name (car p*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car p*))
imports::variable_minus_name(&[{
// (car p*)
imports::car(&[p_star___535.clone()])}])}])},Scm::from(" = args["),k__533.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params__532.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star___535.clone()])},{
// (+ k 1)
imports::_plus_(&[k__533.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (rustify-identifier (variable-name varvar)) " = Scm::list(&args[" k "..]);")
imports::print(&[module__534.clone(),Scm::from("let "),{
// (rustify-identifier (variable-name varvar))
imports::rustify_minus_identifier(&[{
// (variable-name varvar)
imports::variable_minus_name(&[varvar__525.clone()])}])},Scm::from(" = Scm::list(&args["),k__533.clone(),Scm::from("..]);")])}}})});Scm::anything();{
// (rust-block module (lambda () (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}") (gen-params vars 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module__534.clone(),{// Closure
let module__534 = module__534.clone();let vars__526 = vars__526.clone();let gen_minus_params__532 = gen_minus_params__532.clone();let body__524 = body__524.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length vars) "{panic!(\"not enough args\")}")
imports::print(&[module__534.clone(),Scm::from("if args.len() < "),{
// (length vars)
imports::length(&[vars__526.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params vars 0)
gen_minus_params__532.get().invoke(&[vars__526.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body__524.clone().invoke(&[Scm::symbol("gen-rust"),module__534.clone()])}}})}])}}}}}}})});Scm::anything();self__529.set({// Closure
let repr__523 = repr__523.clone();let transform__527 = transform__527.clone();let free_minus_vars__530 = free_minus_vars__530.clone();let gen_minus_rust__531 = gen_minus_rust__531.clone();let vars__526 = vars__526.clone();let varvar__525 = varvar__525.clone();let body__524 = body__524.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg__536 = args[0].clone();let args__537 = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg__536.clone()])}).is_true() {{
// (repr)
repr__523.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg__536.clone()])}).is_true() {{
// (transform (car args))
transform__527.get().invoke(&[{
// (car args)
imports::car(&[args__537.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg__536.clone()])}).is_true() {{
// (free-vars)
free_minus_vars__530.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg__536.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg__536.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust__531.get().invoke(&[{
// (car args)
imports::car(&[args__537.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg__536.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars__526.clone()])}} else if ({
// (eq? (quote get-vararg) msg)
imports::eq_p(&[Scm::symbol("get-vararg"),msg__536.clone()])}).is_true() {{
// (variable-name varvar)
imports::variable_minus_name(&[varvar__525.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg__536.clone()])}).is_true() {vars__526.clone()} else if ({
// (eq? (quote get-varvar) msg)
imports::eq_p(&[Scm::symbol("get-varvar"),msg__536.clone()])}).is_true() {varvar__525.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg__536.clone()])}).is_true() {body__524.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg__536.clone()])}}}})});Scm::anything();self__529.get()}}}}}}}}}}.into()
}
pub fn procedure_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj__337 = args[0].clone();
        if ({
            // (eq? (obj (quote kind)) (quote ABSTRACTION))
            imports::eq_p(&[
                {
                    // (obj (quote kind))
                    obj__337.clone().invoke(&[Scm::symbol("kind")])
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
                        obj__337.clone().invoke(&[Scm::symbol("kind")])
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
            // (define (make-boxify name body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-export env name exname) ...)
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
