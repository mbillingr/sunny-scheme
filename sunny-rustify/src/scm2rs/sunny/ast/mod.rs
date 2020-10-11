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
    {if args.len() != 7{panic!("invalid arity")}let name = args[0].clone();let globals = args[1].clone();let init = args[2].clone();let body = args[3].clone();let imports = args[4].clone();let exports = args[5].clone();let testsuite = args[6].clone();{
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-exports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite)))))) (set! gen-exports (lambda (module exports) (for-each (lambda (expo) (expo (quote gen-rust) module)) exports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (print module "mod imports") (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (println module) (println module) (print module "pub mod exports") (rust-block module (lambda () (gen-exports module exports))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (if (eq? (quote NOP) (body (quote kind))) (println module "pub fn initialize() {") (begin (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (println module) (println module "pub fn initialize() {") (println module "if INITIALIZED.with(|x| x.get()) { return }") (println module "INITIALIZED.with(|x| x.set(true));") (println module))) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init) (body (quote gen-rust) module) (println module ";}") (for-each (lambda (test) (test (quote gen-rust) module)) testsuite))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote LIBRARY)) ((eq? (quote libname) msg) name) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message LIBRARY" msg))))) self))
{let [repr, transform, gen_minus_exports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_exports = gen_minus_exports.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let exports = exports.clone();let imports = imports.clone();let globals = globals.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
imports::append(&[Scm::symbol("LIBRARY"),name.clone(),exports.clone(),imports.clone(),globals.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let imports = imports.clone();let exports = exports.clone();let testsuite = testsuite.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let func = func.clone();let imports = imports.clone();let exports = exports.clone();let testsuite = testsuite.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (_make-library name globals init (body (quote transform) func) imports exports (map (lambda (t) (t (quote transform) func)) testsuite))
Scm::func(__make_minus_library).invoke(&[name.clone(),globals.clone(),init.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])},imports.clone(),exports.clone(),{
// (map (lambda (t) (t (quote transform) func)) testsuite)
imports::map(&[{// Closure
let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let t = args[0].clone();{
// (t (quote transform) func)
t.clone().invoke(&[Scm::symbol("transform"),func.clone()])}})},testsuite.clone()])}])}})}])}})});gen_minus_exports.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let exports = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo = args[0].clone();{
// (expo (quote gen-rust) module)
expo.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},exports.clone()])}})});gen_minus_rust.set({// Closure
let imports = imports.clone();let gen_minus_exports = gen_minus_exports.clone();let exports = exports.clone();let globals = globals.clone();let body = body.clone();let init = init.clone();let testsuite = testsuite.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
imports::println(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
imports::print(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})}])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (print module "pub mod exports")
imports::print(&[module.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
imports::rust_minus_block(&[module.clone(),{// Closure
let gen_minus_exports = gen_minus_exports.clone();let module = module.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports.get().invoke(&[module.clone(),exports.clone()])}})}])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module.clone(),globals.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module)
imports::println(&[module.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p(&[Scm::symbol("NOP"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
imports::println(&[module.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
imports::println(&[module.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module "pub fn initialize() {")
imports::println(&[module.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
imports::println(&[module.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
imports::println(&[module.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
imports::println(&[module.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
imports::print(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{
// (print module (rustify-libname l) "::")
imports::print(&[module.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l.clone()])},Scm::from("::")])}})},lib.clone()])};{
// (println module "initialize();")
imports::println(&[module.clone(),Scm::from("initialize();")])}}})},init.clone()])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";}")
imports::println(&[module.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) testsuite)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test = args[0].clone();{
// (test (quote gen-rust) module)
test.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},testsuite.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let name = name.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
imports::eq_p(&[Scm::symbol("libname"),msg.clone()])}).is_true() {name.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
imports::error(&[Scm::from("Unknown message LIBRARY"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn ast_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        {
            // (procedure? obj)
            imports::procedure_p(&[obj.clone()])
        }
    }
    .into()
}
pub fn free_minus_var_minus_name(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        {
            // (cond ...)
            if ({
                // (symbol? name)
                imports::symbol_p(&[name.clone()])
            })
            .is_true()
            {
                name.clone()
            } else if ({
                // (string? name)
                imports::string_p(&[name.clone()])
            })
            .is_true()
            {
                {
                    // (string->symbol name)
                    imports::string_minus__g_symbol(&[name.clone()])
                }
            } else {
                {
                    // (error "Invalid variable name" name)
                    imports::error(&[Scm::from("Invalid variable name"), name.clone()])
                }
            }
        }
    }
    .into()
}
pub fn make_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let body = args[2].clone();{
// (letrec ((repr (lambda () (list (quote ABSTRACTION) params (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ABSTRACTION) params (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote ABSTRACTION) params (body (quote repr)))
imports::list(&[Scm::symbol("ABSTRACTION"),params.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-abstraction params vars (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let params = params.clone();let vars = vars.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-abstraction params vars (body (quote transform) func))
Scm::func(make_minus_abstraction).invoke(&[params.clone(),vars.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let body = body.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) params)
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},params.clone()])}})});gen_minus_rust.set({// Closure
let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star_.clone()])}).is_true() {{{
// (print module "let ")
imports::print(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (car p*)))
imports::print(&[module.clone(),{
// (rustify-identifier (car p*))
imports::rustify_minus_identifier(&[{
// (car p*)
imports::car(&[p_star_.clone()])}])}])};{
// (print module " = args[")
imports::print(&[module.clone(),Scm::from(" = args[")])};{
// (print module k)
imports::print(&[module.clone(),k.clone()])};{
// (print module "].clone();")
imports::print(&[module.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star_.clone()])},{
// (+ k 1)
imports::_plus_(&[k.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
imports::print(&[module.clone(),Scm::from("if args.len() != ")])};{
// (print module (length params))
imports::print(&[module.clone(),{
// (length params)
imports::length(&[params.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
imports::print(&[module.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_alternative(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let condition = args[0].clone();let consequent = args[1].clone();let alternative = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list(&[Scm::symbol("IF"),{
// (condition (quote repr))
condition.clone().invoke(&[Scm::symbol("repr")])},{
// (consequent (quote repr))
consequent.clone().invoke(&[Scm::symbol("repr")])},{
// (alternative (quote repr))
alternative.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let condition = condition.clone();let func = func.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
Scm::func(make_minus_alternative).invoke(&[{
// (condition (quote transform) func)
condition.clone().invoke(&[Scm::symbol("transform"),func.clone()])},{
// (consequent (quote transform) func)
consequent.clone().invoke(&[Scm::symbol("transform"),func.clone()])},{
// (alternative (quote transform) func)
alternative.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
imports::set_minus_union(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
imports::set_minus_union(&[{
// (condition (quote free-vars))
condition.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({// Closure
let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "if (")
imports::print(&[module.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ").is_true() {")
imports::print(&[module.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "} else ")
imports::print(&[module.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
imports::eq_p(&[{
// (alternative (quote kind))
alternative.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}} else {{{
// (print module "{")
imports::print(&[module.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "}")
imports::print(&[module.clone(),Scm::from("}")])}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
imports::error(&[Scm::from("Unknown message ALTERNATIVE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let func = args[0].clone();let args_ = args[1].clone();let tail_p = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let tail_p = tail_p.clone();let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons(&[if (tail_p.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
imports::cons(&[{
// (func (quote repr))
func.clone().invoke(&[Scm::symbol("repr")])},{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])}])}])}})});transform.set({// Closure
let self_ = self_.clone();let func = func.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc.clone().invoke(&[self_.get(),{// Closure
let func = func.clone();let fnc = fnc.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
Scm::func(make_minus_application).invoke(&[{
// (func (quote transform) fnc)
func.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},tail_p.clone()])}})}])}})});free_minus_vars.set({// Closure
let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
imports::set_minus_union(&[{
// (func (quote free-vars))
func.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({// Closure
let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (func (quote gen-rust) module)
func.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ".invoke(&[")
imports::print(&[module.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "])")
imports::print(&[module.clone(),Scm::from("])")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
imports::error(&[Scm::from("Unknown message APPLICATION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_args(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let arg = args[0].clone();
        let next = args[1].clone();
        {
            // (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote ARG) (cons arg next)))) (set! transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (set! gen-rust (lambda (module) (arg (quote gen-rust) module) (if (not (eq? (quote NULL-ARG) (next (quote kind)))) (begin (print module ",") (next (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))) self))
                {
                    let [repr, transform, free_minus_vars, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let free_minus_vars = free_minus_vars.into_boxed();
                                {
                                    let transform = transform.into_boxed();
                                    {
                                        let repr = repr.into_boxed();
                                        {
                                            repr.set({
                                                // Closure
                                                let arg = arg.clone();
                                                let next = next.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (cons (quote ARG) (cons arg next))
                                                        imports::cons(&[Scm::symbol("ARG"), {
                                                            // (cons arg next)
                                                            imports::cons(&[
                                                                arg.clone(),
                                                                next.clone(),
                                                            ])
                                                        }])
                                                    }
                                                })
                                            });
                                            transform.set({
                                                // Closure
                                                let self_ = self_.clone();
                                                let arg = arg.clone();
                                                let next = next.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let fnc = args[0].clone();
                                                    {
                                                        // (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
                                                        fnc.clone().invoke(&[self_.get(), {
                                                            // Closure
                                                            let arg = arg.clone();
                                                            let fnc = fnc.clone();
                                                            let next = next.clone();
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
                                                                                arg.clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    fnc.clone(),
                                                                                ])
                                                                            },
                                                                            {
                                                                                // (next (quote transform) fnc)
                                                                                next.clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    fnc.clone(),
                                                                                ])
                                                                            },
                                                                        ])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            free_minus_vars.set({
                                                // Closure
                                                let arg = arg.clone();
                                                let next = next.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (set-union (arg (quote free-vars)) (next (quote free-vars)))
                                                        imports::set_minus_union(&[
                                                            {
                                                                // (arg (quote free-vars))
                                                                arg.clone().invoke(&[Scm::symbol(
                                                                    "free-vars",
                                                                )])
                                                            },
                                                            {
                                                                // (next (quote free-vars))
                                                                next.clone().invoke(&[Scm::symbol(
                                                                    "free-vars",
                                                                )])
                                                            },
                                                        ])
                                                    }
                                                })
                                            });
                                            gen_minus_rust.set({
                                                // Closure
                                                let arg = arg.clone();
                                                let next = next.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module = args[0].clone();
                                                    {
                                                        {
                                                            // (arg (quote gen-rust) module)
                                                            arg.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module.clone(),
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
                                                                        next.clone().invoke(&[
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
                                                                        module.clone(),
                                                                        Scm::from(","),
                                                                    ])
                                                                };
                                                                {
                                                                    // (next (quote gen-rust) module)
                                                                    next.clone().invoke(&[
                                                                        Scm::symbol("gen-rust"),
                                                                        module.clone(),
                                                                    ])
                                                                }
                                                            }
                                                        } else {
                                                            Scm::symbol("*UNSPECIFIED*")
                                                        }
                                                    }
                                                })
                                            });
                                            self_.set({
                                                // Closure
                                                let repr = repr.clone();
                                                let transform = transform.clone();
                                                let free_minus_vars = free_minus_vars.clone();
                                                let gen_minus_rust = gen_minus_rust.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg = args[0].clone();
                                                    let args_ = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ARG" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message ARG",
                                                                    ),
                                                                    msg.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            self_.get()
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
        let condition = args[0].clone();
        {
            // (letrec ((repr (lambda () (list (quote ASSERT) condition))) (transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (free-vars (lambda () (condition (quote free-vars)))) (gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote ASSERT) condition))) (set! transform (lambda (func) (func self (lambda () (make-assert (condition (quote transform) func)))))) (set! free-vars (lambda () (condition (quote free-vars)))) (set! gen-rust (lambda (module) (print module "assert!(") (condition (quote gen-rust) module) (println module ".is_true());"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSERT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSERT" msg))))) self))
                {
                    let [repr, transform, free_minus_vars, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let free_minus_vars = free_minus_vars.into_boxed();
                                {
                                    let transform = transform.into_boxed();
                                    {
                                        let repr = repr.into_boxed();
                                        {
                                            repr.set({
                                                // Closure
                                                let condition = condition.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (list (quote ASSERT) condition)
                                                        imports::list(&[
                                                            Scm::symbol("ASSERT"),
                                                            condition.clone(),
                                                        ])
                                                    }
                                                })
                                            });
                                            transform.set({
                                                // Closure
                                                let self_ = self_.clone();
                                                let condition = condition.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func = args[0].clone();
                                                    {
                                                        // (func self (lambda () (make-assert (condition (quote transform) func))))
                                                        func.clone().invoke(&[self_.get(), {
                                                            // Closure
                                                            let condition = condition.clone();
                                                            let func = func.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                {
                                                                    // (make-assert (condition (quote transform) func))
                                                                    Scm::func(make_minus_assert)
                                                                        .invoke(&[{
                                                                            // (condition (quote transform) func)
                                                                            condition
                                                                                .clone()
                                                                                .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    func.clone(),
                                                                                ])
                                                                        }])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            free_minus_vars.set({
                                                // Closure
                                                let condition = condition.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (condition (quote free-vars))
                                                        condition
                                                            .clone()
                                                            .invoke(&[Scm::symbol("free-vars")])
                                                    }
                                                })
                                            });
                                            gen_minus_rust.set({
                                                // Closure
                                                let condition = condition.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module = args[0].clone();
                                                    {
                                                        {
                                                            // (print module "assert!(")
                                                            imports::print(&[
                                                                module.clone(),
                                                                Scm::from("assert!("),
                                                            ])
                                                        };
                                                        {
                                                            // (condition (quote gen-rust) module)
                                                            condition.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module.clone(),
                                                            ])
                                                        };
                                                        {
                                                            // (println module ".is_true());")
                                                            imports::println(&[
                                                                module.clone(),
                                                                Scm::from(".is_true());"),
                                                            ])
                                                        }
                                                    }
                                                })
                                            });
                                            self_.set({
                                                // Closure
                                                let repr = repr.clone();
                                                let transform = transform.clone();
                                                let free_minus_vars = free_minus_vars.clone();
                                                let gen_minus_rust = gen_minus_rust.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg = args[0].clone();
                                                    let args_ = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ASSERT")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ASSERT" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message ASSERT",
                                                                    ),
                                                                    msg.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            self_.get()
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
    {if args.len() != 2{panic!("invalid arity")}let var = args[0].clone();let val = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var)))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var)))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("SET!"),{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])},{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-assignment var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment var (val (quote transform) func))
Scm::func(make_minus_assignment).invoke(&[var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let val = val.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])}])}})});gen_minus_rust.set({// Closure
let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var))))
{let name = {
// (variable-name var)
imports::variable_minus_name(&[var.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print(&[module.clone(),Scm::from("))")])}}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".set(")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
imports::print(&[module.clone(),Scm::from(")")])}}} else {{
// (error "set! on unboxed variable" name var)
imports::error(&[Scm::from("set! on unboxed variable"),name.clone(),var.clone()])}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message ASSIGNMENT" msg)
imports::error(&[Scm::from("Unknown message ASSIGNMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_boxify(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let body = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (set! transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote BOXIFY) (cons name (body (quote repr))))
imports::cons(&[Scm::symbol("BOXIFY"),{
// (cons name (body (quote repr)))
imports::cons(&[name.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-boxify name (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-boxify name (body (quote transform) func))
Scm::func(make_minus_boxify).invoke(&[name.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({// Closure
let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
imports::print(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])}])};{
// (print module " = ")
imports::print(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])}])};{
// (print module ".into_boxed();")
imports::print(&[module.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
imports::error(&[Scm::from("Unknown message BOXIFY"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_closure(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let function = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr, transform, free_minus_vars, prepare_minus_closure, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let prepare_minus_closure = prepare_minus_closure.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
imports::list(&[Scm::symbol("CLOSURE"),function.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-closure (function (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let function = function.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-closure (function (quote transform) func))
Scm::func(make_minus_closure).invoke(&[{
// (function (quote transform) func)
function.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (function (quote free-vars))
function.clone().invoke(&[Scm::symbol("free-vars")])}})});prepare_minus_closure.set({// Closure
let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let free_minus_vars = args[1].clone();if ({
// (pair? free-vars)
imports::pair_p(&[free_minus_vars.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name = {
// (car free-vars)
imports::car(&[free_minus_vars.clone()])};{{
// (print module "let ")
imports::print(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])}])};{
// (print module " = ")
imports::print(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])}])};{
// (print module ".clone();")
imports::print(&[module.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (cdr free-vars)
imports::cdr(&[free_minus_vars.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});gen_minus_rust.set({// Closure
let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "// Closure")
imports::println(&[module.clone(),Scm::from("// Closure")])};{
// (prepare-closure module (free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (free-vars)
free_minus_vars.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
imports::print(&[module.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
imports::print(&[module.clone(),Scm::from(")")])}}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
imports::eq_p(&[Scm::symbol("inner-function"),msg.clone()])}).is_true() {function.clone()} else {{
// (error "Unknown message CLOSURE" msg)
imports::error(&[Scm::from("Unknown message CLOSURE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_comment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let comment = args[0].clone();let node = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, gen_minus_rust_minus_inner, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
imports::list(&[Scm::symbol("COMMENT"),comment.clone(),{
// (node (quote repr))
node.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let comment = comment.clone();let node = node.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-comment comment (node (quote transform) func))
Scm::func(make_minus_comment).invoke(&[comment.clone(),{
// (node (quote transform) func)
node.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (node (quote free-vars))
node.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({// Closure
let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module.clone()])};{
// (print module "// ")
imports::print(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust) module)
node.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});gen_minus_rust_minus_inner.set({// Closure
let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module)
imports::println(&[module.clone()])};{
// (print module "// ")
imports::print(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
imports::showln(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust-inner) module)
node.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote inner) msg)
imports::eq_p(&[Scm::symbol("inner"),msg.clone()])}).is_true() {node.clone()} else {{
// (error "Unknown message COMMENT" msg)
imports::error(&[Scm::from("Unknown message COMMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_constant(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let val = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_constant, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_constant = gen_minus_constant.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
imports::cons(&[Scm::symbol("CONSTANT"),val.clone()])}})});transform.set({// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});gen_minus_constant.set({// Closure
let gen_minus_constant = gen_minus_constant.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let val = args[1].clone();{
// (cond ...)
if ({
// (null? val)
imports::null_p(&[val.clone()])}).is_true() {{
// (print module "Scm::Nil")
imports::print(&[module.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
imports::eq_p(&[val.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
imports::print(&[module.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
imports::eq_p(&[val.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
imports::print(&[module.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
imports::symbol_p(&[val.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
imports::print(&[module.clone(),Scm::from("Scm::symbol(\""),val.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
imports::eq_p(&[val.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
imports::print(&[module.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
imports::char_p(&[val.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
imports::print(&[module.clone(),Scm::from("Scm::char('"),val.clone(),Scm::from("')")])}} else if ({
// (pair? val)
imports::pair_p(&[val.clone()])}).is_true() {{{
// (print module "Scm::pair(")
imports::print(&[module.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (car val)
imports::car(&[val.clone()])}])};{
// (print module ", ")
imports::print(&[module.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (cdr val)
imports::cdr(&[val.clone()])}])};{
// (print module ")")
imports::print(&[module.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
imports::print(&[module.clone(),Scm::from("Scm::from(")])};{
// (show module val)
imports::show(&[module.clone(),val.clone()])};{
// (print module ")")
imports::print(&[module.clone(),Scm::from(")")])}}}}})});gen_minus_rust.set({// Closure
let gen_minus_constant = gen_minus_constant.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (gen-constant module val)
gen_minus_constant.get().invoke(&[module.clone(),val.clone()])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
imports::error(&[Scm::from("Unknown message CONSTANT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_definition(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let var = args[0].clone();let val = args[1].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) (variable-name var) (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) (free-var-name (variable-name var))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable")))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) (variable-name var) (val (quote repr)))
imports::list(&[Scm::symbol("DEFINE"),{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])},{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-definition var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition var (val (quote transform) func))
Scm::func(make_minus_definition).invoke(&[var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let val = val.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])}])}})});gen_minus_rust.set({// Closure
let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable"))))
{let name = {
// (variable-name var)
imports::variable_minus_name(&[var.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print(&[module.clone(),Scm::from("))")])}}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
imports::print(&[module.clone(),Scm::from("))")])}}} else {{
// (error "definition! of non-global variable")
imports::error(&[Scm::from("definition! of non-global variable")])}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg.clone()])}).is_true() {var.clone()} else if ({
// (eq? (quote get-val) msg)
imports::eq_p(&[Scm::symbol("get-val"),msg.clone()])}).is_true() {val.clone()} else {{
// (error "Unknown message DEFINITION" msg)
imports::error(&[Scm::from("Unknown message DEFINITION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_export(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let env = args[0].clone();
        let name = args[1].clone();
        let exname = args[2].clone();
        {
            // (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))) self))
                {
                    let [repr, transform, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let transform = transform.into_boxed();
                                {
                                    let repr = repr.into_boxed();
                                    {
                                        repr.set({
                                            // Closure
                                            let name = name.clone();
                                            let exname = exname.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 0 {
                                                    panic!("invalid arity")
                                                }
                                                {
                                                    // (list (quote EXPORT) name (quote AS) exname)
                                                    imports::list(&[
                                                        Scm::symbol("EXPORT"),
                                                        name.clone(),
                                                        Scm::symbol("AS"),
                                                        exname.clone(),
                                                    ])
                                                }
                                            })
                                        });
                                        transform.set({
                                            // Closure
                                            let self_ = self_.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let func = args[0].clone();
                                                {
                                                    // (func self (lambda () self))
                                                    func.clone().invoke(&[self_.get(), {
                                                        // Closure
                                                        let self_ = self_.clone();
                                                        Scm::func(move |args: &[Scm]| {
                                                            if args.len() != 0 {
                                                                panic!("invalid arity")
                                                            }
                                                            self_.get()
                                                        })
                                                    }])
                                                }
                                            })
                                        });
                                        gen_minus_rust.set({
                                            // Closure
                                            let name = name.clone();
                                            let env = env.clone();
                                            let exname = exname.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let module = args[0].clone();
                                                {
                                                    {
                                                        // (print module "pub use super::")
                                                        imports::print(&[
                                                            module.clone(),
                                                            Scm::from("pub use super::"),
                                                        ])
                                                    };
                                                    {
                                                        // (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))))
                                                        {
                                                            let var = {
                                                                // (lookup name env)
                                                                imports::lookup(&[
                                                                    name.clone(),
                                                                    env.clone(),
                                                                ])
                                                            };
                                                            {
                                                                // (cond ...)
                                                                if ({
// (not var)
imports::not(&[var.clone()])}).is_true() {{
// (error "undefined export" name)
imports::error(&[Scm::from("undefined export"),name.clone()])}} else if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var.clone()])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
imports::error(&[Scm::from("invalid export variable"),var.clone(),name.clone()])}}
                                                            }
                                                        }
                                                    };
                                                    {
                                                        // (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
                                                        imports::println(&[
                                                            module.clone(),
                                                            {
                                                                // (rustify-identifier name)
                                                                imports::rustify_minus_identifier(
                                                                    &[name.clone()],
                                                                )
                                                            },
                                                            Scm::from(" as "),
                                                            {
                                                                // (rustify-identifier exname)
                                                                imports::rustify_minus_identifier(
                                                                    &[exname.clone()],
                                                                )
                                                            },
                                                            Scm::from(";"),
                                                        ])
                                                    }
                                                }
                                            })
                                        });
                                        self_.set({
                                            // Closure
                                            let repr = repr.clone();
                                            let transform = transform.clone();
                                            let gen_minus_rust = gen_minus_rust.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() < 1 {
                                                    panic!("not enough args")
                                                }
                                                let msg = args[0].clone();
                                                let args_ = Scm::list(&args[1..]);
                                                {
                                                    // (cond ...)
                                                    if ({
                                                        // (eq? (quote repr) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("repr"),
                                                            msg.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (repr)
                                                            repr.get().invoke(&[])
                                                        }
                                                    } else if ({
                                                        // (eq? (quote transform) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("transform"),
                                                            msg.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (transform (car args))
                                                            transform.get().invoke(&[{
                                                                // (car args)
                                                                imports::car(&[args_.clone()])
                                                            }])
                                                        }
                                                    } else if ({
                                                        // (eq? (quote kind) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("kind"),
                                                            msg.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        Scm::symbol("EXPORT")
                                                    } else if ({
                                                        // (eq? (quote gen-rust) msg)
                                                        imports::eq_p(&[
                                                            Scm::symbol("gen-rust"),
                                                            msg.clone(),
                                                        ])
                                                    })
                                                    .is_true()
                                                    {
                                                        {
                                                            // (gen-rust (car args))
                                                            gen_minus_rust.get().invoke(&[{
                                                                // (car args)
                                                                imports::car(&[args_.clone()])
                                                            }])
                                                        }
                                                    } else {
                                                        {
                                                            // (error "Unknown message EXPORT" msg)
                                                            imports::error(&[
                                                                Scm::from("Unknown message EXPORT"),
                                                                msg.clone(),
                                                            ])
                                                        }
                                                    }
                                                }
                                            })
                                        });
                                        self_.get()
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
    {if args.len() != 3{panic!("invalid arity")}let vars = args[0].clone();let args_ = args[1].clone();let body = args[2].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) vars (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) (map variable-name vars)) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let vars = vars.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) vars (args (quote repr)) (body (quote repr)))
imports::list(&[Scm::symbol("FIXLET"),vars.clone(),{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc.clone().invoke(&[self_.get(),{// Closure
let vars = vars.clone();let args_ = args_.clone();let fnc = fnc.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet vars (args (quote transform) fnc) (body (quote transform) fnc))
Scm::func(make_minus_fixlet).invoke(&[vars.clone(),{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let body = body.clone();let vars = vars.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars))) (args (quote free-vars)))
imports::set_minus_union(&[{
// (set-remove* (body (quote free-vars)) (map free-var-name (map variable-name vars)))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},{
// (map free-var-name (map variable-name vars))
imports::map(&[Scm::func(free_minus_var_minus_name),{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars.clone()])}])}])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({// Closure
let vars = vars.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*))))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (v*) (if (pair? v*) (begin (print module (rustify-identifier (variable-name (car v*))) ", ") (gen-params (cdr v*)))))) (cond ((= 0 (length vars)) (quote IGNORE)) ((= 1 (length vars)) (print module "let ") (print module (rustify-identifier (variable-name (car vars)))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params vars) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let v_star_ = args[0].clone();if ({
// (pair? v*)
imports::pair_p(&[v_star_.clone()])}).is_true() {{{
// (print module (rustify-identifier (variable-name (car v*))) ", ")
imports::print(&[module.clone(),{
// (rustify-identifier (variable-name (car v*)))
imports::rustify_minus_identifier(&[{
// (variable-name (car v*))
imports::variable_minus_name(&[{
// (car v*)
imports::car(&[v_star_.clone()])}])}])},Scm::from(", ")])};{
// (gen-params (cdr v*))
gen_minus_params.get().invoke(&[{
// (cdr v*)
imports::cdr(&[v_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (cond ...)
if ({
// (= 0 (length vars))
imports::_e_(&[Scm::from(0),{
// (length vars)
imports::length(&[vars.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length vars))
imports::_e_(&[Scm::from(1),{
// (length vars)
imports::length(&[vars.clone()])}])}).is_true() {{{
// (print module "let ")
imports::print(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (variable-name (car vars))))
imports::print(&[module.clone(),{
// (rustify-identifier (variable-name (car vars)))
imports::rustify_minus_identifier(&[{
// (variable-name (car vars))
imports::variable_minus_name(&[{
// (car vars)
imports::car(&[vars.clone()])}])}])}])};{
// (print module " = ")
imports::print(&[module.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
imports::print(&[module.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
imports::print(&[module.clone(),Scm::from("let [")])};{
// (gen-params vars)
gen_minus_params.get().invoke(&[vars.clone()])};{
// (print module "] = [")
imports::print(&[module.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "];")
imports::print(&[module.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
imports::eq_p(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
imports::eq_p(&[Scm::symbol("FIXLET"),{
// ((body (quote inner)) (quote kind))
{
// (body (quote inner))
body.clone().invoke(&[Scm::symbol("inner")])}.invoke(&[Scm::symbol("kind")])}])}} else {Scm::False}}).is_true() {{
// (body (quote gen-rust-inner) module)
body.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else {{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}}}}}}})});gen_minus_rust.set({// Closure
let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (gen-rust-inner module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let arg_star_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car arg*))
transform.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {{
// (map variable-name vars)
imports::map(&[Scm::func(imports::variable_minus_name),vars.clone()])}} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-args) msg)
imports::eq_p(&[Scm::symbol("get-args"),msg.clone()])}).is_true() {args_.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car arg*)
imports::car(&[arg_star_.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
imports::error(&[Scm::from("Unknown message FIXLET"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_function_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let var = args[0].clone();let args_ = args[1].clone();let tail_p = args[2].clone();{
// (letrec ((repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (free-vars (lambda () (args (quote free-vars)))) (gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FN-APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (args (quote free-vars)))) (set! gen-rust (lambda (module) (cond ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid function application" var))) (print module (rustify-identifier (variable-name var)) "(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FN-APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FN-APPLICATION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let tail_p = tail_p.clone();let var = var.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (if tail? (quote FN-APPLY-TC) (quote FN-APPLY)) (variable-name var) (args (quote repr)))
imports::list(&[if (tail_p.clone()).is_true() {Scm::symbol("FN-APPLY-TC")} else {Scm::symbol("FN-APPLY")},{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])},{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let var = var.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-function-application var (args (quote transform) fnc) tail?)))
fnc.clone().invoke(&[self_.get(),{// Closure
let var = var.clone();let args_ = args_.clone();let fnc = fnc.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-function-application var (args (quote transform) fnc) tail?)
Scm::func(make_minus_function_minus_application).invoke(&[var.clone(),{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},tail_p.clone()])}})}])}})});free_minus_vars.set({// Closure
let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}})});gen_minus_rust.set({// Closure
let var = var.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (cond ...)
if ({
// (global-function? var)
imports::global_minus_function_p(&[var.clone()])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module "imports::")
imports::print(&[module.clone(),Scm::from("imports::")])}} else {{
// (error "invalid function application" var)
imports::error(&[Scm::from("invalid function application"),var.clone()])}}};{
// (print module (rustify-identifier (variable-name var)) "(&[")
imports::print(&[module.clone(),{
// (rustify-identifier (variable-name var))
imports::rustify_minus_identifier(&[{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])},Scm::from("(&[")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "])")
imports::print(&[module.clone(),Scm::from("])")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("FN-APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message FN-APPLICATION" msg)
imports::error(&[Scm::from("Unknown message FN-APPLICATION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_import(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
imports::cons(&[Scm::symbol("IMPORT"),lib.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import lib)))
func.clone().invoke(&[self_.get(),{// Closure
let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
Scm::func(make_minus_import).invoke(&[lib.clone()])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});gen_minus_libname.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib.clone()])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
imports::cdr(&[lib.clone()])}])}}}})});gen_minus_rust.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::*;")
imports::print(&[module.clone(),Scm::from("::exports::*;")])};{
// (println module)
imports::println(&[module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import_minus_only(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let lib = args[0].clone();let names = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_imports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_imports = gen_minus_imports.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
imports::cons(&[lib.clone(),names.clone()])}])}})});transform.set({// Closure
let self_ = self_.clone();let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func.clone().invoke(&[self_.get(),{// Closure
let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
Scm::func(make_minus_import_minus_only).invoke(&[lib.clone(),names.clone()])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});gen_minus_libname.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
imports::null_p(&[lib.clone()])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
imports::print(&[module.clone(),{
// (rustify-libname (car lib))
imports::rustify_minus_libname(&[{
// (car lib)
imports::car(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
imports::null_p(&[{
// (cdr lib)
imports::cdr(&[lib.clone()])}])}).is_true() {{
// (print module "")
imports::print(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
imports::print(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
imports::cdr(&[lib.clone()])}])}}}})});gen_minus_imports.set({// Closure
let gen_minus_imports = gen_minus_imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let names = args[1].clone();if ({
// (null? names)
imports::null_p(&[names.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
imports::print(&[module.clone(),{
// (rustify-identifier (car names))
imports::rustify_minus_identifier(&[{
// (car names)
imports::car(&[names.clone()])}])}])};{
// (print module ", ")
imports::print(&[module.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports.get().invoke(&[module.clone(),{
// (cdr names)
imports::cdr(&[names.clone()])}])}}}})});gen_minus_rust.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();let gen_minus_imports = gen_minus_imports.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
imports::print(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::{")
imports::print(&[module.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports.get().invoke(&[module.clone(),names.clone()])};{
// (print module "};")
imports::print(&[module.clone(),Scm::from("};")])};{
// (println module)
imports::println(&[module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
imports::error(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}}.into()
}
pub fn make_minus_library(args: &[Scm]) -> Scm {
    {
        if args.len() != 6 {
            panic!("invalid arity")
        }
        let name = args[0].clone();
        let globals = args[1].clone();
        let init = args[2].clone();
        let body = args[3].clone();
        let imports = args[4].clone();
        let exports = args[5].clone();
        {
            // (let* ((tests (list (quote dummy))) (new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (_make-library name globals init new-body imports exports (cdr tests)))
            {
                // (let ((tests (list (quote dummy)))) (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests)))))
                {
                    let tests = {
                        // (list (quote dummy))
                        imports::list(&[Scm::symbol("dummy")])
                    };
                    // (let ((new-body (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore)))))) (begin (_make-library name globals init new-body imports exports (cdr tests))))
                    let new_minus_body = {
                        // (body (quote transform) (lambda (node ignore) (if (eq? (node (quote kind)) (quote TESTSUITE)) (begin (set-cdr! tests (cons node (cdr tests))) (make-constant (quote *UNSPECIFIED*))) (ignore))))
                        body.clone().invoke(&[Scm::symbol("transform"), {
                            // Closure
                            let tests = tests.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node = args[0].clone();
                                let ignore = args[1].clone();
                                if ({
                                    // (eq? (node (quote kind)) (quote TESTSUITE))
                                    imports::eq_p(&[
                                        {
                                            // (node (quote kind))
                                            node.clone().invoke(&[Scm::symbol("kind")])
                                        },
                                        Scm::symbol("TESTSUITE"),
                                    ])
                                })
                                .is_true()
                                {
                                    {
                                        {
                                            // (set-cdr! tests (cons node (cdr tests)))
                                            imports::set_minus_cdr_i(&[tests.clone(), {
                                                // (cons node (cdr tests))
                                                imports::cons(&[node.clone(), {
                                                    // (cdr tests)
                                                    imports::cdr(&[tests.clone()])
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
                                        ignore.clone().invoke(&[])
                                    }
                                }
                            })
                        }])
                    };
                    {
                        // (_make-library name globals init new-body imports exports (cdr tests))
                        Scm::func(__make_minus_library).invoke(&[
                            name.clone(),
                            globals.clone(),
                            init.clone(),
                            new_minus_body.clone(),
                            imports.clone(),
                            exports.clone(),
                            {
                                // (cdr tests)
                                imports::cdr(&[tests.clone()])
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
                    let [repr, transform, free_minus_vars, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let free_minus_vars = free_minus_vars.into_boxed();
                                {
                                    let transform = transform.into_boxed();
                                    {
                                        let repr = repr.into_boxed();
                                        {
                                            repr.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    Scm::pair(Scm::symbol("NOP"), Scm::Nil)
                                                })
                                            });
                                            transform.set({
                                                // Closure
                                                let self_ = self_.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func = args[0].clone();
                                                    {
                                                        // (func self (lambda () self))
                                                        func.clone().invoke(&[self_.get(), {
                                                            // Closure
                                                            let self_ = self_.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                self_.get()
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            free_minus_vars.set({
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
                                            gen_minus_rust.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module = args[0].clone();
                                                    {
                                                        // (print module "(/*NOP*/)")
                                                        imports::print(&[
                                                            module.clone(),
                                                            Scm::from("(/*NOP*/)"),
                                                        ])
                                                    }
                                                })
                                            });
                                            self_.set({
                                                // Closure
                                                let repr = repr.clone();
                                                let transform = transform.clone();
                                                let free_minus_vars = free_minus_vars.clone();
                                                let gen_minus_rust = gen_minus_rust.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg = args[0].clone();
                                                    let args_ = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NOP")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NOP" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message NOP",
                                                                    ),
                                                                    msg.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            self_.get()
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
                    let [repr, transform, free_minus_vars, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let free_minus_vars = free_minus_vars.into_boxed();
                                {
                                    let transform = transform.into_boxed();
                                    {
                                        let repr = repr.into_boxed();
                                        {
                                            repr.set({
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
                                            transform.set({
                                                // Closure
                                                let self_ = self_.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let fnc = args[0].clone();
                                                    {
                                                        // (fnc self (lambda () self))
                                                        fnc.clone().invoke(&[self_.get(), {
                                                            // Closure
                                                            let self_ = self_.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                self_.get()
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            free_minus_vars.set({
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
                                            gen_minus_rust.set({
                                                // Closure
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module = args[0].clone();
                                                    {
                                                        // (print module "")
                                                        imports::print(&[
                                                            module.clone(),
                                                            Scm::from(""),
                                                        ])
                                                    }
                                                })
                                            });
                                            self_.set({
                                                // Closure
                                                let repr = repr.clone();
                                                let transform = transform.clone();
                                                let free_minus_vars = free_minus_vars.clone();
                                                let gen_minus_rust = gen_minus_rust.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg = args[0].clone();
                                                    let args_ = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NULL-ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NULL-ARG" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message NULL-ARG",
                                                                    ),
                                                                    msg.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            self_.get()
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
    {if args.len() != 5{panic!("invalid arity")}let globals = args[0].clone();let imports = args[1].clone();let init = args[2].clone();let body = args[3].clone();let libraries = args[4].clone();{
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (set! transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (set! gen-imports (lambda (module) (for-each (lambda (i) (i (quote gen-rust) module)) imports))) (set! gen-rust (lambda (module) (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (print module "mod imports") (rust-block module (lambda () (gen-imports module))) (println module) (println module) (rust-gen-global-defs module globals) (println module) (println module) (print module "pub fn main()") (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";"))) (println module) (rust-gen-modules module libraries))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))) self))
{let [repr, transform, gen_minus_imports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_imports = gen_minus_imports.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let globals = globals.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))
imports::cons(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
imports::cons(&[globals.clone(),{
// (cons imports (body (quote repr)))
imports::cons(&[imports.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}])}])}})});transform.set({// Closure
let self_ = self_.clone();let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-program globals imports init (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-program globals imports init (body (quote transform) func))
Scm::func(make_minus_program).invoke(&[globals.clone(),imports.clone(),init.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});gen_minus_imports.set({// Closure
let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})});gen_minus_rust.set({// Closure
let gen_minus_imports = gen_minus_imports.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let libraries = libraries.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
imports::println(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
imports::print(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let gen_minus_imports = gen_minus_imports.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports.get().invoke(&[module.clone()])}})}])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (rust-gen-global-defs module globals)
imports::rust_minus_gen_minus_global_minus_defs(&[module.clone(),globals.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (println module)
imports::println(&[module.clone()])};{
// (print module "pub fn main()")
imports::print(&[module.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
imports::println(&[module.clone()])};{
// (println module "eprintln!(\"built with\");")
imports::println(&[module.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
imports::println(&[module.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
imports::println(&[module.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
imports::print(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{{
// (print module (rustify-libname l))
imports::print(&[module.clone(),{
// (rustify-libname l)
imports::rustify_minus_libname(&[l.clone()])}])};{
// (print module "::")
imports::print(&[module.clone(),Scm::from("::")])}}})},lib.clone()])};{
// (print module "initialize();")
imports::print(&[module.clone(),Scm::from("initialize();")])};{
// (println module)
imports::println(&[module.clone()])}}})},init.clone()])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";")
imports::println(&[module.clone(),Scm::from(";")])}}})}])};{
// (println module)
imports::println(&[module.clone()])};{
// (rust-gen-modules module libraries)
imports::rust_minus_gen_minus_modules(&[module.clone(),libraries.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
imports::error(&[Scm::from("Unknown message PROGRAM"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_reference(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let var = args[0].clone();{
// (letrec ((repr (lambda () (list (quote GET) (variable-name var)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) (variable-name var)))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) (free-var-name (variable-name var)))))) (set! gen-rust (lambda (module) (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()")))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) (variable-name var)) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) (variable-name var))
imports::list(&[Scm::symbol("GET"),{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])}})});transform.set({// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({// Closure
let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
imports::bor(&[{
// (global-variable? var)
imports::global_minus_variable_p(&[var.clone()])},{
// (global-function? var)
imports::global_minus_function_p(&[var.clone()])},{
// (import-variable? var)
imports::import_minus_variable_p(&[var.clone()])}])}).is_true() {{
// (make-set)
imports::make_minus_set(&[])}} else {{
// (set-add (make-set) (free-var-name (variable-name var)))
imports::set_minus_add(&[{
// (make-set)
imports::make_minus_set(&[])},{
// (free-var-name (variable-name var))
Scm::func(free_minus_var_minus_name).invoke(&[{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}])}])}}})});gen_minus_rust.set({// Closure
let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (let ((name (variable-name var))) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))
{let name = {
// (variable-name var)
imports::variable_minus_name(&[var.clone()])};{
// (cond ...)
if ({
// (global-variable? var)
imports::global_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".with(|value| value.get())")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
imports::global_minus_function_p(&[var.clone()])}).is_true() {{
// (print module "Scm::func(" (rustify-identifier name) ")")
imports::print(&[module.clone(),Scm::from("Scm::func("),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(")")])}} else if ({
// (import-variable? var)
imports::import_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module "Scm::func(imports::" (rustify-identifier name) ")")
imports::print(&[module.clone(),Scm::from("Scm::func(imports::"),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(")")])}} else if ({
// (boxed-variable? var)
imports::boxed_minus_variable_p(&[var.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".get()")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".get()")])}} else {{
// (print module (rustify-identifier name) ".clone()")
imports::print(&[module.clone(),{
// (rustify-identifier name)
imports::rustify_minus_identifier(&[name.clone()])},Scm::from(".clone()")])}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
imports::eq_p(&[Scm::symbol("get-name"),msg.clone()])}).is_true() {{
// (variable-name var)
imports::variable_minus_name(&[var.clone()])}} else if ({
// (eq? (quote get-var) msg)
imports::eq_p(&[Scm::symbol("get-var"),msg.clone()])}).is_true() {var.clone()} else {{
// (error "Unknown message REFERENCE" msg)
imports::error(&[Scm::from("Unknown message REFERENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_sequence(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let first = args[0].clone();let next = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list(&[Scm::symbol("SEQUENCE"),{
// (first (quote repr))
first.clone().invoke(&[Scm::symbol("repr")])},{
// (next (quote repr))
next.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let next = next.clone();let first = first.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b))))
func.clone().invoke(&[self_.get(),{// Closure
let next = next.clone();let func = func.clone();let first = first.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
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
Scm::func(make_minus_sequence).invoke(&[a.clone(),b.clone()])}}}}})}])}})});free_minus_vars.set({// Closure
let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
imports::set_minus_union(&[{
// (first (quote free-vars))
first.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({// Closure
let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (first (quote gen-rust) module)
first.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
imports::print(&[module.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else {{
// (next (quote gen-rust) module)
next.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}}})});gen_minus_rust.set({// Closure
let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "{")
imports::print(&[module.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])};{
// (print module "}")
imports::print(&[module.clone(),Scm::from("}")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
imports::eq_p(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
imports::error(&[Scm::from("Unknown message SEQUENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_testcase(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let description = args[0].clone();
        let body = args[1].clone();
        {
            // (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
            {
                // (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
                {
                    let [repr, transform, free_minus_vars, gen_minus_rust, self_] = [
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                        Scm::symbol("*uninitialized*"),
                    ];
                    {
                        let self_ = self_.into_boxed();
                        {
                            let gen_minus_rust = gen_minus_rust.into_boxed();
                            {
                                let free_minus_vars = free_minus_vars.into_boxed();
                                {
                                    let transform = transform.into_boxed();
                                    {
                                        let repr = repr.into_boxed();
                                        {
                                            repr.set({
                                                // Closure
                                                let description = description.clone();
                                                let body = body.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    {
                                                        // (list (quote TESTCASE) description body)
                                                        imports::list(&[
                                                            Scm::symbol("TESTCASE"),
                                                            description.clone(),
                                                            body.clone(),
                                                        ])
                                                    }
                                                })
                                            });
                                            transform.set({
                                                // Closure
                                                let self_ = self_.clone();
                                                let description = description.clone();
                                                let body = body.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let func = args[0].clone();
                                                    {
                                                        // (func self (lambda () (make-testcase description (body (quote transform) func))))
                                                        func.clone().invoke(&[self_.get(), {
                                                            // Closure
                                                            let description = description.clone();
                                                            let body = body.clone();
                                                            let func = func.clone();
                                                            Scm::func(move |args: &[Scm]| {
                                                                if args.len() != 0 {
                                                                    panic!("invalid arity")
                                                                }
                                                                {
                                                                    // (make-testcase description (body (quote transform) func))
                                                                    Scm::func(make_minus_testcase)
                                                                        .invoke(&[
                                                                            description.clone(),
                                                                            {
                                                                                // (body (quote transform) func)
                                                                                body.clone()
                                                                                    .invoke(&[
                                                                                    Scm::symbol(
                                                                                        "transform",
                                                                                    ),
                                                                                    func.clone(),
                                                                                ])
                                                                            },
                                                                        ])
                                                                }
                                                            })
                                                        }])
                                                    }
                                                })
                                            });
                                            free_minus_vars.set({
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
                                            gen_minus_rust.set({
                                                // Closure
                                                let description = description.clone();
                                                let body = body.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let module = args[0].clone();
                                                    {
                                                        {
                                                            // (println module "#[test]")
                                                            imports::println(&[
                                                                module.clone(),
                                                                Scm::from("#[test]"),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "fn " (rustify-testname description) "() {")
                                                            imports::println(&[
                                                                module.clone(),
                                                                Scm::from("fn "),
                                                                {
                                                                    // (rustify-testname description)
                                                                    imports::rustify_minus_testname(
                                                                        &[description.clone()],
                                                                    )
                                                                },
                                                                Scm::from("() {"),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "super::initialize();")
                                                            imports::println(&[
                                                                module.clone(),
                                                                Scm::from("super::initialize();"),
                                                            ])
                                                        };
                                                        {
                                                            // (body (quote gen-rust) module)
                                                            body.clone().invoke(&[
                                                                Scm::symbol("gen-rust"),
                                                                module.clone(),
                                                            ])
                                                        };
                                                        {
                                                            // (println module "}")
                                                            imports::println(&[
                                                                module.clone(),
                                                                Scm::from("}"),
                                                            ])
                                                        }
                                                    }
                                                })
                                            });
                                            self_.set({
                                                // Closure
                                                let repr = repr.clone();
                                                let transform = transform.clone();
                                                let free_minus_vars = free_minus_vars.clone();
                                                let gen_minus_rust = gen_minus_rust.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() < 1 {
                                                        panic!("not enough args")
                                                    }
                                                    let msg = args[0].clone();
                                                    let args_ = Scm::list(&args[1..]);
                                                    {
                                                        // (cond ...)
                                                        if ({
                                                            // (eq? (quote repr) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("repr"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (repr)
                                                                repr.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote transform) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("transform"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (transform (car args))
                                                                transform.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (free-vars)
                                                                free_minus_vars.get().invoke(&[])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("TESTCASE")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            imports::eq_p(&[
                                                                Scm::symbol("gen-rust"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            {
                                                                // (gen-rust (car args))
                                                                gen_minus_rust.get().invoke(&[{
                                                                    // (car args)
                                                                    imports::car(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message TESTCASE" msg)
                                                                imports::error(&[
                                                                    Scm::from(
                                                                        "Unknown message TESTCASE",
                                                                    ),
                                                                    msg.clone(),
                                                                ])
                                                            }
                                                        }
                                                    }
                                                })
                                            });
                                            self_.get()
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
    {if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let cases = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
imports::list(&[Scm::symbol("TESTSUITE"),name.clone(),cases.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let func = func.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
Scm::func(make_minus_testsuite).invoke(&[name.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
imports::map(&[{// Closure
let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote transform) func)
c.clone().invoke(&[Scm::symbol("transform"),func.clone()])}})},cases.clone()])}])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
imports::make_minus_set(&[])}})});gen_minus_rust.set({// Closure
let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[cfg(test)]")
imports::println(&[module.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
imports::println(&[module.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
imports::println(&[module.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
imports::for_minus_each(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote gen-rust) module)
c.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},cases.clone()])};{
// (println module "}")
imports::println(&[module.clone(),Scm::from("}")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
imports::error(&[Scm::from("Unknown message TESTSUITE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 5{panic!("invalid arity")}let params = args[0].clone();let vararg = args[1].clone();let vars = args[2].clone();let varvar = args[3].clone();let body = args[4].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) params (body (quote repr)))
imports::list(&[Scm::symbol("VARARG-ABSTRACTION"),params.clone(),{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))
Scm::func(make_minus_vararg_minus_abstraction).invoke(&[params.clone(),vararg.clone(),vars.clone(),varvar.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let body = body.clone();let vararg = vararg.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-remove* (body (quote free-vars)) (cons vararg params))
imports::set_minus_remove_star_(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},{
// (cons vararg params)
imports::cons(&[vararg.clone(),params.clone()])}])}})});gen_minus_rust.set({// Closure
let vararg = vararg.clone();let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
imports::pair_p(&[p_star_.clone()])}).is_true() {{{
// (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();")
imports::print(&[module.clone(),Scm::from("let "),{
// (rustify-identifier (car p*))
imports::rustify_minus_identifier(&[{
// (car p*)
imports::car(&[p_star_.clone()])}])},Scm::from(" = args["),k.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
imports::cdr(&[p_star_.clone()])},{
// (+ k 1)
imports::_plus_(&[k.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")
imports::print(&[module.clone(),Scm::from("let "),{
// (rustify-identifier vararg)
imports::rustify_minus_identifier(&[vararg.clone()])},Scm::from(" = Scm::list(&args["),k.clone(),Scm::from("..]);")])}}})});{
// (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))
imports::rust_minus_block(&[module.clone(),{// Closure
let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}")
imports::print(&[module.clone(),Scm::from("if args.len() < "),{
// (length params)
imports::length(&[params.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
imports::eq_p(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
imports::eq_p(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
imports::eq_p(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
imports::eq_p(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
imports::eq_p(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
imports::car(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
imports::eq_p(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vararg) msg)
imports::eq_p(&[Scm::symbol("get-vararg"),msg.clone()])}).is_true() {vararg.clone()} else if ({
// (eq? (quote get-vars) msg)
imports::eq_p(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-varvar) msg)
imports::eq_p(&[Scm::symbol("get-varvar"),msg.clone()])}).is_true() {varvar.clone()} else if ({
// (eq? (quote get-body) msg)
imports::eq_p(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn procedure_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        if ({
            // (eq? (obj (quote kind)) (quote ABSTRACTION))
            imports::eq_p(&[
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
                imports::eq_p(&[
                    {
                        // (obj (quote kind))
                        obj.clone().invoke(&[Scm::symbol("kind")])
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
            // (define (make-abstraction params vars body) ...)
            (/*NOP*/)
        };
        {
            // (define (make-vararg-abstraction params vararg vars varvar body) ...)
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
