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
Scm::func(imports::append).invoke(&[Scm::symbol("LIBRARY"),name.clone(),exports.clone(),imports.clone(),globals.clone(),{
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
Scm::func(imports::map).invoke(&[{// Closure
let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let t = args[0].clone();{
// (t (quote transform) func)
t.clone().invoke(&[Scm::symbol("transform"),func.clone()])}})},testsuite.clone()])}])}})}])}})});gen_minus_exports.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let exports = args[1].clone();{
// (for-each (lambda (expo) (expo (quote gen-rust) module)) exports)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo = args[0].clone();{
// (expo (quote gen-rust) module)
expo.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},exports.clone()])}})});gen_minus_rust.set({// Closure
let imports = imports.clone();let gen_minus_exports = gen_minus_exports.clone();let exports = exports.clone();let globals = globals.clone();let body = body.clone();let init = init.clone();let testsuite = testsuite.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};")])};{
// (print module "mod imports")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (for-each (lambda (i) (i (quote gen-rust) module)) imports)))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (for-each (lambda (i) (i (quote gen-rust) module)) imports)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})}])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (print module "pub mod exports")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("pub mod exports")])};{
// (rust-block module (lambda () (gen-exports module exports)))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let gen_minus_exports = gen_minus_exports.clone();let module = module.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-exports module exports)
gen_minus_exports.get().invoke(&[module.clone(),exports.clone()])}})}])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (rust-gen-global-defs module globals)
Scm::func(imports::rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),globals.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};if ({
// (eq? (quote NOP) (body (quote kind)))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("NOP"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (println module "pub fn initialize() {")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("pub fn initialize() {")])}} else {{{
// (println module "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module "pub fn initialize() {")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("pub fn initialize() {")])};{
// (println module "if INITIALIZED.with(|x| x.get()) { return }")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("if INITIALIZED.with(|x| x.get()) { return }")])};{
// (println module "INITIALIZED.with(|x| x.set(true));")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("INITIALIZED.with(|x| x.set(true));")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])}}};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l) "::")) lib) (println module "initialize();")) init)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l) "::")) lib)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{
// (print module (rustify-libname l) "::")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-libname l)
Scm::func(imports::rustify_minus_libname).invoke(&[l.clone()])},Scm::from("::")])}})},lib.clone()])};{
// (println module "initialize();")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("initialize();")])}}})},init.clone()])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";}")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from(";}")])};{
// (for-each (lambda (test) (test (quote gen-rust) module)) testsuite)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let test = args[0].clone();{
// (test (quote gen-rust) module)
test.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},testsuite.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let name = name.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("LIBRARY")} else if ({
// (eq? (quote libname) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("libname"),msg.clone()])}).is_true() {name.clone()} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message LIBRARY" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message LIBRARY"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn ast_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        {
            // (procedure? obj)
            Scm::func(imports::procedure_p).invoke(&[obj.clone()])
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
Scm::func(imports::list).invoke(&[Scm::symbol("ABSTRACTION"),params.clone(),{
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
Scm::func(imports::set_minus_remove_star_).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},params.clone()])}})});gen_minus_rust.set({// Closure
let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1))))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let ") (print module (rustify-identifier (car p*))) (print module " = args[") (print module k) (print module "].clone();") (gen-params (cdr p*) (+ k 1)))))) (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
Scm::func(imports::pair_p).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module "let ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (car p*)))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier (car p*))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (car p*)
Scm::func(imports::car).invoke(&[p_star_.clone()])}])}])};{
// (print module " = args[")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(" = args[")])};{
// (print module k)
Scm::func(imports::print).invoke(&[module.clone(),k.clone()])};{
// (print module "].clone();")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
Scm::func(imports::cdr).invoke(&[p_star_.clone()])},{
// (+ k 1)
Scm::func(imports::_plus_).invoke(&[k.clone(),Scm::from(1)])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (rust-block module (lambda () (print module "if args.len() != ") (print module (length params)) (print module "{panic!(\"invalid arity\")}") (gen-params params 0) (body (quote gen-rust) module)))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() != ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("if args.len() != ")])};{
// (print module (length params))
Scm::func(imports::print).invoke(&[module.clone(),{
// (length params)
Scm::func(imports::length).invoke(&[params.clone()])}])};{
// (print module "{panic!(\"invalid arity\")}")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("{panic!(\"invalid arity\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-body) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message ABSTRACTION" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_alternative(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let condition = args[0].clone();let consequent = args[1].clone();let alternative = args[2].clone();{
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (set! free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (set! gen-rust (lambda (module) (print module "if (") (condition (quote gen-rust) module) (print module ").is_true() {") (consequent (quote gen-rust) module) (print module "} else ") (if (eq? (alternative (quote kind)) (quote ALTERNATIVE)) (alternative (quote gen-rust) module) (begin (print module "{") (alternative (quote gen-rust) module) (print module "}"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("IF"),{
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
Scm::func(imports::set_minus_union).invoke(&[{
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
Scm::func(imports::set_minus_union).invoke(&[{
// (condition (quote free-vars))
condition.clone().invoke(&[Scm::symbol("free-vars")])},{
// (consequent (quote free-vars))
consequent.clone().invoke(&[Scm::symbol("free-vars")])}])},{
// (alternative (quote free-vars))
alternative.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({// Closure
let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "if (")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("if (")])};{
// (condition (quote gen-rust) module)
condition.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ").is_true() {")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(").is_true() {")])};{
// (consequent (quote gen-rust) module)
consequent.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "} else ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("} else ")])};if ({
// (eq? (alternative (quote kind)) (quote ALTERNATIVE))
Scm::func(imports::eq_p).invoke(&[{
// (alternative (quote kind))
alternative.clone().invoke(&[Scm::symbol("kind")])},Scm::symbol("ALTERNATIVE")])}).is_true() {{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}} else {{{
// (print module "{")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("{")])};{
// (alternative (quote gen-rust) module)
alternative.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "}")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("}")])}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ALTERNATIVE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ALTERNATIVE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message ALTERNATIVE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_application(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let func = args[0].clone();let args_ = args[1].clone();let tail_p = args[2].clone();{
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (set! free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (set! gen-rust (lambda (module) (func (quote gen-rust) module) (print module ".invoke(&[") (args (quote gen-rust) module) (print module "])"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let tail_p = tail_p.clone();let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
Scm::func(imports::cons).invoke(&[if (tail_p.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")},{
// (cons (func (quote repr)) (args (quote repr)))
Scm::func(imports::cons).invoke(&[{
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
Scm::func(imports::set_minus_union).invoke(&[{
// (func (quote free-vars))
func.clone().invoke(&[Scm::symbol("free-vars")])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust.set({// Closure
let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (func (quote gen-rust) module)
func.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ".invoke(&[")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(".invoke(&[")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "])")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("])")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("APPLICATION")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message APPLICATION" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message APPLICATION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
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
                                                        Scm::func(imports::cons).invoke(&[
                                                            Scm::symbol("ARG"),
                                                            {
                                                                // (cons arg next)
                                                                Scm::func(imports::cons).invoke(&[
                                                                    arg.clone(),
                                                                    next.clone(),
                                                                ])
                                                            },
                                                        ])
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
                                                        Scm::func(imports::set_minus_union).invoke(
                                                            &[
                                                                {
                                                                    // (arg (quote free-vars))
                                                                    arg.clone().invoke(&[
                                                                        Scm::symbol("free-vars"),
                                                                    ])
                                                                },
                                                                {
                                                                    // (next (quote free-vars))
                                                                    next.clone().invoke(&[
                                                                        Scm::symbol("free-vars"),
                                                                    ])
                                                                },
                                                            ],
                                                        )
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
                                                            Scm::func(imports::not).invoke(&[{
                                                                // (eq? (quote NULL-ARG) (next (quote kind)))
                                                                Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::print)
                                                                        .invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ARG" msg)
                                                                Scm::func(imports::error).invoke(&[
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
                                                        Scm::func(imports::list).invoke(&[
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
                                                            Scm::func(imports::print).invoke(&[
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
                                                            Scm::func(imports::println).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ASSERT")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message ASSERT" msg)
                                                                Scm::func(imports::error).invoke(&[
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
    {if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();let val = args[2].clone();{
// (letrec ((repr (lambda () (list (quote SET!) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SET!) name (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) name))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((boxed-variable? var) (print module (rustify-identifier name) ".set(") (val (quote gen-rust) module) (print module ")")) (else (error "set! on unboxed variable" name var))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SET!) name (val (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("SET!"),name.clone(),{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-assignment name var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-assignment name var (val (quote transform) func))
Scm::func(make_minus_assignment).invoke(&[name.clone(),var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let val = val.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) name)
Scm::func(imports::set_minus_add).invoke(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},name.clone()])}})});gen_minus_rust.set({// Closure
let var = var.clone();let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
Scm::func(imports::global_minus_variable_p).invoke(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("))")])}}} else if ({
// (boxed-variable? var)
Scm::func(imports::boxed_minus_variable_p).invoke(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".set(")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(")")])}}} else {{
// (error "set! on unboxed variable" name var)
Scm::func(imports::error).invoke(&[Scm::from("set! on unboxed variable"),name.clone(),var.clone()])}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("ASSIGNMENT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message ASSIGNMENT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message ASSIGNMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_boxify(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let body = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (set! transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (set! free-vars (lambda () (body (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".into_boxed();") (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote BOXIFY) (cons name (body (quote repr))))
Scm::func(imports::cons).invoke(&[Scm::symbol("BOXIFY"),{
// (cons name (body (quote repr)))
Scm::func(imports::cons).invoke(&[name.clone(),{
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
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "let ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])}])};{
// (print module " = ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])}])};{
// (print module ".into_boxed();")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(".into_boxed();")])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("BOXIFY")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message BOXIFY" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message BOXIFY"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_closure(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let function = args[0].clone();{
// (letrec ((repr (lambda () (list (quote CLOSURE) function))) (transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (free-vars (lambda () (function (quote free-vars)))) (prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (prepare-closure (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote CLOSURE) function))) (set! transform (lambda (func) (func self (lambda () (make-closure (function (quote transform) func)))))) (set! free-vars (lambda () (function (quote free-vars)))) (set! prepare-closure (lambda (module free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CLOSURE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote inner-function) msg) function) (else (error "Unknown message CLOSURE" msg))))) self))
{let [repr, transform, free_minus_vars, prepare_minus_closure, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let prepare_minus_closure = prepare_minus_closure.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote CLOSURE) function)
Scm::func(imports::list).invoke(&[Scm::symbol("CLOSURE"),function.clone()])}})});transform.set({// Closure
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
Scm::func(imports::pair_p).invoke(&[free_minus_vars.clone()])}).is_true() {{
// (let ((name (car free-vars))) (print module "let ") (print module (rustify-identifier name)) (print module " = ") (print module (rustify-identifier name)) (print module ".clone();") (prepare-closure module (cdr free-vars)))
{let name = {
// (car free-vars)
Scm::func(imports::car).invoke(&[free_minus_vars.clone()])};{{
// (print module "let ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier name))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])}])};{
// (print module " = ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(" = ")])};{
// (print module (rustify-identifier name))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])}])};{
// (print module ".clone();")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(".clone();")])};{
// (prepare-closure module (cdr free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (cdr free-vars)
Scm::func(imports::cdr).invoke(&[free_minus_vars.clone()])}])}}}}} else {Scm::symbol("*UNSPECIFIED*")}})});gen_minus_rust.set({// Closure
let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (rust-block module (lambda () (println module "// Closure") (prepare-closure module (free-vars)) (print module "Scm::func(move |args: &[Scm]|") (function (quote gen-rust) module) (print module ")")))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module "// Closure")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("// Closure")])};{
// (prepare-closure module (free-vars))
prepare_minus_closure.get().invoke(&[module.clone(),{
// (free-vars)
free_minus_vars.get().invoke(&[])}])};{
// (print module "Scm::func(move |args: &[Scm]|")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::func(move |args: &[Scm]|")])};{
// (function (quote gen-rust) module)
function.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(")")])}}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let function = function.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CLOSURE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote inner-function) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("inner-function"),msg.clone()])}).is_true() {function.clone()} else {{
// (error "Unknown message CLOSURE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message CLOSURE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_comment(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let comment = args[0].clone();let node = args[1].clone();{
// (letrec ((repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote COMMENT) comment (node (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (set! free-vars (lambda () (node (quote free-vars)))) (set! gen-rust (lambda (module) (rust-block module (lambda () (println module) (print module "// ") (showln module comment) (node (quote gen-rust) module))))) (set! gen-rust-inner (lambda (module) (println module) (print module "// ") (showln module comment) (node (quote gen-rust-inner) module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) ((eq? (quote inner) msg) node) (else (error "Unknown message COMMENT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, gen_minus_rust_minus_inner, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote COMMENT) comment (node (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("COMMENT"),comment.clone(),{
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
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (print module "// ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
Scm::func(imports::showln).invoke(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust) module)
node.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}})});gen_minus_rust_minus_inner.set({// Closure
let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (print module "// ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("// ")])};{
// (showln module comment)
Scm::func(imports::showln).invoke(&[module.clone(),comment.clone()])};{
// (node (quote gen-rust-inner) module)
node.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("COMMENT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote inner) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("inner"),msg.clone()])}).is_true() {node.clone()} else {{
// (error "Unknown message COMMENT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message COMMENT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_constant(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let val = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (gen-rust (lambda (module) (gen-constant module val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-constant (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote CONSTANT) val))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (make-set))) (set! gen-constant (lambda (module val) (cond ((null? val) (print module "Scm::Nil")) ((eq? val #t) (print module "Scm::True")) ((eq? val #f) (print module "Scm::False")) ((symbol? val) (print module "Scm::symbol(\"" val "\")")) ((eq? val #\') (print module "Scm::char_apostrophe()")) ((char? val) (print module "Scm::char('" val "')")) ((pair? val) (print module "Scm::pair(") (gen-constant module (car val)) (print module ", ") (gen-constant module (cdr val)) (print module ")")) (else (print module "Scm::from(") (show module val) (print module ")"))))) (set! gen-rust (lambda (module) (gen-constant module val))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_constant, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_constant = gen_minus_constant.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote CONSTANT) val)
Scm::func(imports::cons).invoke(&[Scm::symbol("CONSTANT"),val.clone()])}})});transform.set({// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}})});gen_minus_constant.set({// Closure
let gen_minus_constant = gen_minus_constant.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let val = args[1].clone();{
// (cond ...)
if ({
// (null? val)
Scm::func(imports::null_p).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::Nil")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::Nil")])}} else if ({
// (eq? val #t)
Scm::func(imports::eq_p).invoke(&[val.clone(),Scm::True])}).is_true() {{
// (print module "Scm::True")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::True")])}} else if ({
// (eq? val #f)
Scm::func(imports::eq_p).invoke(&[val.clone(),Scm::False])}).is_true() {{
// (print module "Scm::False")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::False")])}} else if ({
// (symbol? val)
Scm::func(imports::symbol_p).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::symbol(\"" val "\")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::symbol(\""),val.clone(),Scm::from("\")")])}} else if ({
// (eq? val #\')
Scm::func(imports::eq_p).invoke(&[val.clone(),Scm::char_apostrophe()])}).is_true() {{
// (print module "Scm::char_apostrophe()")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::char_apostrophe()")])}} else if ({
// (char? val)
Scm::func(imports::char_p).invoke(&[val.clone()])}).is_true() {{
// (print module "Scm::char('" val "')")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::char('"),val.clone(),Scm::from("')")])}} else if ({
// (pair? val)
Scm::func(imports::pair_p).invoke(&[val.clone()])}).is_true() {{{
// (print module "Scm::pair(")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::pair(")])};{
// (gen-constant module (car val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (car val)
Scm::func(imports::car).invoke(&[val.clone()])}])};{
// (print module ", ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(", ")])};{
// (gen-constant module (cdr val))
gen_minus_constant.get().invoke(&[module.clone(),{
// (cdr val)
Scm::func(imports::cdr).invoke(&[val.clone()])}])};{
// (print module ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(")")])}}} else {{{
// (print module "Scm::from(")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::from(")])};{
// (show module val)
Scm::func(imports::show).invoke(&[module.clone(),val.clone()])};{
// (print module ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(")")])}}}}})});gen_minus_rust.set({// Closure
let gen_minus_constant = gen_minus_constant.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (gen-constant module val)
gen_minus_constant.get().invoke(&[module.clone(),val.clone()])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("CONSTANT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message CONSTANT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message CONSTANT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_definition(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();let val = args[2].clone();{
// (letrec ((repr (lambda () (list (quote DEFINE) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-definition name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) name) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote DEFINE) name (val (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-definition name var (val (quote transform) func)))))) (set! free-vars (lambda () (set-add (val (quote free-vars)) name))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) ((global-function? var) (print module (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) module) (print module "))")) (else (error "definition! of non-global variable"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote DEFINITION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-name) msg) name) ((eq? (quote get-var) msg) var) ((eq? (quote get-val) msg) val) (else (error "Unknown message DEFINITION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote DEFINE) name (val (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("DEFINE"),name.clone(),{
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-definition name var (val (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-definition name var (val (quote transform) func))
Scm::func(make_minus_definition).invoke(&[name.clone(),var.clone(),{
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let val = val.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-add (val (quote free-vars)) name)
Scm::func(imports::set_minus_add).invoke(&[{
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars")])},name.clone()])}})});gen_minus_rust.set({// Closure
let var = var.clone();let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
Scm::func(imports::global_minus_variable_p).invoke(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("))")])}}} else if ({
// (global-function? var)
Scm::func(imports::global_minus_function_p).invoke(&[var.clone()])}).is_true() {{{
// (print module (rustify-identifier name) ".with(|value| value.set(")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".with(|value| value.set(")])};{
// (val (quote gen-rust) module)
val.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "))")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("))")])}}} else {{
// (error "definition! of non-global variable")
Scm::func(imports::error).invoke(&[Scm::from("definition! of non-global variable")])}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("DEFINITION")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-name) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-name"),msg.clone()])}).is_true() {name.clone()} else if ({
// (eq? (quote get-var) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-var"),msg.clone()])}).is_true() {var.clone()} else if ({
// (eq? (quote get-val) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-val"),msg.clone()])}).is_true() {val.clone()} else {{
// (error "Unknown message DEFINITION" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message DEFINITION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_export(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let env = args[0].clone();let name = args[1].clone();let exname = args[2].clone();{
// (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (set! transform (lambda (func) (func self (lambda () self)))) (set! gen-rust (lambda (module) (print module "pub use super::") (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name)))) (println module (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))) self))
{let [repr, transform, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote EXPORT) name (quote AS) exname)
Scm::func(imports::list).invoke(&[Scm::symbol("EXPORT"),name.clone(),Scm::symbol("AS"),exname.clone()])}})});transform.set({// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});gen_minus_rust.set({// Closure
let name = name.clone();let env = env.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use super::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("pub use super::")])};{
// (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((global-variable? var) (print module "")) ((global-function? var) (print module "")) ((import-variable? var) (print module "imports::")) (else (error "invalid export variable" var name))))
{let var = {
// (lookup name env)
Scm::func(imports::lookup).invoke(&[name.clone(),env.clone()])};{
// (cond ...)
if ({
// (not var)
Scm::func(imports::not).invoke(&[var.clone()])}).is_true() {{
// (error "undefined export" name)
Scm::func(imports::error).invoke(&[Scm::from("undefined export"),name.clone()])}} else if ({
// (global-variable? var)
Scm::func(imports::global_minus_variable_p).invoke(&[var.clone()])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else if ({
// (global-function? var)
Scm::func(imports::global_minus_function_p).invoke(&[var.clone()])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else if ({
// (import-variable? var)
Scm::func(imports::import_minus_variable_p).invoke(&[var.clone()])}).is_true() {{
// (print module "imports::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("imports::")])}} else {{
// (error "invalid export variable" var name)
Scm::func(imports::error).invoke(&[Scm::from("invalid export variable"),var.clone(),name.clone()])}}}}};{
// (println module (rustify-identifier name) " as " (rustify-identifier exname) ";")
Scm::func(imports::println).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(" as "),{
// (rustify-identifier exname)
Scm::func(imports::rustify_minus_identifier).invoke(&[exname.clone()])},Scm::from(";")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("EXPORT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message EXPORT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message EXPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}.into()
}
pub fn make_minus_fixlet(args: &[Scm]) -> Scm {
    {if args.len() != 4{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let args_ = args[2].clone();let body = args[3].clone();{
// (letrec ((repr (lambda () (list (quote FIXLET) params (args (quote repr)) (body (quote repr))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (gen-rust-inner (lambda (module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote FIXLET) params (args (quote repr)) (body (quote repr))))) (set! transform (lambda (fnc) (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc)))))) (set! free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (set! gen-rust-inner (lambda (module) (define (gen-params p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))) (set! gen-rust (lambda (module) (rust-block module (lambda () (gen-rust-inner module))))) (set! self (lambda (msg . arg*) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car arg*))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-args) msg) args) ((eq? (quote get-body) msg) body) ((eq? (quote gen-rust) msg) (gen-rust (car arg*))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car arg*))) (else (error "Unknown message FIXLET" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote FIXLET) params (args (quote repr)) (body (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("FIXLET"),params.clone(),{
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr")])},{
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr")])}])}})});transform.set({// Closure
let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();{
// (fnc self (lambda () (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc))))
fnc.clone().invoke(&[self_.get(),{// Closure
let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let fnc = fnc.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-fixlet params vars (args (quote transform) fnc) (body (quote transform) fnc))
Scm::func(make_minus_fixlet).invoke(&[params.clone(),vars.clone(),{
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])},{
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"),fnc.clone()])}])}})}])}})});free_minus_vars.set({// Closure
let body = body.clone();let params = params.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))
Scm::func(imports::set_minus_union).invoke(&[{
// (set-remove* (body (quote free-vars)) params)
Scm::func(imports::set_minus_remove_star_).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},params.clone()])},{
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({// Closure
let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p*) (if (pair? p*) (begin (print module (rustify-identifier (car p*)) ", ") (gen-params (cdr p*)))))) (cond ((= 0 (length params)) (quote IGNORE)) ((= 1 (length params)) (print module "let ") (print module (rustify-identifier (car params))) (print module " = ") (args (quote gen-rust) module) (print module ";")) (else (print module "let [") (gen-params params) (print module "] = [") (args (quote gen-rust) module) (print module "];"))) (if (eq? (quote FIXLET) (body (quote kind))) (body (quote gen-rust-inner) module) (if (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))) (body (quote gen-rust-inner) module) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p_star_ = args[0].clone();if ({
// (pair? p*)
Scm::func(imports::pair_p).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module (rustify-identifier (car p*)) ", ")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier (car p*))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (car p*)
Scm::func(imports::car).invoke(&[p_star_.clone()])}])},Scm::from(", ")])};{
// (gen-params (cdr p*))
gen_minus_params.get().invoke(&[{
// (cdr p*)
Scm::func(imports::cdr).invoke(&[p_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}})});{
// (cond ...)
if ({
// (= 0 (length params))
Scm::func(imports::_e_).invoke(&[Scm::from(0),{
// (length params)
Scm::func(imports::length).invoke(&[params.clone()])}])}).is_true() {Scm::symbol("IGNORE")} else if ({
// (= 1 (length params))
Scm::func(imports::_e_).invoke(&[Scm::from(1),{
// (length params)
Scm::func(imports::length).invoke(&[params.clone()])}])}).is_true() {{{
// (print module "let ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let ")])};{
// (print module (rustify-identifier (car params)))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier (car params))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (car params)
Scm::func(imports::car).invoke(&[params.clone()])}])}])};{
// (print module " = ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(" = ")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(";")])}}} else {{{
// (print module "let [")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let [")])};{
// (gen-params params)
gen_minus_params.get().invoke(&[params.clone()])};{
// (print module "] = [")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("] = [")])};{
// (args (quote gen-rust) module)
args_.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module "];")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("];")])}}}};if ({
// (eq? (quote FIXLET) (body (quote kind)))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("FIXLET"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (body (quote gen-rust-inner) module)
body.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else if ({
// (and (eq? (quote COMMENT) (body (quote kind))) (eq? (quote FIXLET) ((body (quote inner)) (quote kind))))
if ({
// (eq? (quote COMMENT) (body (quote kind)))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("COMMENT"),{
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (eq? (quote FIXLET) ((body (quote inner)) (quote kind)))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("FIXLET"),{
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
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])}})}])}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let vars = vars.clone();let args_ = args_.clone();let body = body.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let arg_star_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car arg*))
transform.get().invoke(&[{
// (car arg*)
Scm::func(imports::car).invoke(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("FIXLET")} else if ({
// (eq? (quote get-params) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-args) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-args"),msg.clone()])}).is_true() {args_.clone()} else if ({
// (eq? (quote get-body) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car arg*))
gen_minus_rust.get().invoke(&[{
// (car arg*)
Scm::func(imports::car).invoke(&[arg_star_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car arg*))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car arg*)
Scm::func(imports::car).invoke(&[arg_star_.clone()])}])}} else {{
// (error "Unknown message FIXLET" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message FIXLET"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT) lib))) (set! transform (lambda (func) (func self (lambda () (make-import lib))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::*;") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT) lib)
Scm::func(imports::cons).invoke(&[Scm::symbol("IMPORT"),lib.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import lib)))
func.clone().invoke(&[self_.get(),{// Closure
let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import lib)
Scm::func(make_minus_import).invoke(&[lib.clone()])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}})});gen_minus_libname.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
Scm::func(imports::null_p).invoke(&[lib.clone()])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-libname (car lib))
Scm::func(imports::rustify_minus_libname).invoke(&[{
// (car lib)
Scm::func(imports::car).invoke(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
Scm::func(imports::null_p).invoke(&[{
// (cdr lib)
Scm::func(imports::cdr).invoke(&[lib.clone()])}])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
Scm::func(imports::cdr).invoke(&[lib.clone()])}])}}}})});gen_minus_rust.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::*;")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("::exports::*;")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_import_minus_only(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let lib = args[0].clone();let names = args[1].clone();{
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-libname (quote *uninitialized*)) (gen-imports (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (set! transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (set! free-vars (lambda () (make-set))) (set! gen-libname (lambda (module lib) (if (null? lib) (print module "") (begin (print module (rustify-libname (car lib))) (if (null? (cdr lib)) (print module "") (print module "::")) (gen-libname module (cdr lib)))))) (set! gen-imports (lambda (module names) (if (null? names) (quote DONE) (begin (print module (rustify-identifier (car names))) (print module ", ") (gen-imports module (cdr names)))))) (set! gen-rust (lambda (module) (print module "pub use crate::") (gen-libname module lib) (print module "::exports::{") (gen-imports module names) (print module "};") (println module))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_libname, gen_minus_imports, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_imports = gen_minus_imports.into_boxed();{let gen_minus_libname = gen_minus_libname.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (cons (quote IMPORT-ONLY) (cons lib names))
Scm::func(imports::cons).invoke(&[Scm::symbol("IMPORT-ONLY"),{
// (cons lib names)
Scm::func(imports::cons).invoke(&[lib.clone(),names.clone()])}])}})});transform.set({// Closure
let self_ = self_.clone();let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-import-only lib names)))
func.clone().invoke(&[self_.get(),{// Closure
let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-import-only lib names)
Scm::func(make_minus_import_minus_only).invoke(&[lib.clone(),names.clone()])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}})});gen_minus_libname.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let lib = args[1].clone();if ({
// (null? lib)
Scm::func(imports::null_p).invoke(&[lib.clone()])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else {{{
// (print module (rustify-libname (car lib)))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-libname (car lib))
Scm::func(imports::rustify_minus_libname).invoke(&[{
// (car lib)
Scm::func(imports::car).invoke(&[lib.clone()])}])}])};if ({
// (null? (cdr lib))
Scm::func(imports::null_p).invoke(&[{
// (cdr lib)
Scm::func(imports::cdr).invoke(&[lib.clone()])}])}).is_true() {{
// (print module "")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("")])}} else {{
// (print module "::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("::")])}};{
// (gen-libname module (cdr lib))
gen_minus_libname.get().invoke(&[module.clone(),{
// (cdr lib)
Scm::func(imports::cdr).invoke(&[lib.clone()])}])}}}})});gen_minus_imports.set({// Closure
let gen_minus_imports = gen_minus_imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let names = args[1].clone();if ({
// (null? names)
Scm::func(imports::null_p).invoke(&[names.clone()])}).is_true() {Scm::symbol("DONE")} else {{{
// (print module (rustify-identifier (car names)))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier (car names))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (car names)
Scm::func(imports::car).invoke(&[names.clone()])}])}])};{
// (print module ", ")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(", ")])};{
// (gen-imports module (cdr names))
gen_minus_imports.get().invoke(&[module.clone(),{
// (cdr names)
Scm::func(imports::cdr).invoke(&[names.clone()])}])}}}})});gen_minus_rust.set({// Closure
let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();let gen_minus_imports = gen_minus_imports.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "pub use crate::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("pub use crate::")])};{
// (gen-libname module lib)
gen_minus_libname.get().invoke(&[module.clone(),lib.clone()])};{
// (print module "::exports::{")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("::exports::{")])};{
// (gen-imports module names)
gen_minus_imports.get().invoke(&[module.clone(),names.clone()])};{
// (print module "};")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("};")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("IMPORT")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message IMPORT" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message IMPORT"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}}.into()
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
                        Scm::func(imports::list).invoke(&[Scm::symbol("dummy")])
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
                                    Scm::func(imports::eq_p).invoke(&[
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
                                            Scm::func(imports::set_minus_cdr_i).invoke(&[
                                                tests.clone(),
                                                {
                                                    // (cons node (cdr tests))
                                                    Scm::func(imports::cons).invoke(&[
                                                        node.clone(),
                                                        {
                                                            // (cdr tests)
                                                            Scm::func(imports::cdr)
                                                                .invoke(&[tests.clone()])
                                                        },
                                                    ])
                                                },
                                            ])
                                        };
                                        {
                                            // (make-constant (quote *UNSPECIFIED*))
                                            Scm::func(make_minus_constant)
                                                .invoke(&[Scm::symbol("*UNSPECIFIED*")])
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
                                Scm::func(imports::cdr).invoke(&[tests.clone()])
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
                                                        Scm::func(imports::make_minus_set)
                                                            .invoke(&[])
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
                                                        Scm::func(imports::print).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NOP")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NOP" msg)
                                                                Scm::func(imports::error).invoke(&[
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
                                                        Scm::func(imports::list)
                                                            .invoke(&[Scm::symbol("NULL-ARG")])
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
                                                        Scm::func(imports::make_minus_set)
                                                            .invoke(&[])
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
                                                        Scm::func(imports::print).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else if ({
                                                            // (eq? (quote free-vars) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                            Scm::func(imports::eq_p).invoke(&[
                                                                Scm::symbol("kind"),
                                                                msg.clone(),
                                                            ])
                                                        })
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NULL-ARG")
                                                        } else if ({
                                                            // (eq? (quote gen-rust) msg)
                                                            Scm::func(imports::eq_p).invoke(&[
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
                                                                    Scm::func(imports::car)
                                                                        .invoke(&[args_.clone()])
                                                                }])
                                                            }
                                                        } else {
                                                            {
                                                                // (error "Unknown message NULL-ARG" msg)
                                                                Scm::func(imports::error).invoke(&[
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
Scm::func(imports::cons).invoke(&[Scm::symbol("PROGRAM"),{
// (cons globals (cons imports (body (quote repr))))
Scm::func(imports::cons).invoke(&[globals.clone(),{
// (cons imports (body (quote repr)))
Scm::func(imports::cons).invoke(&[imports.clone(),{
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
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();{
// (i (quote gen-rust) module)
i.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},imports.clone()])}})});gen_minus_rust.set({// Closure
let gen_minus_imports = gen_minus_imports.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let libraries = libraries.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")])};{
// (print module "mod imports")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("mod imports")])};{
// (rust-block module (lambda () (gen-imports module)))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let gen_minus_imports = gen_minus_imports.clone();let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (gen-imports module)
gen_minus_imports.get().invoke(&[module.clone()])}})}])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (rust-gen-global-defs module globals)
Scm::func(imports::rust_minus_gen_minus_global_minus_defs).invoke(&[module.clone(),globals.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (print module "pub fn main()")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("pub fn main()")])};{
// (rust-block module (lambda () (println module) (println module "eprintln!(\"built with\");") (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (println module) (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init) (body (quote gen-rust) module) (println module ";")))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (println module "eprintln!(\"built with\");")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("eprintln!(\"built with\");")])};{
// (println module "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (for-each (lambda (lib) (print module "crate::") (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib) (print module "initialize();") (println module)) init)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();{{
// (print module "crate::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("crate::")])};{
// (for-each (lambda (l) (print module (rustify-libname l)) (print module "::")) lib)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();{{
// (print module (rustify-libname l))
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-libname l)
Scm::func(imports::rustify_minus_libname).invoke(&[l.clone()])}])};{
// (print module "::")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("::")])}}})},lib.clone()])};{
// (print module "initialize();")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("initialize();")])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])}}})},init.clone()])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module ";")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from(";")])}}})}])};{
// (println module)
Scm::func(imports::println).invoke(&[module.clone()])};{
// (rust-gen-modules module libraries)
Scm::func(imports::rust_minus_gen_minus_modules).invoke(&[module.clone(),libraries.clone()])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("PROGRAM")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message PROGRAM" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message PROGRAM"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_reference(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();{
// (letrec ((repr (lambda () (list (quote GET) name))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) name)))) (gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote GET) name))) (set! transform (lambda (func) (func self (lambda () self)))) (set! free-vars (lambda () (if (bor (global-variable? var) (global-function? var) (import-variable? var)) (make-set) (set-add (make-set) name)))) (set! gen-rust (lambda (module) (cond ((global-variable? var) (print module (rustify-identifier name) ".with(|value| value.get())")) ((global-function? var) (print module "Scm::func(" (rustify-identifier name) ")")) ((import-variable? var) (print module "Scm::func(imports::" (rustify-identifier name) ")")) ((boxed-variable? var) (print module (rustify-identifier name) ".get()")) (else (print module (rustify-identifier name) ".clone()"))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-var) msg) var) (else (error "Unknown message REFERENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote GET) name)
Scm::func(imports::list).invoke(&[Scm::symbol("GET"),name.clone()])}})});transform.set({// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(),{// Closure
let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}self_.get()})}])}})});free_minus_vars.set({// Closure
let var = var.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}if ({
// (bor (global-variable? var) (global-function? var) (import-variable? var))
Scm::func(imports::bor).invoke(&[{
// (global-variable? var)
Scm::func(imports::global_minus_variable_p).invoke(&[var.clone()])},{
// (global-function? var)
Scm::func(imports::global_minus_function_p).invoke(&[var.clone()])},{
// (import-variable? var)
Scm::func(imports::import_minus_variable_p).invoke(&[var.clone()])}])}).is_true() {{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}} else {{
// (set-add (make-set) name)
Scm::func(imports::set_minus_add).invoke(&[{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])},name.clone()])}}})});gen_minus_rust.set({// Closure
let var = var.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (cond ...)
if ({
// (global-variable? var)
Scm::func(imports::global_minus_variable_p).invoke(&[var.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".with(|value| value.get())")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".with(|value| value.get())")])}} else if ({
// (global-function? var)
Scm::func(imports::global_minus_function_p).invoke(&[var.clone()])}).is_true() {{
// (print module "Scm::func(" (rustify-identifier name) ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::func("),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(")")])}} else if ({
// (import-variable? var)
Scm::func(imports::import_minus_variable_p).invoke(&[var.clone()])}).is_true() {{
// (print module "Scm::func(imports::" (rustify-identifier name) ")")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("Scm::func(imports::"),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(")")])}} else if ({
// (boxed-variable? var)
Scm::func(imports::boxed_minus_variable_p).invoke(&[var.clone()])}).is_true() {{
// (print module (rustify-identifier name) ".get()")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".get()")])}} else {{
// (print module (rustify-identifier name) ".clone()")
Scm::func(imports::print).invoke(&[module.clone(),{
// (rustify-identifier name)
Scm::func(imports::rustify_minus_identifier).invoke(&[name.clone()])},Scm::from(".clone()")])}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("REFERENCE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-var) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-var"),msg.clone()])}).is_true() {var.clone()} else {{
// (error "Unknown message REFERENCE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message REFERENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_sequence(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let first = args[0].clone();let next = args[1].clone();{
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust-inner (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (set! transform (lambda (func) (func self (lambda () (let* ((a (first (quote transform) func)) (b (next (quote transform) func))) (make-sequence a b)))))) (set! free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (set! gen-rust-inner (lambda (module) (first (quote gen-rust) module) (print module ";") (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) module) (next (quote gen-rust) module)))) (set! gen-rust (lambda (module) (print module "{") (gen-rust-inner module) (print module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust_minus_inner, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("SEQUENCE"),{
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
Scm::func(imports::set_minus_union).invoke(&[{
// (first (quote free-vars))
first.clone().invoke(&[Scm::symbol("free-vars")])},{
// (next (quote free-vars))
next.clone().invoke(&[Scm::symbol("free-vars")])}])}})});gen_minus_rust_minus_inner.set({// Closure
let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (first (quote gen-rust) module)
first.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (print module ";")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from(";")])};if ({
// (eq? (quote SEQUENCE) (next (quote kind)))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("SEQUENCE"),{
// (next (quote kind))
next.clone().invoke(&[Scm::symbol("kind")])}])}).is_true() {{
// (next (quote gen-rust-inner) module)
next.clone().invoke(&[Scm::symbol("gen-rust-inner"),module.clone()])}} else {{
// (next (quote gen-rust) module)
next.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}}})});gen_minus_rust.set({// Closure
let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (print module "{")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("{")])};{
// (gen-rust-inner module)
gen_minus_rust_minus_inner.get().invoke(&[module.clone()])};{
// (print module "}")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("SEQUENCE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote gen-rust-inner) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust-inner"),msg.clone()])}).is_true() {{
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message SEQUENCE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message SEQUENCE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}}.into()
}
pub fn make_minus_testcase(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let description = args[0].clone();let body = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTCASE) description body))) (transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTCASE) description body))) (set! transform (lambda (func) (func self (lambda () (make-testcase description (body (quote transform) func)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[test]") (println module "fn " (rustify-testname description) "() {") (println module "super::initialize();") (body (quote gen-rust) module) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTCASE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTCASE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTCASE) description body)
Scm::func(imports::list).invoke(&[Scm::symbol("TESTCASE"),description.clone(),body.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-testcase description (body (quote transform) func))))
func.clone().invoke(&[self_.get(),{// Closure
let description = description.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testcase description (body (quote transform) func))
Scm::func(make_minus_testcase).invoke(&[description.clone(),{
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"),func.clone()])}])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}})});gen_minus_rust.set({// Closure
let description = description.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[test]")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("#[test]")])};{
// (println module "fn " (rustify-testname description) "() {")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("fn "),{
// (rustify-testname description)
Scm::func(imports::rustify_minus_testname).invoke(&[description.clone()])},Scm::from("() {")])};{
// (println module "super::initialize();")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("super::initialize();")])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])};{
// (println module "}")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("TESTCASE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message TESTCASE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message TESTCASE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_testsuite(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let cases = args[1].clone();{
// (letrec ((repr (lambda () (list (quote TESTSUITE) name cases))) (transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (free-vars (lambda () (make-set))) (gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote TESTSUITE) name cases))) (set! transform (lambda (func) (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases)))))) (set! free-vars (lambda () (make-set))) (set! gen-rust (lambda (module) (println module "#[cfg(test)]") (println module "mod tests {") (println module "use super::*;") (for-each (lambda (c) (c (quote gen-rust) module)) cases) (println module "}"))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote TESTSUITE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message TESTSUITE" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote TESTSUITE) name cases)
Scm::func(imports::list).invoke(&[Scm::symbol("TESTSUITE"),name.clone(),cases.clone()])}})});transform.set({// Closure
let self_ = self_.clone();let name = name.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();{
// (func self (lambda () (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))))
func.clone().invoke(&[self_.get(),{// Closure
let name = name.clone();let func = func.clone();let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-testsuite name (map (lambda (c) (c (quote transform) func)) cases))
Scm::func(make_minus_testsuite).invoke(&[name.clone(),{
// (map (lambda (c) (c (quote transform) func)) cases)
Scm::func(imports::map).invoke(&[{// Closure
let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote transform) func)
c.clone().invoke(&[Scm::symbol("transform"),func.clone()])}})},cases.clone()])}])}})}])}})});free_minus_vars.set({// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}})});gen_minus_rust.set({// Closure
let cases = cases.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{{
// (println module "#[cfg(test)]")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("#[cfg(test)]")])};{
// (println module "mod tests {")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("mod tests {")])};{
// (println module "use super::*;")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("use super::*;")])};{
// (for-each (lambda (c) (c (quote gen-rust) module)) cases)
Scm::func(imports::for_minus_each).invoke(&[{// Closure
let module = module.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let c = args[0].clone();{
// (c (quote gen-rust) module)
c.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}})},cases.clone()])};{
// (println module "}")
Scm::func(imports::println).invoke(&[module.clone(),Scm::from("}")])}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("TESTSUITE")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else {{
// (error "Unknown message TESTSUITE" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message TESTSUITE"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn make_minus_vararg_minus_abstraction(args: &[Scm]) -> Scm {
    {if args.len() != 5{panic!("invalid arity")}let params = args[0].clone();let vararg = args[1].clone();let vars = args[2].clone();let varvar = args[3].clone();let body = args[4].clone();{
// (letrec ((repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
{
// (let ((repr (quote *uninitialized*)) (transform (quote *uninitialized*)) (free-vars (quote *uninitialized*)) (gen-rust (quote *uninitialized*)) (self (quote *uninitialized*))) (begin (set! repr (lambda () (list (quote VARARG-ABSTRACTION) params (body (quote repr))))) (set! transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (set! free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (set! gen-rust (lambda (module) (define (gen-params p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))) (set! self (lambda (msg . args) (cond ((eq? (quote repr) msg) (repr)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))) self))
{let [repr, transform, free_minus_vars, gen_minus_rust, self_, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let self_ = self_.into_boxed();{let gen_minus_rust = gen_minus_rust.into_boxed();{let free_minus_vars = free_minus_vars.into_boxed();{let transform = transform.into_boxed();{let repr = repr.into_boxed();{repr.set({// Closure
let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{
// (list (quote VARARG-ABSTRACTION) params (body (quote repr)))
Scm::func(imports::list).invoke(&[Scm::symbol("VARARG-ABSTRACTION"),params.clone(),{
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
Scm::func(imports::set_minus_remove_star_).invoke(&[{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars")])},{
// (cons vararg params)
Scm::func(imports::cons).invoke(&[vararg.clone(),params.clone()])}])}})});gen_minus_rust.set({// Closure
let vararg = vararg.clone();let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let module = args[0].clone();{
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module))))
{
// (let ((gen-params (quote *uninitialized*))) (begin (set! gen-params (lambda (p* k) (if (pair? p*) (begin (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();") (gen-params (cdr p*) (+ k 1))) (begin (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);"))))) (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))))
{let gen_minus_params = Scm::symbol("*uninitialized*");{let gen_minus_params = gen_minus_params.into_boxed();{gen_minus_params.set({// Closure
let module = module.clone();let gen_minus_params = gen_minus_params.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();if ({
// (pair? p*)
Scm::func(imports::pair_p).invoke(&[p_star_.clone()])}).is_true() {{{
// (print module "let " (rustify-identifier (car p*)) " = args[" k "].clone();")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let "),{
// (rustify-identifier (car p*))
Scm::func(imports::rustify_minus_identifier).invoke(&[{
// (car p*)
Scm::func(imports::car).invoke(&[p_star_.clone()])}])},Scm::from(" = args["),k.clone(),Scm::from("].clone();")])};{
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[{
// (cdr p*)
Scm::func(imports::cdr).invoke(&[p_star_.clone()])},{
// (+ k 1)
Scm::func(imports::_plus_).invoke(&[k.clone(),Scm::from(1)])}])}}} else {{
// (print module "let " (rustify-identifier vararg) " = Scm::list(&args[" k "..]);")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("let "),{
// (rustify-identifier vararg)
Scm::func(imports::rustify_minus_identifier).invoke(&[vararg.clone()])},Scm::from(" = Scm::list(&args["),k.clone(),Scm::from("..]);")])}}})});{
// (rust-block module (lambda () (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}") (gen-params params 0) (body (quote gen-rust) module)))
Scm::func(imports::rust_minus_block).invoke(&[module.clone(),{// Closure
let module = module.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}{{
// (print module "if args.len() < " (length params) "{panic!(\"not enough args\")}")
Scm::func(imports::print).invoke(&[module.clone(),Scm::from("if args.len() < "),{
// (length params)
Scm::func(imports::length).invoke(&[params.clone()])},Scm::from("{panic!(\"not enough args\")}")])};{
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(),Scm::from(0)])};{
// (body (quote gen-rust) module)
body.clone().invoke(&[Scm::symbol("gen-rust"),module.clone()])}}})}])}}}}}}})});self_.set({// Closure
let repr = repr.clone();let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);{
// (cond ...)
if ({
// (eq? (quote repr) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("repr"),msg.clone()])}).is_true() {{
// (repr)
repr.get().invoke(&[])}} else if ({
// (eq? (quote transform) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("transform"),msg.clone()])}).is_true() {{
// (transform (car args))
transform.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote free-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("free-vars"),msg.clone()])}).is_true() {{
// (free-vars)
free_minus_vars.get().invoke(&[])}} else if ({
// (eq? (quote kind) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("kind"),msg.clone()])}).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else if ({
// (eq? (quote gen-rust) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("gen-rust"),msg.clone()])}).is_true() {{
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[{
// (car args)
Scm::func(imports::car).invoke(&[args_.clone()])}])}} else if ({
// (eq? (quote get-params) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-params"),msg.clone()])}).is_true() {params.clone()} else if ({
// (eq? (quote get-vararg) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-vararg"),msg.clone()])}).is_true() {vararg.clone()} else if ({
// (eq? (quote get-vars) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-vars"),msg.clone()])}).is_true() {vars.clone()} else if ({
// (eq? (quote get-varvar) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-varvar"),msg.clone()])}).is_true() {varvar.clone()} else if ({
// (eq? (quote get-body) msg)
Scm::func(imports::eq_p).invoke(&[Scm::symbol("get-body"),msg.clone()])}).is_true() {body.clone()} else {{
// (error "Unknown message VARARG-ABSTRACTION" msg)
Scm::func(imports::error).invoke(&[Scm::from("Unknown message VARARG-ABSTRACTION"),msg.clone()])}}}})});self_.get()}}}}}}}}}}.into()
}
pub fn procedure_minus_node_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let obj = args[0].clone();
        if ({
            // (eq? (obj (quote kind)) (quote ABSTRACTION))
            Scm::func(imports::eq_p).invoke(&[
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
                Scm::func(imports::eq_p).invoke(&[
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
            // (define (make-reference name var) ...)
            (/*NOP*/)
        };
        {
            // (define (make-assignment name var val) ...)
            (/*NOP*/)
        };
        {
            // (define (make-definition name var val) ...)
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
            // (define (make-null-arg) ...)
            (/*NOP*/)
        };
        {
            // (define (make-args arg next) ...)
            (/*NOP*/)
        };
        {
            // (define (make-fixlet params vars args body) ...)
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
        }
    };
}
