#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::ast_transforms::boxify::exports::*;
    pub use crate::sunny::ast_transforms::close_procedures::exports::*;
    pub use crate::sunny::astify::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::globals::astify_minus_library;
    pub use super::globals::astify_minus_program;
    pub use super::globals::astify_minus_toplevel;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION astify-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION astify-program"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static astify_minus_toplevel: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION astify-toplevel"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static register_minus_libraries: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL FUNCTION register-libraries"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::ast_transforms::boxify::initialize();
    crate::sunny::ast_transforms::close_procedures::initialize();
    crate::sunny::astify::initialize();
    crate::sunny::library::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::syntax::initialize();
    crate::sunny::utils::initialize();
    {
        (/*NOP*/);
        {
            // (define (astify-toplevel exp*) ...)
            globals::astify_minus_toplevel.with(|value| {
                value.set({
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let exp_star_ = args[0].clone();
                        if ({
                            // (library? (car exp*))
                            imports::library_p.with(|value| value.get()).invoke(&[{
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            }])
                        })
                        .is_true()
                        {
                            {
                                // (astify-library (library-name (car exp*)) (library-decls (car exp*)) (list (quote ())))
                                globals::astify_minus_library
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            // (library-name (car exp*))
                                            imports::library_minus_name
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (car exp*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                        },
                                        {
                                            // (library-decls (car exp*))
                                            imports::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[{
                                                    // (car exp*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()])
                                                }])
                                        },
                                        {
                                            // (list (quote ()))
                                            imports::list
                                                .with(|value| value.get())
                                                .invoke(&[Scm::Nil])
                                        },
                                    ])
                            }
                        } else {
                            {
                                // (astify-program exp*)
                                globals::astify_minus_program
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()])
                            }
                        }
                    })
                })
            })
        };
        {
            // (define (astify-library name exp* library-env) ...)
            globals::astify_minus_library.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let exp_star_ = args[1].clone();let library_minus_env = args[2].clone();{
// (letrec ((init (make-set)) (body (make-nop)) (global-env (make-core-env)) (imports (quote ())) (exports (quote ())) (process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*))))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (boxify (close-procedures body)) imports exports)))
{
// (let ((init (quote *uninitialized*)) (body (quote *uninitialized*)) (global-env (quote *uninitialized*)) (imports (quote *uninitialized*)) (exports (quote *uninitialized*)) (process-library-decls (quote *uninitialized*))) (begin (set! init (make-set)) (set! body (make-nop)) (set! global-env (make-core-env)) (set! imports (quote ())) (set! exports (quote ())) (set! process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*)))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (boxify (close-procedures body)) imports exports))))
{let [init, body, global_minus_env, imports, exports, process_minus_library_minus_decls, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_library_minus_decls = process_minus_library_minus_decls.into_boxed();{let exports = exports.into_boxed();{let imports = imports.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{let body = body.into_boxed();{let init = init.into_boxed();{init.set({
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])});body.set({
// (make-nop)
imports::make_minus_nop.with(|value| value.get()).invoke(&[])});global_minus_env.set({
// (make-core-env)
imports::make_minus_core_minus_env.with(|value| value.get()).invoke(&[])});imports.set(Scm::Nil);exports.set(Scm::Nil);process_minus_library_minus_decls.set({let exports = exports.clone();let global_minus_env = global_minus_env.clone();let process_minus_library_minus_decls = process_minus_library_minus_decls.clone();let library_minus_env = library_minus_env.clone();let init = init.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? exp*)
imports::null_p.with(|value| value.get()).invoke(&[exp_star_.clone()])}).is_true() {Scm::symbol("DONE")} else if ({
// (eq? (quote export) (caar exp*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("export"),{
// (caar exp*)
imports::caar.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{exports.set({
// (append exports (astify-export (cdar exp*) global-env))
imports::append.with(|value| value.get()).invoke(&[exports.get(),{
// (astify-export (cdar exp*) global-env)
imports::astify_minus_export.with(|value| value.get()).invoke(&[{
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}}} else if ({
// (import? (car exp*))
imports::import_p.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[{
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])},library_minus_env.clone()])};init.set({
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_.with(|value| value.get()).invoke(&[init.get(),{
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}])});imports.set({
// (append imports (astify-import (cdar exp*) global-env))
imports::append.with(|value| value.get()).invoke(&[imports.get(),{
// (astify-import (cdar exp*) global-env)
imports::astify_minus_import.with(|value| value.get()).invoke(&[{
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}}} else if ({
// (eq? (quote begin) (caar exp*))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("begin"),{
// (caar exp*)
imports::caar.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{body.set({
// (make-sequence body (astify-sequence (cdar exp*) global-env #f))
imports::make_minus_sequence.with(|value| value.get()).invoke(&[body.get(),{
// (astify-sequence (cdar exp*) global-env #f)
imports::astify_minus_sequence.with(|value| value.get()).invoke(&[{
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone()])},global_minus_env.get(),Scm::False])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});{
// (process-library-decls exp*)
process_minus_library_minus_decls.get().invoke(&[exp_star_.clone()])};{
// (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (boxify (close-procedures body)) imports exports))
{
// (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-library name globals init (boxify (close-procedures body)) imports exports)))
{let globals = {
// (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env))
imports::sort.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (symbol->string (car a)) (symbol->string (car b)))
imports::string_l__p.with(|value| value.get()).invoke(&[{
// (symbol->string (car a))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[{
// (car a)
imports::car.with(|value| value.get()).invoke(&[a.clone()])}])},{
// (symbol->string (car b))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[{
// (car b)
imports::car.with(|value| value.get()).invoke(&[b.clone()])}])}])}})},{
// (cdr global-env)
imports::cdr.with(|value| value.get()).invoke(&[global_minus_env.get()])}])};{
// (make-library name globals init (boxify (close-procedures body)) imports exports)
imports::make_minus_library.with(|value| value.get()).invoke(&[name.clone(),globals.clone(),init.get(),{
// (boxify (close-procedures body))
imports::boxify.with(|value| value.get()).invoke(&[{
// (close-procedures body)
imports::close_minus_procedures.with(|value| value.get()).invoke(&[body.get()])}])},imports.get(),exports.get()])}}}}}}}}}}}}}}})}))
        };
        {
            // (define (astify-program exp*) ...)
            globals::astify_minus_program.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (letrec ((global-env (make-core-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set)))
{
// (let ((global-env (quote *uninitialized*)) (library-env (quote *uninitialized*)) (process-imports (quote *uninitialized*))) (begin (set! global-env (make-core-env)) (set! library-env (list (quote ()))) (set! process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))))) (process-imports exp* (quote ()) (make-set))))
{let [global_minus_env, library_minus_env, process_minus_imports, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_imports = process_minus_imports.into_boxed();{let library_minus_env = library_minus_env.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{global_minus_env.set({
// (make-core-env)
imports::make_minus_core_minus_env.with(|value| value.get()).invoke(&[])});library_minus_env.set({
// (list (quote ()))
imports::list.with(|value| value.get()).invoke(&[Scm::Nil])});process_minus_imports.set({let library_minus_env = library_minus_env.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();{
// (cond ...)
if ({
// (import? (car exp*))
imports::import_p.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[{
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])},library_minus_env.get()])};{
// (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[{
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone()])},{
// (append imports (astify-import (cdar exp*) global-env))
imports::append.with(|value| value.get()).invoke(&[imports.clone(),{
// (astify-import (cdar exp*) global-env)
imports::astify_minus_import.with(|value| value.get()).invoke(&[{
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])},{
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_.with(|value| value.get()).invoke(&[init.clone(),{
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[{
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone()])}])}])}])}}} else {{
// (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{
// (let ((ast (astify-sequence exp* global-env #f))) (let ((main (boxify (close-procedures ast)))) (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env)))))))
{let ast = {
// (astify-sequence exp* global-env #f)
imports::astify_minus_sequence.with(|value| value.get()).invoke(&[exp_star_.clone(),global_minus_env.get(),Scm::False])};
// (let ((main (boxify (close-procedures ast)))) (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env))))))
let main = {
// (boxify (close-procedures ast))
imports::boxify.with(|value| value.get()).invoke(&[{
// (close-procedures ast)
imports::close_minus_procedures.with(|value| value.get()).invoke(&[ast.clone()])}])};
// (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env)))))
let globals = {
// (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env))
imports::sort.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (symbol->string (car a)) (symbol->string (car b)))
imports::string_l__p.with(|value| value.get()).invoke(&[{
// (symbol->string (car a))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[{
// (car a)
imports::car.with(|value| value.get()).invoke(&[a.clone()])}])},{
// (symbol->string (car b))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[{
// (car b)
imports::car.with(|value| value.get()).invoke(&[b.clone()])}])}])}})},{
// (cdr global-env)
imports::cdr.with(|value| value.get()).invoke(&[global_minus_env.get()])}])};{
// (make-program globals imports init main (filter cdr (car library-env)))
imports::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),main.clone(),{
// (filter cdr (car library-env))
imports::filter.with(|value| value.get()).invoke(&[imports::cdr.with(|value| value.get()),{
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.get()])}])}])}}}}}}})});{
// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(),Scm::Nil,{
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[])}])}}}}}}}}})}))
        };
        {
            // (define (register-libraries libs library-env) ...)
            globals::register_minus_libraries.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let libs = args[0].clone();let library_minus_env = args[1].clone();{
// (cond ...)
if ({
// (null? libs)
imports::null_p.with(|value| value.get()).invoke(&[libs.clone()])}).is_true() {Scm::symbol("DONE")} else if ({
// (equal? (quote (sunny testing)) (car libs))
imports::equal_p.with(|value| value.get()).invoke(&[Scm::pair(Scm::symbol("sunny"), Scm::pair(Scm::symbol("testing"), Scm::Nil)),{
// (car libs)
imports::car.with(|value| value.get()).invoke(&[libs.clone()])}])}).is_true() {{
// (register-libraries (cdr libs) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[{
// (cdr libs)
imports::cdr.with(|value| value.get()).invoke(&[libs.clone()])},library_minus_env.clone()])}} else if ({
// (assoc (car libs) (car library-env))
imports::assoc.with(|value| value.get()).invoke(&[{
// (car libs)
imports::car.with(|value| value.get()).invoke(&[libs.clone()])},{
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.clone()])}])}).is_true() {{
// (register-libraries (cdr libs) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[{
// (cdr libs)
imports::cdr.with(|value| value.get()).invoke(&[libs.clone()])},library_minus_env.clone()])}} else {{{
// (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
{
// (let ((lib (get-lib (car libs)))) (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env))))))
{let lib = {
// (get-lib (car libs))
imports::get_minus_lib.with(|value| value.get()).invoke(&[{
// (car libs)
imports::car.with(|value| value.get()).invoke(&[libs.clone()])}])};
// (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env)))))
let libast = if ({
// (library? lib)
imports::library_p.with(|value| value.get()).invoke(&[lib.clone()])}).is_true() {{
// (astify-library (library-name lib) (library-decls lib) library-env)
globals::astify_minus_library.with(|value| value.get()).invoke(&[{
// (library-name lib)
imports::library_minus_name.with(|value| value.get()).invoke(&[lib.clone()])},{
// (library-decls lib)
imports::library_minus_decls.with(|value| value.get()).invoke(&[lib.clone()])},library_minus_env.clone()])}} else {Scm::False};{
// (set-car! library-env (cons (cons (car libs) libast) (car library-env)))
imports::set_minus_car_i.with(|value| value.get()).invoke(&[library_minus_env.clone(),{
// (cons (cons (car libs) libast) (car library-env))
imports::cons.with(|value| value.get()).invoke(&[{
// (cons (car libs) libast)
imports::cons.with(|value| value.get()).invoke(&[{
// (car libs)
imports::car.with(|value| value.get()).invoke(&[libs.clone()])},libast.clone()])},{
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.clone()])}])}])}}}};{
// (register-libraries (cdr libs) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[{
// (cdr libs)
imports::cdr.with(|value| value.get()).invoke(&[libs.clone()])},library_minus_env.clone()])}}}}})}))
        }
    };
}
