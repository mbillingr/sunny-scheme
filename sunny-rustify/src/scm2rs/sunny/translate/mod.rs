#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::chibi::filesystem::exports::*;
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::{
        file_minus_exists_p, open_minus_input_minus_file, open_minus_output_minus_file,
    };
    pub use crate::scheme::read::exports::read;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::module_tree::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::scm_minus__g_ast;
    pub use super::imports::rust_minus_gen_minus_in_minus_module;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL importset-libname"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static reduce: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL reduce"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library_minus_ext: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library-ext"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_path: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-path"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static check_minus_imports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL check-imports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_exports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-exports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_all: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-all"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scan_minus_out_minus_defines: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scan-out-defines"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_regular_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->regular-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_and: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->and"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clauses: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clauses"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_cond: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->cond"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_consequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-consequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static if_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL if-alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->alternative"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_rec: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-rec"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_seq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-seq"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_scope_minus_let: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->scope-let"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_definition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->definition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static wrap_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL wrap-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->assignment"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->constant"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->reference"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static get_minus_lib: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL get-lib"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static assoc: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assoc"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sort: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sort"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_libnames: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-libnames"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static register_minus_libraries: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL register-libraries"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static program_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL program->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scm_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scm->ast"))}
}

thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

pub fn initialize() {
    if INITIALIZED.with(|x| x.get()) {
        return;
    }
    INITIALIZED.with(|x| x.set(true));

    crate::scheme::base::initialize();
    crate::scheme::write::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::read::initialize();
    crate::scheme::file::initialize();
    crate::chibi::filesystem::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::env::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::rust::module::initialize();
    crate::sunny::rust::module_tree::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::utils::initialize();
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        // (define (scm->ast exp*) (if (library? (car exp*)) (library->ast (library-name (car exp*)) (library-decls (car exp*)) (list (quote ()))) (program->ast exp*)))
        globals::scm_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let exp_star_ = args[0].clone();
                    // (letrec () (if (library? (car exp*)) (library->ast (library-name (car exp*)) (library-decls (car exp*)) (list (quote ()))) (program->ast exp*)))
                    {
                        if (
                            // (library? (car exp*))
                            globals::library_p.with(|value| value.get()).invoke(&[
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (library->ast (library-name (car exp*)) (library-decls (car exp*)) (list (quote ())))
                            globals::library_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[
                                    // (library-name (car exp*))
                                    globals::library_minus_name
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ]),
                                    // (library-decls (car exp*))
                                    globals::library_minus_decls
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ]),
                                    // (list (quote ()))
                                    imports::list.with(|value| value.get()).invoke(&[Scm::Nil]),
                                ])
                        } else {
                            // (program->ast exp*)
                            globals::program_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[exp_star_.clone()])
                        }
                    }
                })
            })
        });
        // (define (program->ast exp*) (define global-env (make-global-env)) (define library-env (list (quote ()))) (define (process-imports exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))) (process-imports exp* (quote ()) (make-set)))
        globals::program_minus__g_ast.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();
// (letrec ((global-env (make-global-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set)))
{let global_minus_env = Scm::uninitialized().into_boxed();
let library_minus_env = Scm::uninitialized().into_boxed();
let process_minus_imports = Scm::uninitialized().into_boxed();
global_minus_env.set(
// (make-global-env)
imports::make_minus_global_minus_env.with(|value| value.get()).invoke(&[]));
library_minus_env.set(
// (list (quote ()))
imports::list.with(|value| value.get()).invoke(&[Scm::Nil,]));
process_minus_imports.set({let library_minus_env = library_minus_env.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();
// (letrec () (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))))
{
// (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))
if (
// (import? (car exp*))
globals::import_p.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),])).is_true() {{
// (register-libraries (import-libnames (car exp*)) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[
// (import-libnames (car exp*))
globals::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),]),library_minus_env.get(),]);
// (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone(),]),
// (append imports (sexpr->import (cdar exp*) global-env))
globals::append.with(|value| value.get()).invoke(&[imports.clone(),
// (sexpr->import (cdar exp*) global-env)
globals::sexpr_minus__g_import.with(|value| value.get()).invoke(&[
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone(),]),global_minus_env.get(),]),]),
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_.with(|value| value.get()).invoke(&[init.clone(),
// (import-libnames (car exp*))
globals::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),]),]),])}} else {
// (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{let [main, ] = [
// (boxify (sexpr->sequence exp* global-env #f))
globals::boxify.with(|value| value.get()).invoke(&[
// (sexpr->sequence exp* global-env #f)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[exp_star_.clone(),global_minus_env.get(),Scm::False,]),]),];
// (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{let [globals, ] = [
// (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env))
globals::sort.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();
// (letrec () (string<? (symbol->string (car a)) (symbol->string (car b))))
{
// (string<? (symbol->string (car a)) (symbol->string (car b)))
imports::string_l__p.with(|value| value.get()).invoke(&[
// (symbol->string (car a))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[
// (car a)
imports::car.with(|value| value.get()).invoke(&[a.clone(),]),]),
// (symbol->string (car b))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[
// (car b)
imports::car.with(|value| value.get()).invoke(&[b.clone(),]),]),])}})},
// (cdr global-env)
imports::cdr.with(|value| value.get()).invoke(&[global_minus_env.get(),]),]),];
// (let* () (make-program globals imports init main (filter cdr (car library-env))))

// (make-program globals imports init main (filter cdr (car library-env)))
imports::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),main.clone(),
// (filter cdr (car library-env))
imports::filter.with(|value| value.get()).invoke(&[imports::cdr.with(|value| value.get()),
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.get(),]),]),])}}}}})});

// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(),Scm::Nil,
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[]),])}})}));
        // (define (library->ast name exp* library-env) (library-decls->ast name exp* (make-set) (make-nop) (make-global-env) library-env (quote ()) (quote ())))
        globals::library_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let exp_star_ = args[1].clone();
                    let library_minus_env = args[2].clone();
                    // (letrec () (library-decls->ast name exp* (make-set) (make-nop) (make-global-env) library-env (quote ()) (quote ())))
                    {
                        // (library-decls->ast name exp* (make-set) (make-nop) (make-global-env) library-env (quote ()) (quote ()))
                        globals::library_minus_decls_minus__g_ast
                            .with(|value| value.get())
                            .invoke(&[
                                name.clone(),
                                exp_star_.clone(),
                                // (make-set)
                                imports::make_minus_set
                                    .with(|value| value.get())
                                    .invoke(&[]),
                                // (make-nop)
                                imports::make_minus_nop
                                    .with(|value| value.get())
                                    .invoke(&[]),
                                // (make-global-env)
                                imports::make_minus_global_minus_env
                                    .with(|value| value.get())
                                    .invoke(&[]),
                                library_minus_env.clone(),
                                Scm::Nil,
                                Scm::Nil,
                            ])
                    }
                })
            })
        });
        // (define (library-decls->ast name exp* init body global-env library-env imports exports) (cond ((null? exp*) (make-library name (cdr global-env) init body imports exports)) ((eq? (quote export) (caar exp*)) (library-decls->ast name (cdr exp*) init body global-env library-env imports (append exports (sexpr->export (cdar exp*) global-env)))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (library-decls->ast name (cdr exp*) (set-add* init (import-libnames (car exp*))) body global-env library-env (append imports (sexpr->import (cdar exp*) global-env)) exports)) ((eq? (quote begin) (caar exp*)) (library-decls->ast name (cdr exp*) init (make-sequence body (sexpr->sequence (cdar exp*) global-env #f)) global-env library-env imports exports))))
        globals::library_minus_decls_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 8 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let exp_star_ = args[1].clone();
                    let init = args[2].clone();
                    let body = args[3].clone();
                    let global_minus_env = args[4].clone();
                    let library_minus_env = args[5].clone();
                    let imports = args[6].clone();
                    let exports = args[7].clone();
                    // (letrec () (cond ((null? exp*) (make-library name (cdr global-env) init body imports exports)) ((eq? (quote export) (caar exp*)) (library-decls->ast name (cdr exp*) init body global-env library-env imports (append exports (sexpr->export (cdar exp*) global-env)))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (library-decls->ast name (cdr exp*) (set-add* init (import-libnames (car exp*))) body global-env library-env (append imports (sexpr->import (cdar exp*) global-env)) exports)) ((eq? (quote begin) (caar exp*)) (library-decls->ast name (cdr exp*) init (make-sequence body (sexpr->sequence (cdar exp*) global-env #f)) global-env library-env imports exports))))
                    {
                        // (cond ((null? exp*) (make-library name (cdr global-env) init body imports exports)) ((eq? (quote export) (caar exp*)) (library-decls->ast name (cdr exp*) init body global-env library-env imports (append exports (sexpr->export (cdar exp*) global-env)))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (library-decls->ast name (cdr exp*) (set-add* init (import-libnames (car exp*))) body global-env library-env (append imports (sexpr->import (cdar exp*) global-env)) exports)) ((eq? (quote begin) (caar exp*)) (library-decls->ast name (cdr exp*) init (make-sequence body (sexpr->sequence (cdar exp*) global-env #f)) global-env library-env imports exports)))
                        if (
                            // (null? exp*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[exp_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-library name (cdr global-env) init body imports exports)
                            imports::make_minus_library
                                .with(|value| value.get())
                                .invoke(&[
                                    name.clone(),
                                    // (cdr global-env)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[global_minus_env.clone()]),
                                    init.clone(),
                                    body.clone(),
                                    imports.clone(),
                                    exports.clone(),
                                ])
                        } else if (
                            // (eq? (quote export) (caar exp*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("export"),
                                // (caar exp*)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (library-decls->ast name (cdr exp*) init body global-env library-env imports (append exports (sexpr->export (cdar exp*) global-env)))
                            globals::library_minus_decls_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[
                                    name.clone(),
                                    // (cdr exp*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[exp_star_.clone()]),
                                    init.clone(),
                                    body.clone(),
                                    global_minus_env.clone(),
                                    library_minus_env.clone(),
                                    imports.clone(),
                                    // (append exports (sexpr->export (cdar exp*) global-env))
                                    globals::append.with(|value| value.get()).invoke(&[
                                        exports.clone(),
                                        // (sexpr->export (cdar exp*) global-env)
                                        globals::sexpr_minus__g_export
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cdar exp*)
                                                imports::cdar
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                                global_minus_env.clone(),
                                            ]),
                                    ]),
                                ])
                        } else if (
                            // (import? (car exp*))
                            globals::import_p.with(|value| value.get()).invoke(&[
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            {
                                // (register-libraries (import-libnames (car exp*)) library-env)
                                globals::register_minus_libraries
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (import-libnames (car exp*))
                                        globals::import_minus_libnames
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car exp*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                            ]),
                                        library_minus_env.clone(),
                                    ]);
                                // (library-decls->ast name (cdr exp*) (set-add* init (import-libnames (car exp*))) body global-env library-env (append imports (sexpr->import (cdar exp*) global-env)) exports)
                                globals::library_minus_decls_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[
                                        name.clone(),
                                        // (cdr exp*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[exp_star_.clone()]),
                                        // (set-add* init (import-libnames (car exp*)))
                                        imports::set_minus_add_star_
                                            .with(|value| value.get())
                                            .invoke(&[
                                                init.clone(),
                                                // (import-libnames (car exp*))
                                                globals::import_minus_libnames
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()]),
                                                    ]),
                                            ]),
                                        body.clone(),
                                        global_minus_env.clone(),
                                        library_minus_env.clone(),
                                        // (append imports (sexpr->import (cdar exp*) global-env))
                                        globals::append.with(|value| value.get()).invoke(&[
                                            imports.clone(),
                                            // (sexpr->import (cdar exp*) global-env)
                                            globals::sexpr_minus__g_import
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdar exp*)
                                                    imports::cdar
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()]),
                                                    global_minus_env.clone(),
                                                ]),
                                        ]),
                                        exports.clone(),
                                    ])
                            }
                        } else if (
                            // (eq? (quote begin) (caar exp*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("begin"),
                                // (caar exp*)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (library-decls->ast name (cdr exp*) init (make-sequence body (sexpr->sequence (cdar exp*) global-env #f)) global-env library-env imports exports)
                            globals::library_minus_decls_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[
                                    name.clone(),
                                    // (cdr exp*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[exp_star_.clone()]),
                                    init.clone(),
                                    // (make-sequence body (sexpr->sequence (cdar exp*) global-env #f))
                                    imports::make_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[
                                            body.clone(),
                                            // (sexpr->sequence (cdar exp*) global-env #f)
                                            globals::sexpr_minus__g_sequence
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdar exp*)
                                                    imports::cdar
                                                        .with(|value| value.get())
                                                        .invoke(&[exp_star_.clone()]),
                                                    global_minus_env.clone(),
                                                    Scm::False,
                                                ]),
                                        ]),
                                    global_minus_env.clone(),
                                    library_minus_env.clone(),
                                    imports.clone(),
                                    exports.clone(),
                                ])
                        } else {
                            Scm::symbol("*UNSPECIFIED*")
                        }
                    }
                })
            })
        });
        // (define (register-libraries libs library-env) (cond ((null? libs) (quote DONE)) ((equal? (quote (sunny testing)) (car libs)) (register-libraries (cdr libs) library-env)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env))))
        globals::register_minus_libraries.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let libs = args[0].clone();
                    let library_minus_env = args[1].clone();
                    // (letrec () (cond ((null? libs) (quote DONE)) ((equal? (quote (sunny testing)) (car libs)) (register-libraries (cdr libs) library-env)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env))))
                    {
                        // (cond ((null? libs) (quote DONE)) ((equal? (quote (sunny testing)) (car libs)) (register-libraries (cdr libs) library-env)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env)))
                        if (
                            // (null? libs)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[libs.clone()])
                        )
                        .is_true()
                        {
                            Scm::symbol("DONE")
                        } else if (
                            // (equal? (quote (sunny testing)) (car libs))
                            imports::equal_p.with(|value| value.get()).invoke(&[
                                Scm::pair(
                                    Scm::symbol("sunny"),
                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                ),
                                // (car libs)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[libs.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (register-libraries (cdr libs) library-env)
                            globals::register_minus_libraries
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr libs)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[libs.clone()]),
                                    library_minus_env.clone(),
                                ])
                        } else if (
                            // (assoc (car libs) (car library-env))
                            globals::assoc.with(|value| value.get()).invoke(&[
                                // (car libs)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[libs.clone()]),
                                // (car library-env)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[library_minus_env.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (register-libraries (cdr libs) library-env)
                            globals::register_minus_libraries
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr libs)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[libs.clone()]),
                                    library_minus_env.clone(),
                                ])
                        } else {
                            {
                                // (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
                                {
                                    let [lib] = [
                                        // (get-lib (car libs))
                                        globals::get_minus_lib.with(|value| value.get()).invoke(&[
                                            // (car libs)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[libs.clone()]),
                                        ]),
                                    ];
                                    // (let* ((libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
                                    {
                                        let [libast] = [
                                            if (
                                                // (library? lib)
                                                globals::library_p
                                                    .with(|value| value.get())
                                                    .invoke(&[lib.clone()])
                                            )
                                            .is_true()
                                            {
                                                // (library->ast (library-name lib) (library-decls lib) library-env)
                                                globals::library_minus__g_ast
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (library-name lib)
                                                        globals::library_minus_name
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                        // (library-decls lib)
                                                        globals::library_minus_decls
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                        library_minus_env.clone(),
                                                    ])
                                            } else {
                                                Scm::False
                                            },
                                        ];
                                        // (let* () (set-car! library-env (cons (cons (car libs) libast) (car library-env))))

                                        // (set-car! library-env (cons (cons (car libs) libast) (car library-env)))
                                        imports::set_minus_car_i.with(|value| value.get()).invoke(
                                            &[
                                                library_minus_env.clone(),
                                                // (cons (cons (car libs) libast) (car library-env))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    // (cons (car libs) libast)
                                                    imports::cons.with(|value| value.get()).invoke(
                                                        &[
                                                            // (car libs)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[libs.clone()]),
                                                            libast.clone(),
                                                        ],
                                                    ),
                                                    // (car library-env)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[library_minus_env.clone()]),
                                                ]),
                                            ],
                                        )
                                    }
                                };
                                // (register-libraries (cdr libs) library-env)
                                globals::register_minus_libraries
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr libs)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[libs.clone()]),
                                        library_minus_env.clone(),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->ast exp env tail?) (if (atom? exp) (if (symbol? exp) (sexpr->reference exp env) (sexpr->constant exp env)) (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))))
        globals::sexpr_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (if (atom? exp) (if (symbol? exp) (sexpr->reference exp env) (sexpr->constant exp env)) (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))))
                    {
                        if (
                            // (atom? exp)
                            imports::atom_p
                                .with(|value| value.get())
                                .invoke(&[exp.clone()])
                        )
                        .is_true()
                        {
                            if (
                                // (symbol? exp)
                                imports::symbol_p
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()])
                            )
                            .is_true()
                            {
                                // (sexpr->reference exp env)
                                globals::sexpr_minus__g_reference
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone(), env.clone()])
                            } else {
                                // (sexpr->constant exp env)
                                globals::sexpr_minus__g_constant
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone(), env.clone()])
                            }
                        } else {
                            // (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) ((and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env))) (sexpr->testsuite (cadr exp) (cddr exp) env)) ((and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env))) (sexpr->assert (cadr exp) env)) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))
                            if (
                                // (eq? (quote quote) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("quote"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->constant (cadr exp) env)
                                globals::sexpr_minus__g_constant
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (eq? (quote set!) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("set!"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->assignment (cadr exp) (caddr exp) env)
                                globals::sexpr_minus__g_assignment
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (caddr exp)
                                        imports::caddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (eq? (quote define) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("define"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->definition exp env))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->definition exp env)
                                        globals::sexpr_minus__g_definition
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone(), env.clone()]),
                                    ])
                            } else if (
                                // (eq? (quote lambda) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("lambda"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->abstraction (cadr exp) (cddr exp) env)
                                globals::sexpr_minus__g_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (cddr exp)
                                        imports::cddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (eq? (quote begin) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("begin"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->sequence (cdr exp) env tail?)
                                globals::sexpr_minus__g_sequence
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr exp)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            } else if (
                                // (eq? (quote let) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("let"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-let (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_let
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote let*) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("let*"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-seq (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_seq
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote letrec) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("letrec"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->scope-rec (cadr exp) (cddr exp) env tail?)
                                        globals::sexpr_minus__g_scope_minus_rec
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cadr exp)
                                                imports::cadr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cddr exp)
                                                imports::cddr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote if) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("if"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
                                globals::sexpr_minus__g_alternative
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (if-condition exp)
                                        globals::if_minus_condition
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (if-consequence exp)
                                        globals::if_minus_consequence
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (if-alternative exp)
                                        globals::if_minus_alternative
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            } else if (
                                // (eq? (quote cond) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("cond"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->cond (cond-clauses exp) env tail?)
                                        globals::sexpr_minus__g_cond
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cond-clauses exp)
                                                globals::cond_minus_clauses
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (eq? (quote and) (car exp))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("and"),
                                    // (car exp)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->and (cdr exp) env tail?)
                                        globals::sexpr_minus__g_and
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cdr exp)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            } else if (
                                // (and (eq? (quote testsuite) (car exp)) (not (lookup (quote testsuite) env)))
                                if (
                                    // (eq? (quote testsuite) (car exp))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("testsuite"),
                                        // (car exp)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                    ])
                                )
                                .is_true()
                                {
                                    // (not (lookup (quote testsuite) env))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (lookup (quote testsuite) env)
                                        imports::lookup
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("testsuite"), env.clone()]),
                                    ])
                                } else {
                                    Scm::False
                                }
                            )
                            .is_true()
                            {
                                // (sexpr->testsuite (cadr exp) (cddr exp) env)
                                globals::sexpr_minus__g_testsuite
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        // (cddr exp)
                                        imports::cddr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else if (
                                // (and (eq? (quote assert) (car exp)) (not (lookup (quote assert) env)))
                                if (
                                    // (eq? (quote assert) (car exp))
                                    imports::eq_p.with(|value| value.get()).invoke(&[
                                        Scm::symbol("assert"),
                                        // (car exp)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                    ])
                                )
                                .is_true()
                                {
                                    // (not (lookup (quote assert) env))
                                    imports::not.with(|value| value.get()).invoke(&[
                                        // (lookup (quote assert) env)
                                        imports::lookup
                                            .with(|value| value.get())
                                            .invoke(&[Scm::symbol("assert"), env.clone()]),
                                    ])
                                } else {
                                    Scm::False
                                }
                            )
                            .is_true()
                            {
                                // (sexpr->assert (cadr exp) env)
                                globals::sexpr_minus__g_assert
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadr exp)
                                        imports::cadr
                                            .with(|value| value.get())
                                            .invoke(&[exp.clone()]),
                                        env.clone(),
                                    ])
                            } else {
                                // (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))
                                globals::wrap_minus_sexpr
                                    .with(|value| value.get())
                                    .invoke(&[
                                        exp.clone(),
                                        // (sexpr->application (car exp) (cdr exp) env tail?)
                                        globals::sexpr_minus__g_application
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car exp)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                // (cdr exp)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp.clone()]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (wrap-sexpr exp node) (make-comment exp node))
        globals::wrap_minus_sexpr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let node = args[1].clone();
                    // (letrec () (make-comment exp node))
                    {
                        // (make-comment exp node)
                        imports::make_minus_comment
                            .with(|value| value.get())
                            .invoke(&[exp.clone(), node.clone()])
                    }
                })
            })
        });
        // (define (sexpr->constant exp env) (make-constant exp))
        globals::sexpr_minus__g_constant.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (make-constant exp))
                    {
                        // (make-constant exp)
                        imports::make_minus_constant
                            .with(|value| value.get())
                            .invoke(&[exp.clone()])
                    }
                })
            })
        });
        // (define (sexpr->reference name env) (let ((var (ensure-var! name env))) (make-reference name var)))
        globals::sexpr_minus__g_reference.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let ((var (ensure-var! name env))) (make-reference name var)))
                    {
                        // (let ((var (ensure-var! name env))) (make-reference name var))
                        {
                            let [var] = [
                                // (ensure-var! name env)
                                imports::ensure_minus_var_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            // (make-reference name var)
                            imports::make_minus_reference
                                .with(|value| value.get())
                                .invoke(&[name.clone(), var.clone()])
                        }
                    }
                })
            })
        });
        // (define (sexpr->assignment name exp env) (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val)))
        globals::sexpr_minus__g_assignment.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let exp = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val)))
                    {
                        // (let ((val (sexpr->ast exp env #f)) (var (ensure-var! name env))) (variable-set-mutable! var) (make-assignment name var val))
                        {
                            let [val, var] = [
                                // (sexpr->ast exp env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone(), env.clone(), Scm::False]),
                                // (ensure-var! name env)
                                imports::ensure_minus_var_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            {
                                // (variable-set-mutable! var)
                                imports::variable_minus_set_minus_mutable_i
                                    .with(|value| value.get())
                                    .invoke(&[var.clone()]);
                                // (make-assignment name var val)
                                imports::make_minus_assignment
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), var.clone(), val.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->definition exp env) (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (make-assignment name var val)))
        globals::sexpr_minus__g_definition.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let exp = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (make-assignment name var val)))
                    {
                        // (let* ((name (definition-variable exp)) (value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (make-assignment name var val))
                        {
                            let [name] = [
                                // (definition-variable exp)
                                globals::definition_minus_variable
                                    .with(|value| value.get())
                                    .invoke(&[exp.clone()]),
                            ];
                            // (let* ((value (definition-value exp)) (var (ensure-var! name env)) (val (sexpr->ast value env #f))) (make-assignment name var val))
                            {
                                let [value] = [
                                    // (definition-value exp)
                                    globals::definition_minus_value
                                        .with(|value| value.get())
                                        .invoke(&[exp.clone()]),
                                ];
                                // (let* ((var (ensure-var! name env)) (val (sexpr->ast value env #f))) (make-assignment name var val))
                                {
                                    let [var] = [
                                        // (ensure-var! name env)
                                        imports::ensure_minus_var_i
                                            .with(|value| value.get())
                                            .invoke(&[name.clone(), env.clone()]),
                                    ];
                                    // (let* ((val (sexpr->ast value env #f))) (make-assignment name var val))
                                    {
                                        let [val] = [
                                            // (sexpr->ast value env #f)
                                            globals::sexpr_minus__g_ast
                                                .with(|value| value.get())
                                                .invoke(&[value.clone(), env.clone(), Scm::False]),
                                        ];
                                        // (let* () (make-assignment name var val))

                                        // (make-assignment name var val)
                                        imports::make_minus_assignment
                                            .with(|value| value.get())
                                            .invoke(&[name.clone(), var.clone(), val.clone()])
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->alternative condition consequent alternative env tail?) (let* ((x (sexpr->ast condition env #f)) (a (sexpr->ast consequent env tail?)) (b (sexpr->ast alternative env tail?))) (make-alternative x a b)))
        globals::sexpr_minus__g_alternative.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 5 {
                        panic!("invalid arity")
                    }
                    let condition = args[0].clone();
                    let consequent = args[1].clone();
                    let alternative = args[2].clone();
                    let env = args[3].clone();
                    let tail_p = args[4].clone();
                    // (letrec () (let* ((x (sexpr->ast condition env #f)) (a (sexpr->ast consequent env tail?)) (b (sexpr->ast alternative env tail?))) (make-alternative x a b)))
                    {
                        // (let* ((x (sexpr->ast condition env #f)) (a (sexpr->ast consequent env tail?)) (b (sexpr->ast alternative env tail?))) (make-alternative x a b))
                        {
                            let [x] = [
                                // (sexpr->ast condition env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[condition.clone(), env.clone(), Scm::False]),
                            ];
                            // (let* ((a (sexpr->ast consequent env tail?)) (b (sexpr->ast alternative env tail?))) (make-alternative x a b))
                            {
                                let [a] = [
                                    // (sexpr->ast consequent env tail?)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[consequent.clone(), env.clone(), tail_p.clone()]),
                                ];
                                // (let* ((b (sexpr->ast alternative env tail?))) (make-alternative x a b))
                                {
                                    let [b] = [
                                        // (sexpr->ast alternative env tail?)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[
                                                alternative.clone(),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ];
                                    // (let* () (make-alternative x a b))

                                    // (make-alternative x a b)
                                    imports::make_minus_alternative
                                        .with(|value| value.get())
                                        .invoke(&[x.clone(), a.clone(), b.clone()])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->application func arg* env tail?) (if (and (pair? func) (eq? (car func) (quote lambda))) (sexpr->fixlet (cadr func) (cddr func) arg* env tail?) (sexpr->regular-application func arg* env tail?)))
        globals::sexpr_minus__g_application.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let func = args[0].clone();
                    let arg_star_ = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (if (and (pair? func) (eq? (car func) (quote lambda))) (sexpr->fixlet (cadr func) (cddr func) arg* env tail?) (sexpr->regular-application func arg* env tail?)))
                    {
                        if (
                            // (and (pair? func) (eq? (car func) (quote lambda)))
                            if (
                                // (pair? func)
                                imports::pair_p
                                    .with(|value| value.get())
                                    .invoke(&[func.clone()])
                            )
                            .is_true()
                            {
                                // (eq? (car func) (quote lambda))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    // (car func)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[func.clone()]),
                                    Scm::symbol("lambda"),
                                ])
                            } else {
                                Scm::False
                            }
                        )
                        .is_true()
                        {
                            // (sexpr->fixlet (cadr func) (cddr func) arg* env tail?)
                            globals::sexpr_minus__g_fixlet
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr func)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[func.clone()]),
                                    // (cddr func)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[func.clone()]),
                                    arg_star_.clone(),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else {
                            // (sexpr->regular-application func arg* env tail?)
                            globals::sexpr_minus__g_regular_minus_application
                                .with(|value| value.get())
                                .invoke(&[
                                    func.clone(),
                                    arg_star_.clone(),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->regular-application func arg* env tail?) (let ((func (sexpr->ast func env #f))) (let ((args (sexpr->args arg* env))) (make-application func args tail?))))
        globals::sexpr_minus__g_regular_minus_application.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let func = args[0].clone();
                    let arg_star_ = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (let ((func (sexpr->ast func env #f))) (let ((args (sexpr->args arg* env))) (make-application func args tail?))))
                    {
                        // (let ((func (sexpr->ast func env #f))) (let ((args (sexpr->args arg* env))) (make-application func args tail?)))
                        {
                            let [func] = [
                                // (sexpr->ast func env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[func.clone(), env.clone(), Scm::False]),
                            ];
                            // (let ((args (sexpr->args arg* env))) (make-application func args tail?))
                            {
                                let [args_] = [
                                    // (sexpr->args arg* env)
                                    globals::sexpr_minus__g_args
                                        .with(|value| value.get())
                                        .invoke(&[arg_star_.clone(), env.clone()]),
                                ];
                                // (make-application func args tail?)
                                imports::make_minus_application
                                    .with(|value| value.get())
                                    .invoke(&[func.clone(), args_.clone(), tail_p.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->fixlet param* body arg* env tail?) (let* ((local-env (adjoin-local-env param* env)) (args (sexpr->args arg* env)) (func-body (sexpr->sequence body local-env tail?))) (make-fixlet param* func-body args)))
        globals::sexpr_minus__g_fixlet.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 5 {
                        panic!("invalid arity")
                    }
                    let param_star_ = args[0].clone();
                    let body = args[1].clone();
                    let arg_star_ = args[2].clone();
                    let env = args[3].clone();
                    let tail_p = args[4].clone();
                    // (letrec () (let* ((local-env (adjoin-local-env param* env)) (args (sexpr->args arg* env)) (func-body (sexpr->sequence body local-env tail?))) (make-fixlet param* func-body args)))
                    {
                        // (let* ((local-env (adjoin-local-env param* env)) (args (sexpr->args arg* env)) (func-body (sexpr->sequence body local-env tail?))) (make-fixlet param* func-body args))
                        {
                            let [local_minus_env] = [
                                // (adjoin-local-env param* env)
                                imports::adjoin_minus_local_minus_env
                                    .with(|value| value.get())
                                    .invoke(&[param_star_.clone(), env.clone()]),
                            ];
                            // (let* ((args (sexpr->args arg* env)) (func-body (sexpr->sequence body local-env tail?))) (make-fixlet param* func-body args))
                            {
                                let [args_] = [
                                    // (sexpr->args arg* env)
                                    globals::sexpr_minus__g_args
                                        .with(|value| value.get())
                                        .invoke(&[arg_star_.clone(), env.clone()]),
                                ];
                                // (let* ((func-body (sexpr->sequence body local-env tail?))) (make-fixlet param* func-body args))
                                {
                                    let [func_minus_body] = [
                                        // (sexpr->sequence body local-env tail?)
                                        globals::sexpr_minus__g_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                body.clone(),
                                                local_minus_env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ];
                                    // (let* () (make-fixlet param* func-body args))

                                    // (make-fixlet param* func-body args)
                                    imports::make_minus_fixlet
                                        .with(|value| value.get())
                                        .invoke(&[
                                            param_star_.clone(),
                                            func_minus_body.clone(),
                                            args_.clone(),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->args arg* env) (if (null? arg*) (make-null-arg) (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))))
        globals::sexpr_minus__g_args.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let arg_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (if (null? arg*) (make-null-arg) (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))))
                    {
                        if (
                            // (null? arg*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[arg_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-null-arg)
                            imports::make_minus_null_minus_arg
                                .with(|value| value.get())
                                .invoke(&[])
                        } else {
                            // (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))
                            imports::make_minus_args.with(|value| value.get()).invoke(&[
                                // (sexpr->ast (car arg*) env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car arg*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[arg_star_.clone()]),
                                        env.clone(),
                                        Scm::False,
                                    ]),
                                // (sexpr->args (cdr arg*) env)
                                globals::sexpr_minus__g_args
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr arg*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[arg_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-seq bindings body env tail?) (if (null? bindings) (sexpr->sequence body env tail?) (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)))
        globals::sexpr_minus__g_scope_minus_seq.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (if (null? bindings) (sexpr->sequence body env tail?) (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)))
                    {
                        if (
                            // (null? bindings)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[bindings.clone()])
                        )
                        .is_true()
                        {
                            // (sexpr->sequence body env tail?)
                            globals::sexpr_minus__g_sequence
                                .with(|value| value.get())
                                .invoke(&[body.clone(), env.clone(), tail_p.clone()])
                        } else {
                            // (sexpr->scope-let (list (car bindings)) (list (cons (quote let*) (cons (cdr bindings) body))) env tail?)
                            globals::sexpr_minus__g_scope_minus_let
                                .with(|value| value.get())
                                .invoke(&[
                                    // (list (car bindings))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (car bindings)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[bindings.clone()]),
                                    ]),
                                    // (list (cons (quote let*) (cons (cdr bindings) body)))
                                    imports::list.with(|value| value.get()).invoke(&[
                                        // (cons (quote let*) (cons (cdr bindings) body))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            Scm::symbol("let*"),
                                            // (cons (cdr bindings) body)
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                // (cdr bindings)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[bindings.clone()]),
                                                body.clone(),
                                            ]),
                                        ]),
                                    ]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-rec bindings body env tail?) (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
        globals::sexpr_minus__g_scope_minus_rec.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args)))
                    {
                        // (let* ((params (map (lambda (b) (car b)) bindings)) (body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                        {
                            let [params] = [
                                // (map (lambda (b) (car b)) bindings)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let b = args[0].clone();
                                            // (letrec () (car b))
                                            {
                                                // (car b)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[b.clone()])
                                            }
                                        })
                                    },
                                    bindings.clone(),
                                ]),
                            ];
                            // (let* ((body-env (adjoin-boxed-env params env)) (args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                            {
                                let [body_minus_env] = [
                                    // (adjoin-boxed-env params env)
                                    imports::adjoin_minus_boxed_minus_env
                                        .with(|value| value.get())
                                        .invoke(&[params.clone(), env.clone()]),
                                ];
                                // (let* ((args (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings))) (make-scope params (sexpr->sequence body body-env tail?) args))
                                {
                                    let [args_] = [
                                        // (map (lambda (b) (sexpr->ast (cadr b) body-env #f)) bindings)
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                let body_minus_env = body_minus_env.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let b = args[0].clone();
                                                    // (letrec () (sexpr->ast (cadr b) body-env #f))
                                                    {
                                                        // (sexpr->ast (cadr b) body-env #f)
                                                        globals::sexpr_minus__g_ast
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (cadr b)
                                                                imports::cadr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[b.clone()]),
                                                                body_minus_env.clone(),
                                                                Scm::False,
                                                            ])
                                                    }
                                                })
                                            },
                                            bindings.clone(),
                                        ]),
                                    ];
                                    // (let* () (make-scope params (sexpr->sequence body body-env tail?) args))

                                    // (make-scope params (sexpr->sequence body body-env tail?) args)
                                    imports::make_minus_scope
                                        .with(|value| value.get())
                                        .invoke(&[
                                            params.clone(),
                                            // (sexpr->sequence body body-env tail?)
                                            globals::sexpr_minus__g_sequence
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    body.clone(),
                                                    body_minus_env.clone(),
                                                    tail_p.clone(),
                                                ]),
                                            args_.clone(),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->scope-let bindings body env tail?) (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings))) (sexpr->fixlet param* body arg* env tail?)))
        globals::sexpr_minus__g_scope_minus_let.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 4 {
                        panic!("invalid arity")
                    }
                    let bindings = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    let tail_p = args[3].clone();
                    // (letrec () (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings))) (sexpr->fixlet param* body arg* env tail?)))
                    {
                        // (let* ((param* (map (lambda (b) (car b)) bindings)) (arg* (map (lambda (b) (cadr b)) bindings))) (sexpr->fixlet param* body arg* env tail?))
                        {
                            let [param_star_] = [
                                // (map (lambda (b) (car b)) bindings)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let b = args[0].clone();
                                            // (letrec () (car b))
                                            {
                                                // (car b)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[b.clone()])
                                            }
                                        })
                                    },
                                    bindings.clone(),
                                ]),
                            ];
                            // (let* ((arg* (map (lambda (b) (cadr b)) bindings))) (sexpr->fixlet param* body arg* env tail?))
                            {
                                let [arg_star_] = [
                                    // (map (lambda (b) (cadr b)) bindings)
                                    imports::map.with(|value| value.get()).invoke(&[
                                        {
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let b = args[0].clone();
                                                // (letrec () (cadr b))
                                                {
                                                    // (cadr b)
                                                    imports::cadr
                                                        .with(|value| value.get())
                                                        .invoke(&[b.clone()])
                                                }
                                            })
                                        },
                                        bindings.clone(),
                                    ]),
                                ];
                                // (let* () (sexpr->fixlet param* body arg* env tail?))

                                // (sexpr->fixlet param* body arg* env tail?)
                                globals::sexpr_minus__g_fixlet
                                    .with(|value| value.get())
                                    .invoke(&[
                                        param_star_.clone(),
                                        body.clone(),
                                        arg_star_.clone(),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->abstraction param* body env) (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t)) (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t)))))
        globals::sexpr_minus__g_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let param_star_ = args[0].clone();
                    let body = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t)) (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t)))))
                    {
                        // (let ((local-env (adjoin-local-env param* env)) (body (scan-out-defines body))) (if (dotted-list? param*) (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t)) (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))))
                        {
                            let [local_minus_env, body] = [
                                // (adjoin-local-env param* env)
                                imports::adjoin_minus_local_minus_env
                                    .with(|value| value.get())
                                    .invoke(&[param_star_.clone(), env.clone()]),
                                // (scan-out-defines body)
                                globals::scan_minus_out_minus_defines
                                    .with(|value| value.get())
                                    .invoke(&[body.clone()]),
                            ];
                            if (
                                // (dotted-list? param*)
                                imports::dotted_minus_list_p
                                    .with(|value| value.get())
                                    .invoke(&[param_star_.clone()])
                            )
                            .is_true()
                            {
                                // (make-vararg-abstraction (proper-list-part param*) (last-cdr param*) (map (lambda (p) (lookup p local-env)) (proper-list-part param*)) (lookup (last-cdr param*) local-env) (sexpr->sequence body local-env #t))
                                imports::make_minus_vararg_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (proper-list-part param*)
                                        imports::proper_minus_list_minus_part
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (last-cdr param*)
                                        imports::last_minus_cdr
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (map (lambda (p) (lookup p local-env)) (proper-list-part param*))
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                let local_minus_env = local_minus_env.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let p = args[0].clone();
                                                    // (letrec () (lookup p local-env))
                                                    {
                                                        // (lookup p local-env)
                                                        imports::lookup
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                p.clone(),
                                                                local_minus_env.clone(),
                                                            ])
                                                    }
                                                })
                                            },
                                            // (proper-list-part param*)
                                            imports::proper_minus_list_minus_part
                                                .with(|value| value.get())
                                                .invoke(&[param_star_.clone()]),
                                        ]),
                                        // (lookup (last-cdr param*) local-env)
                                        imports::lookup.with(|value| value.get()).invoke(&[
                                            // (last-cdr param*)
                                            imports::last_minus_cdr
                                                .with(|value| value.get())
                                                .invoke(&[param_star_.clone()]),
                                            local_minus_env.clone(),
                                        ]),
                                        // (sexpr->sequence body local-env #t)
                                        globals::sexpr_minus__g_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                body.clone(),
                                                local_minus_env.clone(),
                                                Scm::True,
                                            ]),
                                    ])
                            } else {
                                // (make-abstraction param* (map (lambda (p) (lookup p local-env)) param*) (sexpr->sequence body local-env #t))
                                imports::make_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        param_star_.clone(),
                                        // (map (lambda (p) (lookup p local-env)) param*)
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                let local_minus_env = local_minus_env.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let p = args[0].clone();
                                                    // (letrec () (lookup p local-env))
                                                    {
                                                        // (lookup p local-env)
                                                        imports::lookup
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                p.clone(),
                                                                local_minus_env.clone(),
                                                            ])
                                                    }
                                                })
                                            },
                                            param_star_.clone(),
                                        ]),
                                        // (sexpr->sequence body local-env #t)
                                        globals::sexpr_minus__g_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                body.clone(),
                                                local_minus_env.clone(),
                                                Scm::True,
                                            ]),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->sequence expr* env tail?) (if (null? expr*) (error "empty sequence")) (if (null? (cdr expr*)) (sexpr->ast (car expr*) env tail?) (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))))
        globals::sexpr_minus__g_sequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let expr_star_ = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (if (null? expr*) (error "empty sequence")) (if (null? (cdr expr*)) (sexpr->ast (car expr*) env tail?) (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))))
                    {
                        {
                            if (
                                // (null? expr*)
                                imports::null_p
                                    .with(|value| value.get())
                                    .invoke(&[expr_star_.clone()])
                            )
                            .is_true()
                            {
                                // (error "empty sequence")
                                imports::error
                                    .with(|value| value.get())
                                    .invoke(&[Scm::from("empty sequence")])
                            } else {
                                Scm::symbol("*UNSPECIFIED*")
                            };
                            if (
                                // (null? (cdr expr*))
                                imports::null_p.with(|value| value.get()).invoke(&[
                                    // (cdr expr*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[expr_star_.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (sexpr->ast (car expr*) env tail?)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car expr*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[expr_star_.clone()]),
                                        env.clone(),
                                        tail_p.clone(),
                                    ])
                            } else {
                                // (let ((first (sexpr->ast (car expr*) env #f))) (make-sequence first (sexpr->sequence (cdr expr*) env tail?)))
                                {
                                    let [first] = [
                                        // (sexpr->ast (car expr*) env #f)
                                        globals::sexpr_minus__g_ast
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car expr*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[expr_star_.clone()]),
                                                env.clone(),
                                                Scm::False,
                                            ]),
                                    ];
                                    // (make-sequence first (sexpr->sequence (cdr expr*) env tail?))
                                    imports::make_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[
                                            first.clone(),
                                            // (sexpr->sequence (cdr expr*) env tail?)
                                            globals::sexpr_minus__g_sequence
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdr expr*)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[expr_star_.clone()]),
                                                    env.clone(),
                                                    tail_p.clone(),
                                                ]),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (sexpr->cond clauses env tail?) (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))))
        globals::sexpr_minus__g_cond.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let clauses = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))))
                    {
                        // (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))))
                        if (
                            // (null? clauses)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[clauses.clone()])
                        )
                        .is_true()
                        {
                            // (make-constant (quote *UNSPECIFIED*))
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("*UNSPECIFIED*")])
                        } else if (
                            // (eq? (quote else) (cond-clause-condition (car clauses)))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("else"),
                                // (cond-clause-condition (car clauses))
                                globals::cond_minus_clause_minus_condition
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car clauses)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[clauses.clone()]),
                                    ]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
                            globals::sexpr_minus__g_sequence
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cond-clause-sequence (car clauses))
                                    globals::cond_minus_clause_minus_sequence
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car clauses)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[clauses.clone()]),
                                        ]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else if (
                            // (pair? clauses)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[clauses.clone()])
                        )
                        .is_true()
                        {
                            // (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                            {
                                let [condition] = [
                                    // (sexpr->ast (cond-clause-condition (car clauses)) env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (cond-clause-condition (car clauses))
                                            globals::cond_minus_clause_minus_condition
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (car clauses)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[clauses.clone()]),
                                                ]),
                                            env.clone(),
                                            Scm::False,
                                        ]),
                                ];
                                // (let* ((sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                                {
                                    let [sequence] = [
                                        // (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
                                        globals::sexpr_minus__g_sequence
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cond-clause-sequence (car clauses))
                                                globals::cond_minus_clause_minus_sequence
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car clauses)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[clauses.clone()]),
                                                    ]),
                                                env.clone(),
                                                tail_p.clone(),
                                            ]),
                                    ];
                                    // (let* ((rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
                                    {
                                        let [rest] = [
                                            // (sexpr->cond (cdr clauses) env tail?)
                                            globals::sexpr_minus__g_cond
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdr clauses)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[clauses.clone()]),
                                                    env.clone(),
                                                    tail_p.clone(),
                                                ]),
                                        ];
                                        // (let* () (make-alternative condition sequence rest))

                                        // (make-alternative condition sequence rest)
                                        imports::make_minus_alternative
                                            .with(|value| value.get())
                                            .invoke(&[
                                                condition.clone(),
                                                sequence.clone(),
                                                rest.clone(),
                                            ])
                                    }
                                }
                            }
                        } else {
                            Scm::symbol("*UNSPECIFIED*")
                        }
                    }
                })
            })
        });
        // (define (sexpr->and args env tail?) (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f)))))
        globals::sexpr_minus__g_and.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let args_ = args[0].clone();
                    let env = args[1].clone();
                    let tail_p = args[2].clone();
                    // (letrec () (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f)))))
                    {
                        // (cond ((null? args) (make-constant #t)) ((null? (cdr args)) (sexpr->ast (car args) env tail?)) (else (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f))))
                        if (
                            // (null? args)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[args_.clone()])
                        )
                        .is_true()
                        {
                            // (make-constant #t)
                            imports::make_minus_constant
                                .with(|value| value.get())
                                .invoke(&[Scm::True])
                        } else if (
                            // (null? (cdr args))
                            imports::null_p.with(|value| value.get()).invoke(&[
                                // (cdr args)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[args_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->ast (car args) env tail?)
                            globals::sexpr_minus__g_ast
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car args)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[args_.clone()]),
                                    env.clone(),
                                    tail_p.clone(),
                                ])
                        } else {
                            // (make-alternative (sexpr->ast (car args) env #f) (sexpr->and (cdr args) env tail?) (make-constant #f))
                            imports::make_minus_alternative
                                .with(|value| value.get())
                                .invoke(&[
                                    // (sexpr->ast (car args) env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car args)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                            env.clone(),
                                            Scm::False,
                                        ]),
                                    // (sexpr->and (cdr args) env tail?)
                                    globals::sexpr_minus__g_and
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (cdr args)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[args_.clone()]),
                                            env.clone(),
                                            tail_p.clone(),
                                        ]),
                                    // (make-constant #f)
                                    imports::make_minus_constant
                                        .with(|value| value.get())
                                        .invoke(&[Scm::False]),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->import stmt* env) (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
        globals::sexpr_minus__g_import.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let stmt_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
                    {
                        // (cond ((null? stmt*) (quote ())) ((equal? (quote (sunny testing)) (car stmt*)) (sexpr->import (cdr stmt*) env)) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))))
                        if (
                            // (null? stmt*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[stmt_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else if (
                            // (equal? (quote (sunny testing)) (car stmt*))
                            imports::equal_p.with(|value| value.get()).invoke(&[
                                Scm::pair(
                                    Scm::symbol("sunny"),
                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                ),
                                // (car stmt*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[stmt_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (sexpr->import (cdr stmt*) env)
                            globals::sexpr_minus__g_import
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr stmt*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[stmt_star_.clone()]),
                                    env.clone(),
                                ])
                        } else if (
                            // (eq? (quote only) (caar stmt*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("only"),
                                // (caar stmt*)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[stmt_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (import-only (cadar stmt*) (cddar stmt*) env)
                                globals::import_minus_only
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cadar stmt*)
                                        imports::cadar
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        // (cddar stmt*)
                                        imports::cddar
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                                // (sexpr->import (cdr stmt*) env)
                                globals::sexpr_minus__g_import
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr stmt*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        } else {
                            // (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (import-all (car stmt*) env)
                                globals::import_minus_all
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car stmt*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                                // (sexpr->import (cdr stmt*) env)
                                globals::sexpr_minus__g_import
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr stmt*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->export export-spec* env) (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env)))))
        globals::sexpr_minus__g_export.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let export_minus_spec_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env)))))
                    {
                        // (cond ((null? export-spec*) (quote ())) (else (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))))
                        if (
                            // (null? export-spec*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[export_minus_spec_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else {
                            // (cons (make-export env (car export-spec*) (car export-spec*)) (sexpr->export (cdr export-spec*) env))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (make-export env (car export-spec*) (car export-spec*))
                                imports::make_minus_export
                                    .with(|value| value.get())
                                    .invoke(&[
                                        env.clone(),
                                        // (car export-spec*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                        // (car export-spec*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                    ]),
                                // (sexpr->export (cdr export-spec*) env)
                                globals::sexpr_minus__g_export
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr export-spec*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[export_minus_spec_star_.clone()]),
                                        env.clone(),
                                    ]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (import-all lib env) (adjoin-import*! (library-exports (library-decls (get-lib lib))) env) (make-import lib))
        globals::import_minus_all.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (adjoin-import*! (library-exports (library-decls (get-lib lib))) env) (make-import lib))
                    {
                        {
                            // (adjoin-import*! (library-exports (library-decls (get-lib lib))) env)
                            imports::adjoin_minus_import_star__i
                                .with(|value| value.get())
                                .invoke(&[
                                    // (library-exports (library-decls (get-lib lib)))
                                    globals::library_minus_exports
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (library-decls (get-lib lib))
                                            globals::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (get-lib lib)
                                                    globals::get_minus_lib
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ]),
                                        ]),
                                    env.clone(),
                                ]);
                            // (make-import lib)
                            imports::make_minus_import
                                .with(|value| value.get())
                                .invoke(&[lib.clone()])
                        }
                    }
                })
            })
        });
        // (define (import-only lib names env) (check-imports names (library-exports (library-decls (get-lib lib))) lib) (adjoin-import*! names env) (make-import-only lib names))
        globals::import_minus_only.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    let names = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (check-imports names (library-exports (library-decls (get-lib lib))) lib) (adjoin-import*! names env) (make-import-only lib names))
                    {
                        {
                            // (check-imports names (library-exports (library-decls (get-lib lib))) lib)
                            globals::check_minus_imports
                                .with(|value| value.get())
                                .invoke(&[
                                    names.clone(),
                                    // (library-exports (library-decls (get-lib lib)))
                                    globals::library_minus_exports
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (library-decls (get-lib lib))
                                            globals::library_minus_decls
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (get-lib lib)
                                                    globals::get_minus_lib
                                                        .with(|value| value.get())
                                                        .invoke(&[lib.clone()]),
                                                ]),
                                        ]),
                                    lib.clone(),
                                ]);
                            // (adjoin-import*! names env)
                            imports::adjoin_minus_import_star__i
                                .with(|value| value.get())
                                .invoke(&[names.clone(), env.clone()]);
                            // (make-import-only lib names)
                            imports::make_minus_import_minus_only
                                .with(|value| value.get())
                                .invoke(&[lib.clone(), names.clone()])
                        }
                    }
                })
            })
        });
        // (define (get-lib lib) (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib))))
        globals::get_minus_lib.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    // (letrec () (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib))))
                    {
                        // (let ((full-path (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib)))
                        {
                            let [full_minus_path] = [
                                // (find-library (quote ("." "./lib" "./scheme/lib" "scm-libs" "../scheme/lib" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx")))
                                globals::find_minus_library
                                    .with(|value| value.get())
                                    .invoke(&[
                                        Scm::pair(
                                            Scm::from("."),
                                            Scm::pair(
                                                Scm::from("./lib"),
                                                Scm::pair(
                                                    Scm::from("./scheme/lib"),
                                                    Scm::pair(
                                                        Scm::from("scm-libs"),
                                                        Scm::pair(
                                                            Scm::from("../scheme/lib"),
                                                            Scm::pair(
                                                                Scm::from("../scm-libs"),
                                                                Scm::pair(
                                                                    Scm::from("../../scm-libs"),
                                                                    Scm::Nil,
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                ),
                                            ),
                                        ),
                                        // (library-path lib)
                                        globals::library_minus_path
                                            .with(|value| value.get())
                                            .invoke(&[lib.clone()]),
                                        Scm::pair(
                                            Scm::from(".sld"),
                                            Scm::pair(Scm::from(".slx"), Scm::Nil),
                                        ),
                                    ]),
                            ];
                            if (full_minus_path.clone()).is_true() {
                                // (read (open-input-file full-path))
                                imports::read.with(|value| value.get()).invoke(&[
                                    // (open-input-file full-path)
                                    imports::open_minus_input_minus_file
                                        .with(|value| value.get())
                                        .invoke(&[full_minus_path.clone()]),
                                ])
                            } else {
                                // (error "Unknown library" lib)
                                imports::error
                                    .with(|value| value.get())
                                    .invoke(&[Scm::from("Unknown library"), lib.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (find-library base-path* relative-path extension*) (if (null? base-path*) #f (let* ((path (string-append (car base-path*) relative-path)) (full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))))
        globals::find_minus_library.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let base_minus_path_star_ = args[0].clone();
                    let relative_minus_path = args[1].clone();
                    let extension_star_ = args[2].clone();
                    // (letrec () (if (null? base-path*) #f (let* ((path (string-append (car base-path*) relative-path)) (full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))))
                    {
                        if (
                            // (null? base-path*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[base_minus_path_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::False
                        } else {
                            // (let* ((path (string-append (car base-path*) relative-path)) (full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))
                            {
                                let [path] = [
                                    // (string-append (car base-path*) relative-path)
                                    imports::string_minus_append
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car base-path*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[base_minus_path_star_.clone()]),
                                            relative_minus_path.clone(),
                                        ]),
                                ];
                                // (let* ((full-path (find-library-ext path extension*))) (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))
                                {
                                    let [full_minus_path] = [
                                        // (find-library-ext path extension*)
                                        globals::find_minus_library_minus_ext
                                            .with(|value| value.get())
                                            .invoke(&[path.clone(), extension_star_.clone()]),
                                    ];
                                    // (let* () (if full-path full-path (find-library (cdr base-path*) relative-path extension*)))
                                    if (full_minus_path.clone()).is_true() {
                                        full_minus_path.clone()
                                    } else {
                                        // (find-library (cdr base-path*) relative-path extension*)
                                        globals::find_minus_library
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (cdr base-path*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[base_minus_path_star_.clone()]),
                                                relative_minus_path.clone(),
                                                extension_star_.clone(),
                                            ])
                                    }
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (find-library-ext path extension*) (if (null? extension*) #f (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))))
        globals::find_minus_library_minus_ext.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let path = args[0].clone();
                    let extension_star_ = args[1].clone();
                    // (letrec () (if (null? extension*) #f (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))))
                    {
                        if (
                            // (null? extension*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[extension_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::False
                        } else {
                            // (let ((full-path (string-append path (car extension*)))) (if (file-exists? full-path) full-path (find-library-ext path (cdr extension*))))
                            {
                                let [full_minus_path] = [
                                    // (string-append path (car extension*))
                                    imports::string_minus_append
                                        .with(|value| value.get())
                                        .invoke(&[
                                            path.clone(),
                                            // (car extension*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[extension_star_.clone()]),
                                        ]),
                                ];
                                if (
                                    // (file-exists? full-path)
                                    imports::file_minus_exists_p
                                        .with(|value| value.get())
                                        .invoke(&[full_minus_path.clone()])
                                )
                                .is_true()
                                {
                                    full_minus_path.clone()
                                } else {
                                    // (find-library-ext path (cdr extension*))
                                    globals::find_minus_library_minus_ext
                                        .with(|value| value.get())
                                        .invoke(&[
                                            path.clone(),
                                            // (cdr extension*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[extension_star_.clone()]),
                                        ])
                                }
                            }
                        }
                    }
                })
            })
        });
        // (define (library-path lib) (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib)))
        globals::library_minus_path.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib = args[0].clone();
                    // (letrec () (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib)))
                    {
                        // (reduce (lambda (left right) (string-append left (string-append "/" right))) "" (map symbol->string lib))
                        globals::reduce.with(|value| value.get()).invoke(&[
                            {
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 2 {
                                        panic!("invalid arity")
                                    }
                                    let left = args[0].clone();
                                    let right = args[1].clone();
                                    // (letrec () (string-append left (string-append "/" right)))
                                    {
                                        // (string-append left (string-append "/" right))
                                        imports::string_minus_append
                                            .with(|value| value.get())
                                            .invoke(&[
                                                left.clone(),
                                                // (string-append "/" right)
                                                imports::string_minus_append
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::from("/"), right.clone()]),
                                            ])
                                    }
                                })
                            },
                            Scm::from(""),
                            // (map symbol->string lib)
                            imports::map.with(|value| value.get()).invoke(&[
                                imports::symbol_minus__g_string.with(|value| value.get()),
                                lib.clone(),
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (check-imports imports exports lib) (if (null? imports) #t (if (memq (car imports) exports) (check-imports (cdr imports) exports lib) (error "Invalid import" (car imports) lib))))
        globals::check_minus_imports.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let imports = args[0].clone();
                    let exports = args[1].clone();
                    let lib = args[2].clone();
                    // (letrec () (if (null? imports) #t (if (memq (car imports) exports) (check-imports (cdr imports) exports lib) (error "Invalid import" (car imports) lib))))
                    {
                        if (
                            // (null? imports)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[imports.clone()])
                        )
                        .is_true()
                        {
                            Scm::True
                        } else if (
                            // (memq (car imports) exports)
                            imports::memq.with(|value| value.get()).invoke(&[
                                // (car imports)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[imports.clone()]),
                                exports.clone(),
                            ])
                        )
                        .is_true()
                        {
                            // (check-imports (cdr imports) exports lib)
                            globals::check_minus_imports
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr imports)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[imports.clone()]),
                                    exports.clone(),
                                    lib.clone(),
                                ])
                        } else {
                            // (error "Invalid import" (car imports) lib)
                            imports::error.with(|value| value.get()).invoke(&[
                                Scm::from("Invalid import"),
                                // (car imports)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[imports.clone()]),
                                lib.clone(),
                            ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->testsuite name cases env) (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases)))
        globals::sexpr_minus__g_testsuite.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let cases = args[1].clone();
                    let env = args[2].clone();
                    // (letrec () (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases)))
                    {
                        // (make-testsuite name (map (lambda (case) (sexpr->testcase case env)) cases))
                        imports::make_minus_testsuite
                            .with(|value| value.get())
                            .invoke(&[
                                name.clone(),
                                // (map (lambda (case) (sexpr->testcase case env)) cases)
                                imports::map.with(|value| value.get()).invoke(&[
                                    {
                                        let env = env.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let case = args[0].clone();
                                            // (letrec () (sexpr->testcase case env))
                                            {
                                                // (sexpr->testcase case env)
                                                globals::sexpr_minus__g_testcase
                                                    .with(|value| value.get())
                                                    .invoke(&[case.clone(), env.clone()])
                                            }
                                        })
                                    },
                                    cases.clone(),
                                ]),
                            ])
                    }
                })
            })
        });
        // (define (sexpr->testcase case env) (define (given stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)) (define (when stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt))) (define (then stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))) (define (dispatch section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f))))
        globals::sexpr_minus__g_testcase.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let case = args[0].clone();
                    let env = args[1].clone();
                    // (letrec ((given (lambda (stmt body) (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))) (when (lambda (stmt body) (define (loop stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))) (loop (cdr stmt)))) (then (lambda (stmt body) (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))) (dispatch (lambda (section* body) (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))))) (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f))))
                    {
                        let given = Scm::uninitialized().into_boxed();
                        let when = Scm::uninitialized().into_boxed();
                        let then = Scm::uninitialized().into_boxed();
                        let dispatch = Scm::uninitialized().into_boxed();
                        given.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body))
                                {
                                    // (list (quote let*) (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt)) body)
                                    imports::list.with(|value| value.get()).invoke(&[
                                        Scm::symbol("let*"),
                                        // (map (lambda (assignment) (list (car assignment) (caddr assignment))) (cdr stmt))
                                        imports::map.with(|value| value.get()).invoke(&[
                                            {
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let assignment = args[0].clone();
                                                    // (letrec () (list (car assignment) (caddr assignment)))
                                                    {
                                                        // (list (car assignment) (caddr assignment))
                                                        imports::list
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (car assignment)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[assignment.clone()]),
                                                                // (caddr assignment)
                                                                imports::caddr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[assignment.clone()]),
                                                            ])
                                                    }
                                                })
                                            },
                                            // (cdr stmt)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[stmt.clone()]),
                                        ]),
                                        body.clone(),
                                    ])
                                }
                            })
                        });
                        when.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec ((loop (lambda (stmt*) (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))))) (loop (cdr stmt)))
                                {
                                    let loop_ = Scm::uninitialized().into_boxed();
                                    loop_.set({
                                        let body = body.clone();
                                        let loop_ = loop_.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let stmt_star_ = args[0].clone();
                                            // (letrec () (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*))))))
                                            {
                                                // (cond ((null? stmt*) body) ((eq? (quote <-) (cadar stmt*)) (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))) (else (list (quote begin) (car stmt*) (loop (cdr stmt*)))))
                                                if (
                                                    // (null? stmt*)
                                                    imports::null_p
                                                        .with(|value| value.get())
                                                        .invoke(&[stmt_star_.clone()])
                                                )
                                                .is_true()
                                                {
                                                    body.clone()
                                                } else if (
                                                    // (eq? (quote <-) (cadar stmt*))
                                                    imports::eq_p.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("<-"),
                                                            // (cadar stmt*)
                                                            imports::cadar
                                                                .with(|value| value.get())
                                                                .invoke(&[stmt_star_.clone()]),
                                                        ],
                                                    )
                                                )
                                                .is_true()
                                                {
                                                    // (list (quote let) (list (list (caar stmt*) (caddar stmt*))) (loop (cdr stmt*)))
                                                    imports::list.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("let"),
                                                            // (list (list (caar stmt*) (caddar stmt*)))
                                                            imports::list
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (list (caar stmt*) (caddar stmt*))
                                                                    imports::list
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            // (caar stmt*)
                                                                            imports::caar
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    stmt_star_
                                                                                        .clone(),
                                                                                ]),
                                                                            // (caddar stmt*)
                                                                            imports::caddar
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    stmt_star_
                                                                                        .clone(),
                                                                                ]),
                                                                        ]),
                                                                ]),
                                                            // (loop (cdr stmt*))
                                                            loop_.get().invoke(&[
                                                                // (cdr stmt*)
                                                                imports::cdr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[stmt_star_.clone()]),
                                                            ]),
                                                        ],
                                                    )
                                                } else {
                                                    // (list (quote begin) (car stmt*) (loop (cdr stmt*)))
                                                    imports::list.with(|value| value.get()).invoke(
                                                        &[
                                                            Scm::symbol("begin"),
                                                            // (car stmt*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[stmt_star_.clone()]),
                                                            // (loop (cdr stmt*))
                                                            loop_.get().invoke(&[
                                                                // (cdr stmt*)
                                                                imports::cdr
                                                                    .with(|value| value.get())
                                                                    .invoke(&[stmt_star_.clone()]),
                                                            ]),
                                                        ],
                                                    )
                                                }
                                            }
                                        })
                                    });

                                    // (loop (cdr stmt))
                                    loop_.get().invoke(&[
                                        // (cdr stmt)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[stmt.clone()]),
                                    ])
                                }
                            })
                        });
                        then.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let stmt = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)))
                                {
                                    // (cons (quote begin) (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        Scm::symbol("begin"),
                                        // (append (map (lambda (pred) (list (quote assert) pred)) (cdr stmt)) body)
                                        globals::append.with(|value| value.get()).invoke(&[
                                            // (map (lambda (pred) (list (quote assert) pred)) (cdr stmt))
                                            imports::map.with(|value| value.get()).invoke(&[
                                                {
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 1 {
                                                            panic!("invalid arity")
                                                        }
                                                        let pred = args[0].clone();
                                                        // (letrec () (list (quote assert) pred))
                                                        {
                                                            // (list (quote assert) pred)
                                                            imports::list
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    Scm::symbol("assert"),
                                                                    pred.clone(),
                                                                ])
                                                        }
                                                    })
                                                },
                                                // (cdr stmt)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[stmt.clone()]),
                                            ]),
                                            body.clone(),
                                        ]),
                                    ])
                                }
                            })
                        });
                        dispatch.set({
                            let given = given.clone();
                            let dispatch = dispatch.clone();
                            let when = when.clone();
                            let then = then.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let section_star_ = args[0].clone();
                                let body = args[1].clone();
                                // (letrec () (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase"))))
                                {
                                    // (cond ((null? section*) body) ((eq? (quote given) (caar section*)) (given (car section*) (dispatch (cdr section*) body))) ((eq? (quote when) (caar section*)) (when (car section*) (dispatch (cdr section*) body))) ((eq? (quote then) (caar section*)) (then (car section*) (dispatch (cdr section*) body))) (else (error "invalid testcase")))
                                    if (
                                        // (null? section*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[section_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        body.clone()
                                    } else if (
                                        // (eq? (quote given) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("given"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (given (car section*) (dispatch (cdr section*) body))
                                        given.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else if (
                                        // (eq? (quote when) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("when"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (when (car section*) (dispatch (cdr section*) body))
                                        when.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else if (
                                        // (eq? (quote then) (caar section*))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            Scm::symbol("then"),
                                            // (caar section*)
                                            imports::caar
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (then (car section*) (dispatch (cdr section*) body))
                                        then.get().invoke(&[
                                            // (car section*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[section_star_.clone()]),
                                            // (dispatch (cdr section*) body)
                                            dispatch.get().invoke(&[
                                                // (cdr section*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[section_star_.clone()]),
                                                body.clone(),
                                            ]),
                                        ])
                                    } else {
                                        // (error "invalid testcase")
                                        imports::error
                                            .with(|value| value.get())
                                            .invoke(&[Scm::from("invalid testcase")])
                                    }
                                }
                            })
                        });

                        // (let ((body (dispatch (cddr case) (quote ())))) (make-testcase (cadr case) (sexpr->ast body env #f)))
                        {
                            let [body] = [
                                // (dispatch (cddr case) (quote ()))
                                dispatch.get().invoke(&[
                                    // (cddr case)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[case.clone()]),
                                    Scm::Nil,
                                ]),
                            ];
                            // (make-testcase (cadr case) (sexpr->ast body env #f))
                            imports::make_minus_testcase
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr case)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[case.clone()]),
                                    // (sexpr->ast body env #f)
                                    globals::sexpr_minus__g_ast
                                        .with(|value| value.get())
                                        .invoke(&[body.clone(), env.clone(), Scm::False]),
                                ])
                        }
                    }
                })
            })
        });
        // (define (sexpr->assert cond env) (make-assert (sexpr->ast cond env #f)))
        globals::sexpr_minus__g_assert.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let cond = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (make-assert (sexpr->ast cond env #f)))
                    {
                        // (make-assert (sexpr->ast cond env #f))
                        imports::make_minus_assert
                            .with(|value| value.get())
                            .invoke(&[
                                // (sexpr->ast cond env #f)
                                globals::sexpr_minus__g_ast
                                    .with(|value| value.get())
                                    .invoke(&[cond.clone(), env.clone(), Scm::False]),
                            ])
                    }
                })
            })
        });
        // (define (library? exp*) (and (pair? exp*) (eq? (quote define-library) (car exp*))))
        globals::library_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let exp_star_ = args[0].clone();
                    // (letrec () (and (pair? exp*) (eq? (quote define-library) (car exp*))))
                    {
                        // (and (pair? exp*) (eq? (quote define-library) (car exp*)))
                        if (
                            // (pair? exp*)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[exp_star_.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (quote define-library) (car exp*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("define-library"),
                                // (car exp*)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (definition? expr) (and (pair? expr) (eq? (car expr) (quote define))))
        globals::definition_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (and (pair? expr) (eq? (car expr) (quote define))))
                    {
                        // (and (pair? expr) (eq? (car expr) (quote define)))
                        if (
                            // (pair? expr)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (car expr) (quote define))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                                Scm::symbol("define"),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (definition-variable expr) (if (pair? (cadr expr)) (caadr expr) (cadr expr)))
        globals::definition_minus_variable.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cadr expr)) (caadr expr) (cadr expr)))
                    {
                        if (
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (caadr expr)
                            imports::caadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        } else {
                            // (cadr expr)
                            imports::cadr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    }
                })
            })
        });
        // (define (definition-value expr) (if (pair? (cadr expr)) (cons (quote lambda) (cons (cdadr expr) (cddr expr))) (caddr expr)))
        globals::definition_minus_value.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cadr expr)) (cons (quote lambda) (cons (cdadr expr) (cddr expr))) (caddr expr)))
                    {
                        if (
                            // (pair? (cadr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cadr expr)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cons (quote lambda) (cons (cdadr expr) (cddr expr)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                Scm::symbol("lambda"),
                                // (cons (cdadr expr) (cddr expr))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    // (cdadr expr)
                                    imports::cdadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                    // (cddr expr)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ]),
                            ])
                        } else {
                            // (caddr expr)
                            imports::caddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        }
                    }
                })
            })
        });
        // (define (if-condition expr) (cadr expr))
        globals::if_minus_condition.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cadr expr))
                    {
                        // (cadr expr)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (if-consequence expr) (caddr expr))
        globals::if_minus_consequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (caddr expr))
                    {
                        // (caddr expr)
                        imports::caddr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (if-alternative expr) (if (pair? (cdddr expr)) (cadddr expr) (quote (quote *UNSPECIFIED*))))
        globals::if_minus_alternative.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (if (pair? (cdddr expr)) (cadddr expr) (quote (quote *UNSPECIFIED*))))
                    {
                        if (
                            // (pair? (cdddr expr))
                            imports::pair_p.with(|value| value.get()).invoke(&[
                                // (cdddr expr)
                                imports::cdddr
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cadddr expr)
                            imports::cadddr
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        } else {
                            Scm::pair(
                                Scm::symbol("quote"),
                                Scm::pair(Scm::symbol("*UNSPECIFIED*"), Scm::Nil),
                            )
                        }
                    }
                })
            })
        });
        // (define (cond-clauses expr) (cdr expr))
        globals::cond_minus_clauses.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cdr expr))
                    {
                        // (cdr expr)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (cond-clause-condition clause) (car clause))
        globals::cond_minus_clause_minus_condition.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let clause = args[0].clone();
                    // (letrec () (car clause))
                    {
                        // (car clause)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[clause.clone()])
                    }
                })
            })
        });
        // (define (cond-clause-sequence clause) (cdr clause))
        globals::cond_minus_clause_minus_sequence.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let clause = args[0].clone();
                    // (letrec () (cdr clause))
                    {
                        // (cdr clause)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[clause.clone()])
                    }
                })
            })
        });
        // (define (import? expr) (and (pair? expr) (eq? (car expr) (quote import))))
        globals::import_p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (and (pair? expr) (eq? (car expr) (quote import))))
                    {
                        // (and (pair? expr) (eq? (car expr) (quote import)))
                        if (
                            // (pair? expr)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[expr.clone()])
                        )
                        .is_true()
                        {
                            // (eq? (car expr) (quote import))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                                Scm::symbol("import"),
                            ])
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (import-libnames exp*) (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*))))
        globals::import_minus_libnames.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let exp_star_ = args[0].clone();
                    // (letrec () (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*))))
                    {
                        // (filter (lambda (libname) (not (equal? libname (quote (sunny testing))))) (map importset-libname (cdr exp*)))
                        imports::filter.with(|value| value.get()).invoke(&[
                            {
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let libname = args[0].clone();
                                    // (letrec () (not (equal? libname (quote (sunny testing)))))
                                    {
                                        // (not (equal? libname (quote (sunny testing))))
                                        imports::not.with(|value| value.get()).invoke(&[
                                            // (equal? libname (quote (sunny testing)))
                                            imports::equal_p.with(|value| value.get()).invoke(&[
                                                libname.clone(),
                                                Scm::pair(
                                                    Scm::symbol("sunny"),
                                                    Scm::pair(Scm::symbol("testing"), Scm::Nil),
                                                ),
                                            ]),
                                        ])
                                    }
                                })
                            },
                            // (map importset-libname (cdr exp*))
                            imports::map.with(|value| value.get()).invoke(&[
                                globals::importset_minus_libname.with(|value| value.get()),
                                // (cdr exp*)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[exp_star_.clone()]),
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (importset-libname expr) (cond ((eq? (quote only) (car expr)) (importset-libname (cadr expr))) ((eq? (quote except) (car expr)) (importset-libname (cadr expr))) (else expr)))
        globals::importset_minus_libname.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cond ((eq? (quote only) (car expr)) (importset-libname (cadr expr))) ((eq? (quote except) (car expr)) (importset-libname (cadr expr))) (else expr)))
                    {
                        // (cond ((eq? (quote only) (car expr)) (importset-libname (cadr expr))) ((eq? (quote except) (car expr)) (importset-libname (cadr expr))) (else expr))
                        if (
                            // (eq? (quote only) (car expr))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("only"),
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (importset-libname (cadr expr))
                            globals::importset_minus_libname
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ])
                        } else if (
                            // (eq? (quote except) (car expr))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("except"),
                                // (car expr)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (importset-libname (cadr expr))
                            globals::importset_minus_libname
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cadr expr)
                                    imports::cadr
                                        .with(|value| value.get())
                                        .invoke(&[expr.clone()]),
                                ])
                        } else {
                            expr.clone()
                        }
                    }
                })
            })
        });
        // (define (library-name expr) (cadr expr))
        globals::library_minus_name.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cadr expr))
                    {
                        // (cadr expr)
                        imports::cadr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (library-decls expr) (cddr expr))
        globals::library_minus_decls.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let expr = args[0].clone();
                    // (letrec () (cddr expr))
                    {
                        // (cddr expr)
                        imports::cddr
                            .with(|value| value.get())
                            .invoke(&[expr.clone()])
                    }
                })
            })
        });
        // (define (library-exports lib-decl*) (cond ((null? lib-decl*) (quote ())) ((eq? (quote export) (caar lib-decl*)) (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))) (else (library-exports (cdr lib-decl*)))))
        globals::library_minus_exports.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let lib_minus_decl_star_ = args[0].clone();
                    // (letrec () (cond ((null? lib-decl*) (quote ())) ((eq? (quote export) (caar lib-decl*)) (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))) (else (library-exports (cdr lib-decl*)))))
                    {
                        // (cond ((null? lib-decl*) (quote ())) ((eq? (quote export) (caar lib-decl*)) (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))) (else (library-exports (cdr lib-decl*))))
                        if (
                            // (null? lib-decl*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[lib_minus_decl_star_.clone()])
                        )
                        .is_true()
                        {
                            Scm::Nil
                        } else if (
                            // (eq? (quote export) (caar lib-decl*))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("export"),
                                // (caar lib-decl*)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[lib_minus_decl_star_.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (append (cdar lib-decl*) (library-exports (cdr lib-decl*)))
                            globals::append.with(|value| value.get()).invoke(&[
                                // (cdar lib-decl*)
                                imports::cdar
                                    .with(|value| value.get())
                                    .invoke(&[lib_minus_decl_star_.clone()]),
                                // (library-exports (cdr lib-decl*))
                                globals::library_minus_exports
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr lib-decl*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[lib_minus_decl_star_.clone()]),
                                    ]),
                            ])
                        } else {
                            // (library-exports (cdr lib-decl*))
                            globals::library_minus_exports
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr lib-decl*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[lib_minus_decl_star_.clone()]),
                                ])
                        }
                    }
                })
            })
        });
        // (define (scan-out-defines body) (define (initializations exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*))))) (define (transform exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))) (list (cons (quote letrec) (cons (initializations body) (transform body)))))
        globals::scan_minus_out_minus_defines.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let body = args[0].clone();
                    // (letrec ((initializations (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))) (transform (lambda (exp*) (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))))) (list (cons (quote letrec) (cons (initializations body) (transform body)))))
                    {
                        let initializations = Scm::uninitialized().into_boxed();
                        let transform = Scm::uninitialized().into_boxed();
                        initializations.set({
                            let initializations = initializations.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let exp_star_ = args[0].clone();
                                // (letrec () (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*)))))
                                {
                                    // (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))) (else (initializations (cdr exp*))))
                                    if (
                                        // (null? exp*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[exp_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else if (
                                        // (definition? (car exp*))
                                        globals::definition_p.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (list (definition-variable (car exp*)) (definition-value (car exp*)))
                                            imports::list.with(|value| value.get()).invoke(&[
                                                // (definition-variable (car exp*))
                                                globals::definition_minus_variable
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()]),
                                                    ]),
                                                // (definition-value (car exp*))
                                                globals::definition_minus_value
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car exp*)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[exp_star_.clone()]),
                                                    ]),
                                            ]),
                                            // (initializations (cdr exp*))
                                            initializations.get().invoke(&[
                                                // (cdr exp*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                            ]),
                                        ])
                                    } else {
                                        // (initializations (cdr exp*))
                                        initializations.get().invoke(&[
                                            // (cdr exp*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    }
                                }
                            })
                        });
                        transform.set({
                            let transform = transform.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 1 {
                                    panic!("invalid arity")
                                }
                                let exp_star_ = args[0].clone();
                                // (letrec () (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*))))))
                                {
                                    // (cond ((null? exp*) (quote ())) ((definition? (car exp*)) (transform (cdr exp*))) (else (cons (car exp*) (transform (cdr exp*)))))
                                    if (
                                        // (null? exp*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[exp_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else if (
                                        // (definition? (car exp*))
                                        globals::definition_p.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (transform (cdr exp*))
                                        transform.get().invoke(&[
                                            // (cdr exp*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ])
                                    } else {
                                        // (cons (car exp*) (transform (cdr exp*)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                            // (transform (cdr exp*))
                                            transform.get().invoke(&[
                                                // (cdr exp*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[exp_star_.clone()]),
                                            ]),
                                        ])
                                    }
                                }
                            })
                        });

                        // (list (cons (quote letrec) (cons (initializations body) (transform body))))
                        imports::list.with(|value| value.get()).invoke(&[
                            // (cons (quote letrec) (cons (initializations body) (transform body)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                Scm::symbol("letrec"),
                                // (cons (initializations body) (transform body))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    // (initializations body)
                                    initializations.get().invoke(&[body.clone()]),
                                    // (transform body)
                                    transform.get().invoke(&[body.clone()]),
                                ]),
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (boxify node) (define (transform node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))) (node (quote transform) transform))
        globals::boxify.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let node = args[0].clone();
                    // (letrec ((transform (lambda (node ignore) (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))))) (node (quote transform) transform))
                    {
                        let transform = Scm::uninitialized().into_boxed();
                        transform.set({
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let node = args[0].clone();
                                let ignore = args[1].clone();
                                // (letrec () (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore))))
                                {
                                    // (cond ((eq? (node (quote kind)) (quote ABSTRACTION)) (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))) ((eq? (node (quote kind)) (quote VARARG-ABSTRACTION)) (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))) (else (ignore)))
                                    if (
                                        // (eq? (node (quote kind)) (quote ABSTRACTION))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            // (node (quote kind))
                                            node.clone().invoke(&[Scm::symbol("kind")]),
                                            Scm::symbol("ABSTRACTION"),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (boxify-abstraction (node (quote get-params)) (node (quote get-vars)) (node (quote get-params)) (node (quote get-vars)) (node (quote get-body)))
                                        globals::boxify_minus_abstraction
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-body))
                                                node.clone().invoke(&[Scm::symbol("get-body")]),
                                            ])
                                    } else if (
                                        // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            // (node (quote kind))
                                            node.clone().invoke(&[Scm::symbol("kind")]),
                                            Scm::symbol("VARARG-ABSTRACTION"),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))
                                        globals::boxify_minus_vararg_minus_abstraction
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (node (quote get-params))
                                                node.clone().invoke(&[Scm::symbol("get-params")]),
                                                // (node (quote get-vararg))
                                                node.clone().invoke(&[Scm::symbol("get-vararg")]),
                                                // (node (quote get-vars))
                                                node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                // (node (quote get-varvar))
                                                node.clone().invoke(&[Scm::symbol("get-varvar")]),
                                                // (cons (node (quote get-vararg)) (node (quote get-params)))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    // (node (quote get-vararg))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-vararg")]),
                                                    // (node (quote get-params))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-params")]),
                                                ]),
                                                // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    // (node (quote get-varvar))
                                                    node.clone()
                                                        .invoke(&[Scm::symbol("get-varvar")]),
                                                    // (node (quote get-vars))
                                                    node.clone().invoke(&[Scm::symbol("get-vars")]),
                                                ]),
                                                // (node (quote get-body))
                                                node.clone().invoke(&[Scm::symbol("get-body")]),
                                            ])
                                    } else {
                                        // (ignore)
                                        ignore.clone().invoke(&[])
                                    }
                                }
                            })
                        });

                        // (node (quote transform) transform)
                        node.clone()
                            .invoke(&[Scm::symbol("transform"), transform.get()])
                    }
                })
            })
        });
        // (define (boxify-abstraction params vars param* var* body) (if (null? var*) (make-abstraction params vars body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-abstraction params vars (cdr param*) (cdr var*) body))))
        globals::boxify_minus_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 5 {
                        panic!("invalid arity")
                    }
                    let params = args[0].clone();
                    let vars = args[1].clone();
                    let param_star_ = args[2].clone();
                    let var_star_ = args[3].clone();
                    let body = args[4].clone();
                    // (letrec () (if (null? var*) (make-abstraction params vars body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-abstraction params vars (cdr param*) (cdr var*) body))))
                    {
                        if (
                            // (null? var*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[var_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-abstraction params vars body)
                            imports::make_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[params.clone(), vars.clone(), body.clone()])
                        } else if (
                            // (variable-mut? (car var*))
                            imports::variable_minus_mut_p
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car var*)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                ])
                        )
                        .is_true()
                        {
                            {
                                // (variable-set-setter! (car var*) (quote BOXED-SET))
                                imports::variable_minus_set_minus_setter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-SET"),
                                    ]);
                                // (variable-set-getter! (car var*) (quote BOXED-REF))
                                imports::variable_minus_set_minus_getter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-REF"),
                                    ]);
                                // (boxify-abstraction params vars (cdr param*) (cdr var*) (make-boxify (car param*) body))
                                globals::boxify_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        params.clone(),
                                        vars.clone(),
                                        // (cdr param*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (cdr var*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        // (make-boxify (car param*) body)
                                        imports::make_minus_boxify
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car param*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[param_star_.clone()]),
                                                body.clone(),
                                            ]),
                                    ])
                            }
                        } else {
                            // (boxify-abstraction params vars (cdr param*) (cdr var*) body)
                            globals::boxify_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vars.clone(),
                                    // (cdr param*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[param_star_.clone()]),
                                    // (cdr var*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                    body.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (boxify-vararg-abstraction params vararg vars varvar param* var* body) (if (null? var*) (make-vararg-abstraction params vararg vars varvar body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body))))
        globals::boxify_minus_vararg_minus_abstraction.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 7 {
                        panic!("invalid arity")
                    }
                    let params = args[0].clone();
                    let vararg = args[1].clone();
                    let vars = args[2].clone();
                    let varvar = args[3].clone();
                    let param_star_ = args[4].clone();
                    let var_star_ = args[5].clone();
                    let body = args[6].clone();
                    // (letrec () (if (null? var*) (make-vararg-abstraction params vararg vars varvar body) (if (variable-mut? (car var*)) (begin (variable-set-setter! (car var*) (quote BOXED-SET)) (variable-set-getter! (car var*) (quote BOXED-REF)) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))) (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body))))
                    {
                        if (
                            // (null? var*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[var_star_.clone()])
                        )
                        .is_true()
                        {
                            // (make-vararg-abstraction params vararg vars varvar body)
                            imports::make_minus_vararg_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vararg.clone(),
                                    vars.clone(),
                                    varvar.clone(),
                                    body.clone(),
                                ])
                        } else if (
                            // (variable-mut? (car var*))
                            imports::variable_minus_mut_p
                                .with(|value| value.get())
                                .invoke(&[
                                    // (car var*)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                ])
                        )
                        .is_true()
                        {
                            {
                                // (variable-set-setter! (car var*) (quote BOXED-SET))
                                imports::variable_minus_set_minus_setter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-SET"),
                                    ]);
                                // (variable-set-getter! (car var*) (quote BOXED-REF))
                                imports::variable_minus_set_minus_getter_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (car var*)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        Scm::symbol("BOXED-REF"),
                                    ]);
                                // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) (make-boxify (car param*) body))
                                globals::boxify_minus_vararg_minus_abstraction
                                    .with(|value| value.get())
                                    .invoke(&[
                                        params.clone(),
                                        vararg.clone(),
                                        vars.clone(),
                                        varvar.clone(),
                                        // (cdr param*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[param_star_.clone()]),
                                        // (cdr var*)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[var_star_.clone()]),
                                        // (make-boxify (car param*) body)
                                        imports::make_minus_boxify
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (car param*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[param_star_.clone()]),
                                                body.clone(),
                                            ]),
                                    ])
                            }
                        } else {
                            // (boxify-vararg-abstraction params vararg vars varvar (cdr param*) (cdr var*) body)
                            globals::boxify_minus_vararg_minus_abstraction
                                .with(|value| value.get())
                                .invoke(&[
                                    params.clone(),
                                    vararg.clone(),
                                    vars.clone(),
                                    varvar.clone(),
                                    // (cdr param*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[param_star_.clone()]),
                                    // (cdr var*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[var_star_.clone()]),
                                    body.clone(),
                                ])
                        }
                    }
                })
            })
        });
        // (define (reduce f init seq) (if (pair? seq) (reduce f (f init (car seq)) (cdr seq)) init))
        globals::reduce.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let init = args[1].clone();
                    let seq = args[2].clone();
                    // (letrec () (if (pair? seq) (reduce f (f init (car seq)) (cdr seq)) init))
                    {
                        if (
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            // (reduce f (f init (car seq)) (cdr seq))
                            globals::reduce.with(|value| value.get()).invoke(&[
                                f.clone(),
                                // (f init (car seq))
                                f.clone().invoke(&[
                                    init.clone(),
                                    // (car seq)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ]),
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()]),
                            ])
                        } else {
                            init.clone()
                        }
                    }
                })
            })
        });
        // (define (assoc obj seq) (if (pair? seq) (if (equal? obj (caar seq)) (car seq) (assoc obj (cdr seq))) #f))
        globals::assoc.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let obj = args[0].clone();
                    let seq = args[1].clone();
                    // (letrec () (if (pair? seq) (if (equal? obj (caar seq)) (car seq) (assoc obj (cdr seq))) #f))
                    {
                        if (
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            if (
                                // (equal? obj (caar seq))
                                imports::equal_p.with(|value| value.get()).invoke(&[
                                    obj.clone(),
                                    // (caar seq)
                                    imports::caar
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            } else {
                                // (assoc obj (cdr seq))
                                globals::assoc.with(|value| value.get()).invoke(&[
                                    obj.clone(),
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ])
                            }
                        } else {
                            Scm::False
                        }
                    }
                })
            })
        });
        // (define (append seq-a seq-b) (if (pair? seq-a) (cons (car seq-a) (append (cdr seq-a) seq-b)) seq-b))
        globals::append.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let seq_minus_a = args[0].clone();
                    let seq_minus_b = args[1].clone();
                    // (letrec () (if (pair? seq-a) (cons (car seq-a) (append (cdr seq-a) seq-b)) seq-b))
                    {
                        if (
                            // (pair? seq-a)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq_minus_a.clone()])
                        )
                        .is_true()
                        {
                            // (cons (car seq-a) (append (cdr seq-a) seq-b))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // (car seq-a)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq_minus_a.clone()]),
                                // (append (cdr seq-a) seq-b)
                                globals::append.with(|value| value.get()).invoke(&[
                                    // (cdr seq-a)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq_minus_a.clone()]),
                                    seq_minus_b.clone(),
                                ]),
                            ])
                        } else {
                            seq_minus_b.clone()
                        }
                    }
                })
            })
        });
        // (define (sort cmp ass) (if (pair? ass) (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))) (quote ())))
        globals::sort.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let cmp = args[0].clone();
                    let ass = args[1].clone();
                    // (letrec () (if (pair? ass) (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))) (quote ())))
                    {
                        if (
                            // (pair? ass)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[ass.clone()])
                        )
                        .is_true()
                        {
                            // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                            {
                                let [pivot] = [
                                    // (car ass)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[ass.clone()]),
                                ];
                                // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                                globals::append.with(|value| value.get()).invoke(&[
                                    // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                    globals::sort.with(|value| value.get()).invoke(&[
                                        cmp.clone(),
                                        // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                        imports::filter.with(|value| value.get()).invoke(&[
                                            {
                                                let cmp = cmp.clone();
                                                let pivot = pivot.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x = args[0].clone();
                                                    // (letrec () (cmp x pivot))
                                                    {
                                                        // (cmp x pivot)
                                                        cmp.clone()
                                                            .invoke(&[x.clone(), pivot.clone()])
                                                    }
                                                })
                                            },
                                            // (cdr ass)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[ass.clone()]),
                                        ]),
                                    ]),
                                    // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        pivot.clone(),
                                        // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                        globals::sort.with(|value| value.get()).invoke(&[
                                            cmp.clone(),
                                            // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                            imports::filter.with(|value| value.get()).invoke(&[
                                                {
                                                    let cmp = cmp.clone();
                                                    let pivot = pivot.clone();
                                                    Scm::func(move |args: &[Scm]| {
                                                        if args.len() != 1 {
                                                            panic!("invalid arity")
                                                        }
                                                        let x = args[0].clone();
                                                        // (letrec () (not (cmp x pivot)))
                                                        {
                                                            // (not (cmp x pivot))
                                                            imports::not
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (cmp x pivot)
                                                                    cmp.clone().invoke(&[
                                                                        x.clone(),
                                                                        pivot.clone(),
                                                                    ]),
                                                                ])
                                                        }
                                                    })
                                                },
                                                // (cdr ass)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[ass.clone()]),
                                            ]),
                                        ]),
                                    ]),
                                ])
                            }
                        } else {
                            Scm::Nil
                        }
                    }
                })
            })
        })
    };
}
