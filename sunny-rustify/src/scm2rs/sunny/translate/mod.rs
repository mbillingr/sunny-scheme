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
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::module_tree::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::rust_minus_gen_minus_in_minus_module;
    pub use super::globals::scm_minus__g_ast;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_boxed: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-boxed"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_boxed: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-boxed"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_local: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-local"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_local: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-local"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_import_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-import!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_global: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-global"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_global_minus_var_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-global-var!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_globals: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-globals"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_global_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-global!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static new_minus_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL new-import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_in_minus_module: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-in-module"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_in_minus_submodule: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-in-submodule"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-module-tree"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-module-tree-list"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static list_minus_find_minus_free_minus_vars: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL list-find-free-vars"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static transform_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL transform-list"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static print_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL print-list"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-boxify"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_modules: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-modules"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_global_minus_defs: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-global-defs"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static any: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL any"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL importset-libname"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testcase: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testcase"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static reduce: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL reduce"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library_minus_ext: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library-ext"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_path: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-path"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static find_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL find-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static check_minus_imports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL check-imports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_exports: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-exports"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_import_star__i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-import*!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_all: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-all"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_only: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-only"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static cond_minus_clause_minus_condition: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cond-clause-condition"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-vararg-abstraction"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static scan_minus_out_minus_defines: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scan-out-defines"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_boxed_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-boxed-env"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_local_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-local-env"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->args"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_regular_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->regular-application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->fixlet"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-value"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-variable"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static ensure_minus_var_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ensure-var!"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->application"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_assert: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->assert"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_testsuite: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->testsuite"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static lookup: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL lookup"))}
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
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-library"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-program"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sort: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sort"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->sequence"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_minus_libnames: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import-libnames"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static register_minus_libraries: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL register-libraries"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static import_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL import?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_global_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-global-env"))}
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
globals::make_minus_global_minus_env.with(|value| value.get()).invoke(&[]));
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
globals::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),main.clone(),
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
                                globals::make_minus_global_minus_env
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
                            globals::make_minus_library
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
                                    globals::make_minus_sequence
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
                                        globals::lookup
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
                                        globals::lookup
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
                                globals::ensure_minus_var_i
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
                                globals::ensure_minus_var_i
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
                                        globals::ensure_minus_var_i
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
                                globals::adjoin_minus_local_minus_env
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
                                    globals::adjoin_minus_boxed_minus_env
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
                                globals::adjoin_minus_local_minus_env
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
                                globals::make_minus_vararg_minus_abstraction
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
                                                        globals::lookup
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
                                        globals::lookup.with(|value| value.get()).invoke(&[
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
                                globals::make_minus_abstraction
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
                                                        globals::lookup
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
                                    globals::make_minus_sequence
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
                                globals::make_minus_export
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
                            globals::adjoin_minus_import_star__i
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
                            globals::make_minus_import
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
                            globals::adjoin_minus_import_star__i
                                .with(|value| value.get())
                                .invoke(&[names.clone(), env.clone()]);
                            // (make-import-only lib names)
                            globals::make_minus_import_minus_only
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
                        globals::make_minus_testsuite
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
                            globals::make_minus_testcase
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
                        globals::make_minus_assert
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
globals::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
imports::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])}})},globals.clone(),])).is_true() {
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};"),])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs module globals)
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone(),])}}})},]);
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
globals::rust_minus_gen_minus_modules.with(|value| value.get()).invoke(&[module.clone(),libraries.clone(),])}}})});
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
globals::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
imports::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])}})},globals.clone(),])).is_true() {
// (println module "use sunny_core::{Mut, Scm};")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("use sunny_core::{Mut, Scm};"),])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs module globals)
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),globals.clone(),])}}})},]);
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
imports::make_minus_constant.with(|value| value.get()).invoke(&[Scm::symbol("*UNSPECIFIED*"),])}} else {
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
                                                globals::lookup
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
        });
        // (define (print-list seq) (if (pair? seq) (cons ((car seq) (quote repr)) (print-list (cdr seq))) (quote ())))
        globals::print_minus_list.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (letrec () (if (pair? seq) (cons ((car seq) (quote repr)) (print-list (cdr seq))) (quote ())))
                    {
                        if (
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            // (cons ((car seq) (quote repr)) (print-list (cdr seq)))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // ((car seq) (quote repr))

                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                                    .invoke(&[Scm::symbol("repr")]),
                                // (print-list (cdr seq))
                                globals::print_minus_list
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr seq)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                    ]),
                            ])
                        } else {
                            Scm::Nil
                        }
                    }
                })
            })
        });
        // (define (transform-list seq func) (if (pair? seq) (cons ((car seq) (quote transform) func) (transform-list (cdr seq) func)) (quote ())))
        globals::transform_minus_list.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    let func = args[1].clone();
                    // (letrec () (if (pair? seq) (cons ((car seq) (quote transform) func) (transform-list (cdr seq) func)) (quote ())))
                    {
                        if (
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            // (cons ((car seq) (quote transform) func) (transform-list (cdr seq) func))
                            imports::cons.with(|value| value.get()).invoke(&[
                                // ((car seq) (quote transform) func)

                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                                    .invoke(&[Scm::symbol("transform"), func.clone()]),
                                // (transform-list (cdr seq) func)
                                globals::transform_minus_list
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr seq)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                        func.clone(),
                                    ]),
                            ])
                        } else {
                            Scm::Nil
                        }
                    }
                })
            })
        });
        // (define (list-find-free-vars seq local-env) (if (pair? seq) (append ((car seq) (quote free-vars) local-env) (list-find-free-vars (cdr seq) local-env)) (quote ())))
        globals::list_minus_find_minus_free_minus_vars.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    let local_minus_env = args[1].clone();
                    // (letrec () (if (pair? seq) (append ((car seq) (quote free-vars) local-env) (list-find-free-vars (cdr seq) local-env)) (quote ())))
                    {
                        if (
                            // (pair? seq)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            // (append ((car seq) (quote free-vars) local-env) (list-find-free-vars (cdr seq) local-env))
                            globals::append.with(|value| value.get()).invoke(&[
                                // ((car seq) (quote free-vars) local-env)

                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                                    .invoke(&[Scm::symbol("free-vars"), local_minus_env.clone()]),
                                // (list-find-free-vars (cdr seq) local-env)
                                globals::list_minus_find_minus_free_minus_vars
                                    .with(|value| value.get())
                                    .invoke(&[
                                        // (cdr seq)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                        local_minus_env.clone(),
                                    ]),
                            ])
                        } else {
                            Scm::Nil
                        }
                    }
                })
            })
        });
        // (define (rust-gen-global-defs module g) (if (null? g) (println module) (if (global-imported? (cdar g)) (rust-gen-global-defs module (cdr g)) (begin (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}") (rust-gen-global-defs module (cdr g))))))
        globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let g = args[1].clone();
// (letrec () (if (null? g) (println module) (if (global-imported? (cdar g)) (rust-gen-global-defs module (cdr g)) (begin (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}") (rust-gen-global-defs module (cdr g))))))
{if (
// (null? g)
imports::null_p.with(|value| value.get()).invoke(&[g.clone(),])).is_true() {
// (println module)
imports::println.with(|value| value.get()).invoke(&[module.clone(),])} else if (
// (global-imported? (cdar g))
imports::global_minus_imported_p.with(|value| value.get()).invoke(&[
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone(),]),])).is_true() {
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])} else {{
// (println module "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "),
// (rustify-identifier (caar g))
imports::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone(),]),]),Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL "),
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone(),]),Scm::from("\"))}"),]);
// (rust-gen-global-defs module (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[module.clone(),
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(),]),])}}}})}));
        // (define (rust-gen-modules module libs) (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree))))
        globals::rust_minus_gen_minus_modules.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let module = args[0].clone();
                    let libs = args[1].clone();
                    // (letrec () (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree))))
                    {
                        // (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list module (module-tree-children module-tree)))
                        {
                            let [module_minus_tree] = [
                                // (make-module-tree-node (quote root))
                                imports::make_minus_module_minus_tree_minus_node
                                    .with(|value| value.get())
                                    .invoke(&[Scm::symbol("root")]),
                            ];
                            {
                                // (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
                                imports::for_minus_each.with(|value| value.get()).invoke(&[
                                    {
                                        let module_minus_tree = module_minus_tree.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let lib = args[0].clone();
                                            // (letrec () (module-tree-insert! module-tree (car lib) (cdr lib)))
                                            {
                                                // (module-tree-insert! module-tree (car lib) (cdr lib))
                                                imports::module_minus_tree_minus_insert_i
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        module_minus_tree.clone(),
                                                        // (car lib)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                        // (cdr lib)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[lib.clone()]),
                                                    ])
                                            }
                                        })
                                    },
                                    libs.clone(),
                                ]);
                                // (rust-gen-module-tree-list module (module-tree-children module-tree))
                                globals::rust_minus_gen_minus_module_minus_tree_minus_list
                                    .with(|value| value.get())
                                    .invoke(&[
                                        module.clone(),
                                        // (module-tree-children module-tree)
                                        imports::module_minus_tree_minus_children
                                            .with(|value| value.get())
                                            .invoke(&[module_minus_tree.clone()]),
                                    ])
                            }
                        }
                    }
                })
            })
        });
        // (define (rust-gen-module-tree module node) (println module "pub mod " (rustify-libname (module-tree-name node)) ";") (if (module-tree-leaf? node) (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod))) (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) (rust-gen-module-tree-list submod (module-tree-children node))))))
        globals::rust_minus_gen_minus_module_minus_tree.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let module = args[0].clone();let node = args[1].clone();
// (letrec () (println module "pub mod " (rustify-libname (module-tree-name node)) ";") (if (module-tree-leaf? node) (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod))) (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) (rust-gen-module-tree-list submod (module-tree-children node))))))
{{
// (println module "pub mod " (rustify-libname (module-tree-name node)) ";")
imports::println.with(|value| value.get()).invoke(&[module.clone(),Scm::from("pub mod "),
// (rustify-libname (module-tree-name node))
imports::rustify_minus_libname.with(|value| value.get()).invoke(&[
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone(),]),]),Scm::from(";"),]);if (
// (module-tree-leaf? node)
imports::module_minus_tree_minus_leaf_p.with(|value| value.get()).invoke(&[node.clone(),])).is_true() {
// (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) ((module-tree-libobj node) (quote gen-rust) submod)))
globals::rust_minus_gen_minus_in_minus_submodule.with(|value| value.get()).invoke(&[
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone(),]),module.clone(),{let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let submod = args[0].clone();
// (letrec () ((module-tree-libobj node) (quote gen-rust) submod))
{
// ((module-tree-libobj node) (quote gen-rust) submod)

// (module-tree-libobj node)
imports::module_minus_tree_minus_libobj.with(|value| value.get()).invoke(&[node.clone(),]).invoke(&[Scm::symbol("gen-rust"),submod.clone(),])}})},])} else {
// (rust-gen-in-submodule (module-tree-name node) module (lambda (submod) (rust-gen-module-tree-list submod (module-tree-children node))))
globals::rust_minus_gen_minus_in_minus_submodule.with(|value| value.get()).invoke(&[
// (module-tree-name node)
imports::module_minus_tree_minus_name.with(|value| value.get()).invoke(&[node.clone(),]),module.clone(),{let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let submod = args[0].clone();
// (letrec () (rust-gen-module-tree-list submod (module-tree-children node)))
{
// (rust-gen-module-tree-list submod (module-tree-children node))
globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| value.get()).invoke(&[submod.clone(),
// (module-tree-children node)
imports::module_minus_tree_minus_children.with(|value| value.get()).invoke(&[node.clone(),]),])}})},])}}}})}));
        // (define (rust-gen-module-tree-list module nodes) (for-each (lambda (child) (rust-gen-module-tree module child)) nodes))
        globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let module = args[0].clone();
                    let nodes = args[1].clone();
                    // (letrec () (for-each (lambda (child) (rust-gen-module-tree module child)) nodes))
                    {
                        // (for-each (lambda (child) (rust-gen-module-tree module child)) nodes)
                        imports::for_minus_each.with(|value| value.get()).invoke(&[
                            {
                                let module = module.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let child = args[0].clone();
                                    // (letrec () (rust-gen-module-tree module child))
                                    {
                                        // (rust-gen-module-tree module child)
                                        globals::rust_minus_gen_minus_module_minus_tree
                                            .with(|value| value.get())
                                            .invoke(&[module.clone(), child.clone()])
                                    }
                                })
                            },
                            nodes.clone(),
                        ])
                    }
                })
            })
        });
        // (define (rust-gen-in-module name base-path body) (let ((module (open-module name base-path))) (body module) (close-module module)))
        globals::rust_minus_gen_minus_in_minus_module.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let base_minus_path = args[1].clone();
                    let body = args[2].clone();
                    // (letrec () (let ((module (open-module name base-path))) (body module) (close-module module)))
                    {
                        // (let ((module (open-module name base-path))) (body module) (close-module module))
                        {
                            let [module] = [
                                // (open-module name base-path)
                                imports::open_minus_module
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), base_minus_path.clone()]),
                            ];
                            {
                                // (body module)
                                body.clone().invoke(&[module.clone()]);
                                // (close-module module)
                                imports::close_minus_module
                                    .with(|value| value.get())
                                    .invoke(&[module.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (rust-gen-in-submodule name parent body) (let ((module (open-submodule name parent))) (body module) (close-module module)))
        globals::rust_minus_gen_minus_in_minus_submodule.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let parent = args[1].clone();
                    let body = args[2].clone();
                    // (letrec () (let ((module (open-submodule name parent))) (body module) (close-module module)))
                    {
                        // (let ((module (open-submodule name parent))) (body module) (close-module module))
                        {
                            let [module] = [
                                // (open-submodule name parent)
                                imports::open_minus_submodule
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), parent.clone()]),
                            ];
                            {
                                // (body module)
                                body.clone().invoke(&[module.clone()]);
                                // (close-module module)
                                imports::close_minus_module
                                    .with(|value| value.get())
                                    .invoke(&[module.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (make-global-env) (list (quote GLOBAL-MARKER) (new-import (quote assert-eq)) (new-import (quote assert-equal))))
        globals::make_minus_global_minus_env.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (list (quote GLOBAL-MARKER) (new-import (quote assert-eq)) (new-import (quote assert-equal))))
                    {
                        // (list (quote GLOBAL-MARKER) (new-import (quote assert-eq)) (new-import (quote assert-equal)))
                        imports::list.with(|value| value.get()).invoke(&[
                            Scm::symbol("GLOBAL-MARKER"),
                            // (new-import (quote assert-eq))
                            globals::new_minus_import
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("assert-eq")]),
                            // (new-import (quote assert-equal))
                            globals::new_minus_import
                                .with(|value| value.get())
                                .invoke(&[Scm::symbol("assert-equal")]),
                        ])
                    }
                })
            })
        });
        // (define (ensure-var! name env) (let ((var (lookup name env))) (if var var (adjoin-global! name env))))
        globals::ensure_minus_var_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let ((var (lookup name env))) (if var var (adjoin-global! name env))))
                    {
                        // (let ((var (lookup name env))) (if var var (adjoin-global! name env)))
                        {
                            let [var] = [
                                // (lookup name env)
                                globals::lookup
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()]),
                            ];
                            if (var.clone()).is_true() {
                                var.clone()
                            } else {
                                // (adjoin-global! name env)
                                globals::adjoin_minus_global_i
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), env.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (lookup name env) (cond ((null? env) #f) ((eq? (quote GLOBAL-MARKER) (car env)) (lookup name (cdr env))) ((eq? name (caar env)) (cdar env)) (else (lookup name (cdr env)))))
        globals::lookup.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? env) #f) ((eq? (quote GLOBAL-MARKER) (car env)) (lookup name (cdr env))) ((eq? name (caar env)) (cdar env)) (else (lookup name (cdr env)))))
                    {
                        // (cond ((null? env) #f) ((eq? (quote GLOBAL-MARKER) (car env)) (lookup name (cdr env))) ((eq? name (caar env)) (cdar env)) (else (lookup name (cdr env))))
                        if (
                            // (null? env)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[env.clone()])
                        )
                        .is_true()
                        {
                            Scm::False
                        } else if (
                            // (eq? (quote GLOBAL-MARKER) (car env))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("GLOBAL-MARKER"),
                                // (car env)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (lookup name (cdr env))
                            globals::lookup.with(|value| value.get()).invoke(&[
                                name.clone(),
                                // (cdr env)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ])
                        } else if (
                            // (eq? name (caar env))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                name.clone(),
                                // (caar env)
                                imports::caar
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            // (cdar env)
                            imports::cdar
                                .with(|value| value.get())
                                .invoke(&[env.clone()])
                        } else {
                            // (lookup name (cdr env))
                            globals::lookup.with(|value| value.get()).invoke(&[
                                name.clone(),
                                // (cdr env)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ])
                        }
                    }
                })
            })
        });
        // (define (find-globals env) (if (eq? (quote GLOBAL-MARKER) (car env)) env (find-globals (cdr env))))
        globals::find_minus_globals.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let env = args[0].clone();
                    // (letrec () (if (eq? (quote GLOBAL-MARKER) (car env)) env (find-globals (cdr env))))
                    {
                        if (
                            // (eq? (quote GLOBAL-MARKER) (car env))
                            imports::eq_p.with(|value| value.get()).invoke(&[
                                Scm::symbol("GLOBAL-MARKER"),
                                // (car env)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ])
                        )
                        .is_true()
                        {
                            env.clone()
                        } else {
                            // (find-globals (cdr env))
                            globals::find_minus_globals
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr env)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[env.clone()]),
                                ])
                        }
                    }
                })
            })
        });
        // (define (adjoin-global! name env) (adjoin-global-var! (new-global name) env))
        globals::adjoin_minus_global_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (adjoin-global-var! (new-global name) env))
                    {
                        // (adjoin-global-var! (new-global name) env)
                        globals::adjoin_minus_global_minus_var_i
                            .with(|value| value.get())
                            .invoke(&[
                                // (new-global name)
                                globals::new_minus_global
                                    .with(|value| value.get())
                                    .invoke(&[name.clone()]),
                                env.clone(),
                            ])
                    }
                })
            })
        });
        // (define (adjoin-import! name env) (adjoin-global-var! (new-import name) env))
        globals::adjoin_minus_import_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (adjoin-global-var! (new-import name) env))
                    {
                        // (adjoin-global-var! (new-import name) env)
                        globals::adjoin_minus_global_minus_var_i
                            .with(|value| value.get())
                            .invoke(&[
                                // (new-import name)
                                globals::new_minus_import
                                    .with(|value| value.get())
                                    .invoke(&[name.clone()]),
                                env.clone(),
                            ])
                    }
                })
            })
        });
        // (define (adjoin-global-var! var env) (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) (cdr var)))
        globals::adjoin_minus_global_minus_var_i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let var = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) (cdr var)))
                    {
                        // (let ((genv (find-globals env))) (set-cdr! genv (cons var (cdr genv))) (cdr var))
                        {
                            let [genv] = [
                                // (find-globals env)
                                globals::find_minus_globals
                                    .with(|value| value.get())
                                    .invoke(&[env.clone()]),
                            ];
                            {
                                // (set-cdr! genv (cons var (cdr genv)))
                                imports::set_minus_cdr_i.with(|value| value.get()).invoke(&[
                                    genv.clone(),
                                    // (cons var (cdr genv))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        var.clone(),
                                        // (cdr genv)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[genv.clone()]),
                                    ]),
                                ]);
                                // (cdr var)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[var.clone()])
                            }
                        }
                    }
                })
            })
        });
        // (define (adjoin-local name env) (cons (new-local name) env))
        globals::adjoin_minus_local.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cons (new-local name) env))
                    {
                        // (cons (new-local name) env)
                        imports::cons.with(|value| value.get()).invoke(&[
                            // (new-local name)
                            globals::new_minus_local
                                .with(|value| value.get())
                                .invoke(&[name.clone()]),
                            env.clone(),
                        ])
                    }
                })
            })
        });
        // (define (adjoin-local-env name* env) (cond ((null? name*) env) ((pair? name*) (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))) (else (adjoin-local name* env))))
        globals::adjoin_minus_local_minus_env.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? name*) env) ((pair? name*) (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))) (else (adjoin-local name* env))))
                    {
                        // (cond ((null? name*) env) ((pair? name*) (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))) (else (adjoin-local name* env)))
                        if (
                            // (null? name*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone()])
                        )
                        .is_true()
                        {
                            env.clone()
                        } else if (
                            // (pair? name*)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone()])
                        )
                        .is_true()
                        {
                            // (adjoin-local-env (cdr name*) (adjoin-local (car name*) env))
                            globals::adjoin_minus_local_minus_env
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr name*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[name_star_.clone()]),
                                    // (adjoin-local (car name*) env)
                                    globals::adjoin_minus_local
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car name*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[name_star_.clone()]),
                                            env.clone(),
                                        ]),
                                ])
                        } else {
                            // (adjoin-local name* env)
                            globals::adjoin_minus_local
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone(), env.clone()])
                        }
                    }
                })
            })
        });
        // (define (adjoin-import*! name* env) (define (loop name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))) (loop name* (find-globals env)))
        globals::adjoin_minus_import_star__i.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec ((loop (lambda (name* genv) (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv)))))) (loop name* (find-globals env)))
                    {
                        let loop_ = Scm::uninitialized().into_boxed();
                        loop_.set({
                            let loop_ = loop_.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let name_star_ = args[0].clone();
                                let genv = args[1].clone();
                                // (letrec () (if (null? name*) (quote ()) (begin (set-cdr! genv (cons (new-import (car name*)) (cdr genv))) (loop (cdr name*) genv))))
                                {
                                    if (
                                        // (null? name*)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[name_star_.clone()])
                                    )
                                    .is_true()
                                    {
                                        Scm::Nil
                                    } else {
                                        {
                                            // (set-cdr! genv (cons (new-import (car name*)) (cdr genv)))
                                            imports::set_minus_cdr_i
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    genv.clone(),
                                                    // (cons (new-import (car name*)) (cdr genv))
                                                    imports::cons.with(|value| value.get()).invoke(
                                                        &[
                                                            // (new-import (car name*))
                                                            globals::new_minus_import
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (car name*)
                                                                    imports::car
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            name_star_.clone()
                                                                        ]),
                                                                ]),
                                                            // (cdr genv)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[genv.clone()]),
                                                        ],
                                                    ),
                                                ]);
                                            // (loop (cdr name*) genv)
                                            loop_.get().invoke(&[
                                                // (cdr name*)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[name_star_.clone()]),
                                                genv.clone(),
                                            ])
                                        }
                                    }
                                }
                            })
                        });

                        // (loop name* (find-globals env))
                        loop_.get().invoke(&[
                            name_star_.clone(),
                            // (find-globals env)
                            globals::find_minus_globals
                                .with(|value| value.get())
                                .invoke(&[env.clone()]),
                        ])
                    }
                })
            })
        });
        // (define (adjoin-boxed name env) (cons (new-boxed name) env))
        globals::adjoin_minus_boxed.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cons (new-boxed name) env))
                    {
                        // (cons (new-boxed name) env)
                        imports::cons.with(|value| value.get()).invoke(&[
                            // (new-boxed name)
                            globals::new_minus_boxed
                                .with(|value| value.get())
                                .invoke(&[name.clone()]),
                            env.clone(),
                        ])
                    }
                })
            })
        });
        // (define (adjoin-boxed-env name* env) (cond ((null? name*) env) ((pair? name*) (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))) (else (adjoin-boxed name* env))))
        globals::adjoin_minus_boxed_minus_env.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let name_star_ = args[0].clone();
                    let env = args[1].clone();
                    // (letrec () (cond ((null? name*) env) ((pair? name*) (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))) (else (adjoin-boxed name* env))))
                    {
                        // (cond ((null? name*) env) ((pair? name*) (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))) (else (adjoin-boxed name* env)))
                        if (
                            // (null? name*)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone()])
                        )
                        .is_true()
                        {
                            env.clone()
                        } else if (
                            // (pair? name*)
                            imports::pair_p
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone()])
                        )
                        .is_true()
                        {
                            // (adjoin-boxed-env (cdr name*) (adjoin-boxed (car name*) env))
                            globals::adjoin_minus_boxed_minus_env
                                .with(|value| value.get())
                                .invoke(&[
                                    // (cdr name*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[name_star_.clone()]),
                                    // (adjoin-boxed (car name*) env)
                                    globals::adjoin_minus_boxed
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car name*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[name_star_.clone()]),
                                            env.clone(),
                                        ]),
                                ])
                        } else {
                            // (adjoin-boxed name* env)
                            globals::adjoin_minus_boxed
                                .with(|value| value.get())
                                .invoke(&[name_star_.clone(), env.clone()])
                        }
                    }
                })
            })
        });
        // (define (new-import name) (cons name (variable (quote IMPORT-REF) (quote IMPORT-SET) #f)))
        globals::new_minus_import.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (variable (quote IMPORT-REF) (quote IMPORT-SET) #f)))
                    {
                        // (cons name (variable (quote IMPORT-REF) (quote IMPORT-SET) #f))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (variable (quote IMPORT-REF) (quote IMPORT-SET) #f)
                            imports::variable.with(|value| value.get()).invoke(&[
                                Scm::symbol("IMPORT-REF"),
                                Scm::symbol("IMPORT-SET"),
                                Scm::False,
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (new-global name) (cons name (variable (quote GLOBAL-REF) (quote GLOBAL-SET) #f)))
        globals::new_minus_global.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (variable (quote GLOBAL-REF) (quote GLOBAL-SET) #f)))
                    {
                        // (cons name (variable (quote GLOBAL-REF) (quote GLOBAL-SET) #f))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (variable (quote GLOBAL-REF) (quote GLOBAL-SET) #f)
                            imports::variable.with(|value| value.get()).invoke(&[
                                Scm::symbol("GLOBAL-REF"),
                                Scm::symbol("GLOBAL-SET"),
                                Scm::False,
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (new-local name) (cons name (variable (quote LOCAL-REF) (quote LOCAL-SET) #f)))
        globals::new_minus_local.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (variable (quote LOCAL-REF) (quote LOCAL-SET) #f)))
                    {
                        // (cons name (variable (quote LOCAL-REF) (quote LOCAL-SET) #f))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (variable (quote LOCAL-REF) (quote LOCAL-SET) #f)
                            imports::variable.with(|value| value.get()).invoke(&[
                                Scm::symbol("LOCAL-REF"),
                                Scm::symbol("LOCAL-SET"),
                                Scm::False,
                            ]),
                        ])
                    }
                })
            })
        });
        // (define (new-boxed name) (cons name (variable (quote BOXED-REF) (quote BOXED-SET) #f)))
        globals::new_minus_boxed.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    // (letrec () (cons name (variable (quote BOXED-REF) (quote BOXED-SET) #f)))
                    {
                        // (cons name (variable (quote BOXED-REF) (quote BOXED-SET) #f))
                        imports::cons.with(|value| value.get()).invoke(&[
                            name.clone(),
                            // (variable (quote BOXED-REF) (quote BOXED-SET) #f)
                            imports::variable.with(|value| value.get()).invoke(&[
                                Scm::symbol("BOXED-REF"),
                                Scm::symbol("BOXED-SET"),
                                Scm::False,
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
                            globals::make_minus_abstraction
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
                                        globals::make_minus_boxify
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
                            globals::make_minus_vararg_minus_abstraction
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
                                        globals::make_minus_boxify
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
        // (define (any f seq) (if (pair? seq) (if (f (car seq)) #t (any f (cdr seq))) #f))
        globals::any.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 2 {
                        panic!("invalid arity")
                    }
                    let f = args[0].clone();
                    let seq = args[1].clone();
                    // (letrec () (if (pair? seq) (if (f (car seq)) #t (any f (cdr seq))) #f))
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
                                // (f (car seq))
                                f.clone().invoke(&[
                                    // (car seq)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()]),
                                ])
                            )
                            .is_true()
                            {
                                Scm::True
                            } else {
                                // (any f (cdr seq))
                                globals::any.with(|value| value.get()).invoke(&[
                                    f.clone(),
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
