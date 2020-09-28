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
    pub use crate::sunny::ast_transforms::boxify::exports::*;
    pub use crate::sunny::env::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::rust::codegen::exports::*;
    pub use crate::sunny::rust::module::exports::*;
    pub use crate::sunny::rust::module_tree::exports::*;
    pub use crate::sunny::rust::rustify::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::sexpr_ast::exports::*;
    pub use crate::sunny::syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::globals::scm_minus__g_ast;
    pub use super::imports::rust_minus_gen_minus_in_minus_module;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static assoc: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assoc"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static sort: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sort"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static register_minus_libraries: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL register-libraries"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static library_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library->ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static program_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL program->ast"))}
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
    crate::sunny::ast_transforms::boxify::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::env::initialize();
    crate::sunny::library::initialize();
    crate::sunny::rust::codegen::initialize();
    crate::sunny::rust::module::initialize();
    crate::sunny::rust::module_tree::initialize();
    crate::sunny::rust::rustify::initialize();
    crate::sunny::scheme_syntax::initialize();
    crate::sunny::sets::initialize();
    crate::sunny::sexpr_ast::initialize();
    crate::sunny::syntax::initialize();
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
                            imports::library_p.with(|value| value.get()).invoke(&[
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
                                    imports::library_minus_name
                                        .with(|value| value.get())
                                        .invoke(&[
                                            // (car exp*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[exp_star_.clone()]),
                                        ]),
                                    // (library-decls (car exp*))
                                    imports::library_minus_decls
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
        // (define (program->ast exp*) (define global-env (make-core-env)) (define library-env (list (quote ()))) (define (process-imports exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (sexpr->sequence exp* global-env #f)) (main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))) (process-imports exp* (quote ()) (make-set)))
        globals::program_minus__g_ast.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();
// (letrec ((global-env (make-core-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (sexpr->sequence exp* global-env #f)) (main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set)))
{let global_minus_env = Scm::uninitialized().into_boxed();
let library_minus_env = Scm::uninitialized().into_boxed();
let process_minus_imports = Scm::uninitialized().into_boxed();
global_minus_env.set(
// (make-core-env)
imports::make_minus_core_minus_env.with(|value| value.get()).invoke(&[]));
library_minus_env.set(
// (list (quote ()))
imports::list.with(|value| value.get()).invoke(&[Scm::Nil,]));
process_minus_imports.set({let library_minus_env = library_minus_env.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();
// (letrec () (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (sexpr->sequence exp* global-env #f)) (main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))))
{
// (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (sexpr->sequence exp* global-env #f)) (main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))
if (
// (import? (car exp*))
imports::import_p.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),])).is_true() {{
// (register-libraries (import-libnames (car exp*)) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),]),library_minus_env.get(),]);
// (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone(),]),
// (append imports (sexpr->import (cdar exp*) global-env))
globals::append.with(|value| value.get()).invoke(&[imports.clone(),
// (sexpr->import (cdar exp*) global-env)
imports::sexpr_minus__g_import.with(|value| value.get()).invoke(&[
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone(),]),global_minus_env.get(),]),]),
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_.with(|value| value.get()).invoke(&[init.clone(),
// (import-libnames (car exp*))
imports::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(),]),]),]),])}} else {
// (let* ((ast (sexpr->sequence exp* global-env #f)) (main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{let [ast, ] = [
// (sexpr->sequence exp* global-env #f)
imports::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[exp_star_.clone(),global_minus_env.get(),Scm::False,]),];
// (letrec () (let* ((main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))
{
// (let* ((main (boxify ast)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{let [main, ] = [
// (boxify ast)
imports::boxify.with(|value| value.get()).invoke(&[ast.clone(),]),];
// (letrec () (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))
{
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
// (letrec () (let* () (make-program globals imports init main (filter cdr (car library-env)))))
{
// (let* () (make-program globals imports init main (filter cdr (car library-env))))

// (make-program globals imports init main (filter cdr (car library-env)))
imports::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(),imports.clone(),init.clone(),main.clone(),
// (filter cdr (car library-env))
imports::filter.with(|value| value.get()).invoke(&[imports::cdr.with(|value| value.get()),
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.get(),]),]),])}}}}}}}}})});

// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(),Scm::Nil,
// (make-set)
imports::make_minus_set.with(|value| value.get()).invoke(&[]),])}})}));
        // (define (library->ast name exp* library-env) (library-decls->ast name exp* (make-set) (make-nop) (make-core-env) library-env (quote ()) (quote ())))
        globals::library_minus__g_ast.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 3 {
                        panic!("invalid arity")
                    }
                    let name = args[0].clone();
                    let exp_star_ = args[1].clone();
                    let library_minus_env = args[2].clone();
                    // (letrec () (library-decls->ast name exp* (make-set) (make-nop) (make-core-env) library-env (quote ()) (quote ())))
                    {
                        // (library-decls->ast name exp* (make-set) (make-nop) (make-core-env) library-env (quote ()) (quote ()))
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
                                // (make-core-env)
                                imports::make_minus_core_minus_env
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
                                        imports::sexpr_minus__g_export
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
                            imports::import_p.with(|value| value.get()).invoke(&[
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
                                        imports::import_minus_libnames
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
                                                imports::import_minus_libnames
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
                                            imports::sexpr_minus__g_import
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
                                            imports::sexpr_minus__g_sequence
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
                                        imports::get_minus_lib.with(|value| value.get()).invoke(&[
                                            // (car libs)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[libs.clone()]),
                                        ]),
                                    ];
                                    // (letrec () (let* ((libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))))
                                    {
                                        // (let* ((libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
                                        {
                                            let [libast] = [
                                                if (
                                                    // (library? lib)
                                                    imports::library_p
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
                                                            imports::library_minus_name
                                                                .with(|value| value.get())
                                                                .invoke(&[lib.clone()]),
                                                            // (library-decls lib)
                                                            imports::library_minus_decls
                                                                .with(|value| value.get())
                                                                .invoke(&[lib.clone()]),
                                                            library_minus_env.clone(),
                                                        ])
                                                } else {
                                                    Scm::False
                                                },
                                            ];
                                            // (letrec () (let* () (set-car! library-env (cons (cons (car libs) libast) (car library-env)))))
                                            {
                                                // (let* () (set-car! library-env (cons (cons (car libs) libast) (car library-env))))

                                                // (set-car! library-env (cons (cons (car libs) libast) (car library-env)))
                                                imports::set_minus_car_i
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        library_minus_env.clone(),
                                                        // (cons (cons (car libs) libast) (car library-env))
                                                        imports::cons
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (cons (car libs) libast)
                                                                imports::cons
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (car libs)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(
                                                                                &[libs.clone()],
                                                                            ),
                                                                        libast.clone(),
                                                                    ]),
                                                                // (car library-env)
                                                                imports::car
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        library_minus_env.clone()
                                                                    ]),
                                                            ]),
                                                    ])
                                            }
                                        }
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
                                // (letrec () (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                                {
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
                                                imports::filter.with(|value| value.get()).invoke(
                                                    &[
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
                                                    ],
                                                ),
                                            ]),
                                        ]),
                                    ])
                                }
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
