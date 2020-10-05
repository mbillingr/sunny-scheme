#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::sunny::ast::exports::*;
    pub use crate::sunny::ast_transforms::boxify::exports::*;
    pub use crate::sunny::ast_transforms::close_procedures::exports::*;
    pub use crate::sunny::ast_transforms::extract_definitions::exports::*;
    pub use crate::sunny::astify::exports::*;
    pub use crate::sunny::library::exports::*;
    pub use crate::sunny::scheme_syntax::exports::*;
    pub use crate::sunny::sets::exports::*;
    pub use crate::sunny::syntax::exports::*;
    pub use crate::sunny::utils::exports::*;
}

pub mod exports {
    pub use super::astify_minus_library;
    pub use super::astify_minus_program;
    pub use super::astify_minus_toplevel;
}

pub fn astify_minus_library(args: &[Scm]) -> Scm {
    {if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let exp_star_ = args[1].clone();let library_minus_env = args[2].clone();{
// (letrec ((init (make-set)) (body (make-nop)) (global-env (make-core-env)) (imports (quote ())) (exports (quote ())) (process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*))))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (extract-definitions (boxify (close-procedures body))) imports exports)))
{
// (let ((init (quote *uninitialized*)) (body (quote *uninitialized*)) (global-env (quote *uninitialized*)) (imports (quote *uninitialized*)) (exports (quote *uninitialized*)) (process-library-decls (quote *uninitialized*))) (begin (set! init (make-set)) (set! body (make-nop)) (set! global-env (make-core-env)) (set! imports (quote ())) (set! exports (quote ())) (set! process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*)))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (extract-definitions (boxify (close-procedures body))) imports exports))))
{let [init, body, global_minus_env, imports, exports, process_minus_library_minus_decls, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_library_minus_decls = process_minus_library_minus_decls.into_boxed();{let exports = exports.into_boxed();{let imports = imports.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{let body = body.into_boxed();{let init = init.into_boxed();{init.set({
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])});body.set({
// (make-nop)
Scm::func(imports::make_minus_nop).invoke(&[])});global_minus_env.set({
// (make-core-env)
Scm::func(imports::make_minus_core_minus_env).invoke(&[])});imports.set(Scm::Nil);exports.set(Scm::Nil);process_minus_library_minus_decls.set({// Closure
let exports = exports.clone();let global_minus_env = global_minus_env.clone();let process_minus_library_minus_decls = process_minus_library_minus_decls.clone();let library_minus_env = library_minus_env.clone();let init = init.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? exp*)
Scm::func(imports::null_p).invoke(&[exp_star_.clone()])}).is_true() {Scm::symbol("DONE")} else if ({
// (eq? (quote export) (caar exp*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("export"),{
// (caar exp*)
Scm::func(imports::caar).invoke(&[exp_star_.clone()])}])}).is_true() {{exports.set({
// (append exports (astify-export (cdar exp*) global-env))
Scm::func(imports::append).invoke(&[exports.get(),{
// (astify-export (cdar exp*) global-env)
Scm::func(imports::astify_minus_export).invoke(&[{
// (cdar exp*)
Scm::func(imports::cdar).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
Scm::func(imports::cdr).invoke(&[exp_star_.clone()])}])}}} else if ({
// (import? (car exp*))
Scm::func(imports::import_p).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env)
Scm::func(register_minus_libraries).invoke(&[{
// (import-libnames (car exp*))
Scm::func(imports::import_minus_libnames).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])},library_minus_env.clone()])};init.set({
// (set-add* init (import-libnames (car exp*)))
Scm::func(imports::set_minus_add_star_).invoke(&[init.get(),{
// (import-libnames (car exp*))
Scm::func(imports::import_minus_libnames).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])}])});imports.set({
// (append imports (astify-import (cdar exp*) global-env))
Scm::func(imports::append).invoke(&[imports.get(),{
// (astify-import (cdar exp*) global-env)
Scm::func(imports::astify_minus_import).invoke(&[{
// (cdar exp*)
Scm::func(imports::cdar).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
Scm::func(imports::cdr).invoke(&[exp_star_.clone()])}])}}} else if ({
// (eq? (quote begin) (caar exp*))
Scm::func(imports::eq_p).invoke(&[Scm::symbol("begin"),{
// (caar exp*)
Scm::func(imports::caar).invoke(&[exp_star_.clone()])}])}).is_true() {{body.set({
// (make-sequence body (astify-sequence (cdar exp*) global-env #f))
Scm::func(imports::make_minus_sequence).invoke(&[body.get(),{
// (astify-sequence (cdar exp*) global-env #f)
Scm::func(imports::astify_minus_sequence).invoke(&[{
// (cdar exp*)
Scm::func(imports::cdar).invoke(&[exp_star_.clone()])},global_minus_env.get(),Scm::False])}])});{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
Scm::func(imports::cdr).invoke(&[exp_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});{
// (process-library-decls exp*)
process_minus_library_minus_decls.get().invoke(&[exp_star_.clone()])};{
// (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-library name globals init (extract-definitions (boxify (close-procedures body))) imports exports))
{
// (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-library name globals init (extract-definitions (boxify (close-procedures body))) imports exports)))
{let globals = {
// (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env))
Scm::func(imports::sort).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (symbol->string (car a)) (symbol->string (car b)))
Scm::func(imports::string_l__p).invoke(&[{
// (symbol->string (car a))
Scm::func(imports::symbol_minus__g_string).invoke(&[{
// (car a)
Scm::func(imports::car).invoke(&[a.clone()])}])},{
// (symbol->string (car b))
Scm::func(imports::symbol_minus__g_string).invoke(&[{
// (car b)
Scm::func(imports::car).invoke(&[b.clone()])}])}])}})},{
// (cdr global-env)
Scm::func(imports::cdr).invoke(&[global_minus_env.get()])}])};{
// (make-library name globals init (extract-definitions (boxify (close-procedures body))) imports exports)
Scm::func(imports::make_minus_library).invoke(&[name.clone(),globals.clone(),init.get(),{
// (extract-definitions (boxify (close-procedures body)))
Scm::func(imports::extract_minus_definitions).invoke(&[{
// (boxify (close-procedures body))
Scm::func(imports::boxify).invoke(&[{
// (close-procedures body)
Scm::func(imports::close_minus_procedures).invoke(&[body.get()])}])}])},imports.get(),exports.get()])}}}}}}}}}}}}}}}.into()
}
pub fn astify_minus_program(args: &[Scm]) -> Scm {
    {if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (letrec ((global-env (make-core-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (main (extract-definitions main)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set)))
{
// (let ((global-env (quote *uninitialized*)) (library-env (quote *uninitialized*)) (process-imports (quote *uninitialized*))) (begin (set! global-env (make-core-env)) (set! library-env (list (quote ()))) (set! process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (main (extract-definitions main)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env)))))))) (process-imports exp* (quote ()) (make-set))))
{let [global_minus_env, library_minus_env, process_minus_imports, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_imports = process_minus_imports.into_boxed();{let library_minus_env = library_minus_env.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{global_minus_env.set({
// (make-core-env)
Scm::func(imports::make_minus_core_minus_env).invoke(&[])});library_minus_env.set({
// (list (quote ()))
Scm::func(imports::list).invoke(&[Scm::Nil])});process_minus_imports.set({// Closure
let library_minus_env = library_minus_env.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();{
// (cond ...)
if ({
// (import? (car exp*))
Scm::func(imports::import_p).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env)
Scm::func(register_minus_libraries).invoke(&[{
// (import-libnames (car exp*))
Scm::func(imports::import_minus_libnames).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])},library_minus_env.get()])};{
// (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[{
// (cdr exp*)
Scm::func(imports::cdr).invoke(&[exp_star_.clone()])},{
// (append imports (astify-import (cdar exp*) global-env))
Scm::func(imports::append).invoke(&[imports.clone(),{
// (astify-import (cdar exp*) global-env)
Scm::func(imports::astify_minus_import).invoke(&[{
// (cdar exp*)
Scm::func(imports::cdar).invoke(&[exp_star_.clone()])},global_minus_env.get()])}])},{
// (set-add* init (import-libnames (car exp*)))
Scm::func(imports::set_minus_add_star_).invoke(&[init.clone(),{
// (import-libnames (car exp*))
Scm::func(imports::import_minus_libnames).invoke(&[{
// (car exp*)
Scm::func(imports::car).invoke(&[exp_star_.clone()])}])}])}])}}} else {{
// (let* ((ast (astify-sequence exp* global-env #f)) (main (boxify (close-procedures ast))) (main (extract-definitions main)) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (make-program globals imports init main (filter cdr (car library-env))))
{
// (let ((ast (astify-sequence exp* global-env #f))) (let ((main (boxify (close-procedures ast)))) (let ((main (extract-definitions main))) (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env))))))))
{let ast = {
// (astify-sequence exp* global-env #f)
Scm::func(imports::astify_minus_sequence).invoke(&[exp_star_.clone(),global_minus_env.get(),Scm::False])};
// (let ((main (boxify (close-procedures ast)))) (let ((main (extract-definitions main))) (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env)))))))
let main = {
// (boxify (close-procedures ast))
Scm::func(imports::boxify).invoke(&[{
// (close-procedures ast)
Scm::func(imports::close_minus_procedures).invoke(&[ast.clone()])}])};
// (let ((main (extract-definitions main))) (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env))))))
let main = {
// (extract-definitions main)
Scm::func(imports::extract_minus_definitions).invoke(&[main.clone()])};
// (let ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (begin (make-program globals imports init main (filter cdr (car library-env)))))
let globals = {
// (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env))
Scm::func(imports::sort).invoke(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (symbol->string (car a)) (symbol->string (car b)))
Scm::func(imports::string_l__p).invoke(&[{
// (symbol->string (car a))
Scm::func(imports::symbol_minus__g_string).invoke(&[{
// (car a)
Scm::func(imports::car).invoke(&[a.clone()])}])},{
// (symbol->string (car b))
Scm::func(imports::symbol_minus__g_string).invoke(&[{
// (car b)
Scm::func(imports::car).invoke(&[b.clone()])}])}])}})},{
// (cdr global-env)
Scm::func(imports::cdr).invoke(&[global_minus_env.get()])}])};{
// (make-program globals imports init main (filter cdr (car library-env)))
Scm::func(imports::make_minus_program).invoke(&[globals.clone(),imports.clone(),init.clone(),main.clone(),{
// (filter cdr (car library-env))
Scm::func(imports::filter).invoke(&[Scm::func(imports::cdr),{
// (car library-env)
Scm::func(imports::car).invoke(&[library_minus_env.get()])}])}])}}}}}}})});{
// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(),Scm::Nil,{
// (make-set)
Scm::func(imports::make_minus_set).invoke(&[])}])}}}}}}}}}.into()
}
pub fn astify_minus_toplevel(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let exp_star_ = args[0].clone();
        if ({
            // (library? (car exp*))
            Scm::func(imports::library_p).invoke(&[{
                // (car exp*)
                Scm::func(imports::car).invoke(&[exp_star_.clone()])
            }])
        })
        .is_true()
        {
            {
                // (astify-library (library-name (car exp*)) (library-decls (car exp*)) (list (quote ())))
                Scm::func(astify_minus_library).invoke(&[
                    {
                        // (library-name (car exp*))
                        Scm::func(imports::library_minus_name).invoke(&[{
                            // (car exp*)
                            Scm::func(imports::car).invoke(&[exp_star_.clone()])
                        }])
                    },
                    {
                        // (library-decls (car exp*))
                        Scm::func(imports::library_minus_decls).invoke(&[{
                            // (car exp*)
                            Scm::func(imports::car).invoke(&[exp_star_.clone()])
                        }])
                    },
                    {
                        // (list (quote ()))
                        Scm::func(imports::list).invoke(&[Scm::Nil])
                    },
                ])
            }
        } else {
            {
                // (astify-program exp*)
                Scm::func(astify_minus_program).invoke(&[exp_star_.clone()])
            }
        }
    }
    .into()
}
pub fn register_minus_libraries(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let libs = args[0].clone();
        let library_minus_env = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? libs)
                Scm::func(imports::null_p).invoke(&[libs.clone()])
            })
            .is_true()
            {
                Scm::symbol("DONE")
            } else if ({
                // (equal? (quote (sunny testing)) (car libs))
                Scm::func(imports::equal_p).invoke(&[
                    Scm::pair(
                        Scm::symbol("sunny"),
                        Scm::pair(Scm::symbol("testing"), Scm::Nil),
                    ),
                    {
                        // (car libs)
                        Scm::func(imports::car).invoke(&[libs.clone()])
                    },
                ])
            })
            .is_true()
            {
                {
                    // (register-libraries (cdr libs) library-env)
                    Scm::func(register_minus_libraries).invoke(&[
                        {
                            // (cdr libs)
                            Scm::func(imports::cdr).invoke(&[libs.clone()])
                        },
                        library_minus_env.clone(),
                    ])
                }
            } else if ({
                // (assoc (car libs) (car library-env))
                Scm::func(imports::assoc).invoke(&[
                    {
                        // (car libs)
                        Scm::func(imports::car).invoke(&[libs.clone()])
                    },
                    {
                        // (car library-env)
                        Scm::func(imports::car).invoke(&[library_minus_env.clone()])
                    },
                ])
            })
            .is_true()
            {
                {
                    // (register-libraries (cdr libs) library-env)
                    Scm::func(register_minus_libraries).invoke(&[
                        {
                            // (cdr libs)
                            Scm::func(imports::cdr).invoke(&[libs.clone()])
                        },
                        library_minus_env.clone(),
                    ])
                }
            } else {
                {
                    {
                        // (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
                        {
                            // (let ((lib (get-lib (car libs)))) (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env))))))
                            {
                                let lib = {
                                    // (get-lib (car libs))
                                    Scm::func(imports::get_minus_lib).invoke(&[{
                                        // (car libs)
                                        Scm::func(imports::car).invoke(&[libs.clone()])
                                    }])
                                };
                                // (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env)))))
                                let libast = if ({
                                    // (library? lib)
                                    Scm::func(imports::library_p).invoke(&[lib.clone()])
                                })
                                .is_true()
                                {
                                    {
                                        // (astify-library (library-name lib) (library-decls lib) library-env)
                                        Scm::func(astify_minus_library).invoke(&[
                                            {
                                                // (library-name lib)
                                                Scm::func(imports::library_minus_name)
                                                    .invoke(&[lib.clone()])
                                            },
                                            {
                                                // (library-decls lib)
                                                Scm::func(imports::library_minus_decls)
                                                    .invoke(&[lib.clone()])
                                            },
                                            library_minus_env.clone(),
                                        ])
                                    }
                                } else {
                                    Scm::False
                                };
                                {
                                    // (set-car! library-env (cons (cons (car libs) libast) (car library-env)))
                                    Scm::func(imports::set_minus_car_i).invoke(&[
                                        library_minus_env.clone(),
                                        {
                                            // (cons (cons (car libs) libast) (car library-env))
                                            Scm::func(imports::cons).invoke(&[
                                                {
                                                    // (cons (car libs) libast)
                                                    Scm::func(imports::cons).invoke(&[
                                                        {
                                                            // (car libs)
                                                            Scm::func(imports::car)
                                                                .invoke(&[libs.clone()])
                                                        },
                                                        libast.clone(),
                                                    ])
                                                },
                                                {
                                                    // (car library-env)
                                                    Scm::func(imports::car)
                                                        .invoke(&[library_minus_env.clone()])
                                                },
                                            ])
                                        },
                                    ])
                                }
                            }
                        }
                    };
                    {
                        // (register-libraries (cdr libs) library-env)
                        Scm::func(register_minus_libraries).invoke(&[
                            {
                                // (cdr libs)
                                Scm::func(imports::cdr).invoke(&[libs.clone()])
                            },
                            library_minus_env.clone(),
                        ])
                    }
                }
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
    crate::scheme::cxr::initialize();
    crate::sunny::ast::initialize();
    crate::sunny::ast_transforms::boxify::initialize();
    crate::sunny::ast_transforms::close_procedures::initialize();
    crate::sunny::ast_transforms::extract_definitions::initialize();
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
            (/*NOP*/)
        };
        {
            // (define (astify-library name exp* library-env) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-program exp*) ...)
            (/*NOP*/)
        };
        {
            // (define (register-libraries libs library-env) ...)
            (/*NOP*/)
        }
    };
}
