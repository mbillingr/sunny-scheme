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
    pub use crate::sunny::variable::exports::*;
}

pub mod exports {
    pub use super::astify_minus_library;
    pub use super::astify_minus_program;
    pub use super::astify_minus_toplevel;
}

pub fn astify_minus_library(args: &[Scm]) -> Scm {
    {if args.len() != 4{panic!("invalid arity")}let name = args[0].clone();let exp_star_ = args[1].clone();let library_minus_env = args[2].clone();let ast_minus_transform = args[3].clone();{
// (letrec ((init (make-set)) (body (make-nop)) (global-env (make-core-env)) (imports (quote ())) (exports (quote ())) (process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env ast-transform) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*))))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-library name globals init body imports exports))))
{
// (let ((init (quote *uninitialized*)) (body (quote *uninitialized*)) (global-env (quote *uninitialized*)) (imports (quote *uninitialized*)) (exports (quote *uninitialized*)) (process-library-decls (quote *uninitialized*))) (begin (set! init (make-set)) (set! body (make-nop)) (set! global-env (make-core-env)) (set! imports (quote ())) (set! exports (quote ())) (set! process-library-decls (lambda (exp*) (cond ((null? exp*) (quote DONE)) ((eq? (quote export) (caar exp*)) (set! exports (append exports (astify-export (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env ast-transform) (set! init (set-add* init (import-libnames (car exp*)))) (set! imports (append imports (astify-import (cdar exp*) global-env))) (process-library-decls (cdr exp*))) ((eq? (quote begin) (caar exp*)) (set! body (make-sequence body (astify-sequence (cdar exp*) global-env #f))) (process-library-decls (cdr exp*)))))) (process-library-decls exp*) (let* ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-library name globals init body imports exports)))))
{let [init, body, global_minus_env, imports, exports, process_minus_library_minus_decls, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_library_minus_decls = process_minus_library_minus_decls.into_boxed();{let exports = exports.into_boxed();{let imports = imports.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{let body = body.into_boxed();{let init = init.into_boxed();{init.set({
// (make-set)
imports::make_minus_set(&[])});Scm::anything();body.set({
// (make-nop)
imports::make_minus_nop(&[])});Scm::anything();global_minus_env.set({
// (make-core-env)
imports::make_minus_core_minus_env(&[])});Scm::anything();imports.set(Scm::Nil);Scm::anything();exports.set(Scm::Nil);Scm::anything();process_minus_library_minus_decls.set({// Closure
let exports = exports.clone();let global_minus_env = global_minus_env.clone();let process_minus_library_minus_decls = process_minus_library_minus_decls.clone();let library_minus_env = library_minus_env.clone();let ast_minus_transform = ast_minus_transform.clone();let init = init.clone();let imports = imports.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();{
// (cond ...)
if ({
// (null? exp*)
imports::null_p(&[exp_star_.clone()])}).is_true() {Scm::symbol("DONE")} else if ({
// (eq? (quote export) (caar exp*))
imports::eq_p(&[Scm::symbol("export"),{
// (caar exp*)
imports::caar(&[exp_star_.clone()])}])}).is_true() {{exports.set({
// (append exports (astify-export (cdar exp*) global-env))
imports::append(&[exports.get(),{
// (astify-export (cdar exp*) global-env)
imports::astify_minus_export(&[{
// (cdar exp*)
imports::cdar(&[exp_star_.clone()])},global_minus_env.get()])}])});Scm::anything();{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr(&[exp_star_.clone()])}])}}} else if ({
// (import? (car exp*))
imports::import_p(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env ast-transform)
Scm::func(register_minus_libraries).invoke(&[{
// (import-libnames (car exp*))
imports::import_minus_libnames(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])},library_minus_env.clone(),ast_minus_transform.clone()])};init.set({
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_(&[init.get(),{
// (import-libnames (car exp*))
imports::import_minus_libnames(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])}])});Scm::anything();imports.set({
// (append imports (astify-import (cdar exp*) global-env))
imports::append(&[imports.get(),{
// (astify-import (cdar exp*) global-env)
imports::astify_minus_import(&[{
// (cdar exp*)
imports::cdar(&[exp_star_.clone()])},global_minus_env.get()])}])});Scm::anything();{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr(&[exp_star_.clone()])}])}}} else if ({
// (eq? (quote begin) (caar exp*))
imports::eq_p(&[Scm::symbol("begin"),{
// (caar exp*)
imports::caar(&[exp_star_.clone()])}])}).is_true() {{body.set({
// (make-sequence body (astify-sequence (cdar exp*) global-env #f))
imports::make_minus_sequence(&[body.get(),{
// (astify-sequence (cdar exp*) global-env #f)
imports::astify_minus_sequence(&[{
// (cdar exp*)
imports::cdar(&[exp_star_.clone()])},global_minus_env.get(),Scm::False])}])});Scm::anything();{
// (process-library-decls (cdr exp*))
process_minus_library_minus_decls.get().invoke(&[{
// (cdr exp*)
imports::cdr(&[exp_star_.clone()])}])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});Scm::anything();{
// (process-library-decls exp*)
process_minus_library_minus_decls.get().invoke(&[exp_star_.clone()])};{
// (let* ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-library name globals init body imports exports)))
{
// (let ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (begin (ast-transform (make-library name globals init body imports exports))))
{let globals = {
// (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env))
imports::sort(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (variable-name a) (variable-name b))
imports::string_l__p(&[{
// (variable-name a)
imports::variable_minus_name(&[a.clone()])},{
// (variable-name b)
imports::variable_minus_name(&[b.clone()])}])}})},{
// (cdr global-env)
imports::cdr(&[global_minus_env.get()])}])};{
// (ast-transform (make-library name globals init body imports exports))
ast_minus_transform.clone().invoke(&[{
// (make-library name globals init body imports exports)
imports::make_minus_library(&[name.clone(),globals.clone(),init.get(),body.get(),imports.get(),exports.get()])}])}}}}}}}}}}}}}}}.into()
}
pub fn astify_minus_program(args: &[Scm]) -> Scm {
    {if args.len() != 2{panic!("invalid arity")}let exp_star_ = args[0].clone();let ast_minus_transform = args[1].clone();{
// (letrec ((global-env (make-core-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env ast-transform) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((body (astify-sequence exp* global-env #f)) (globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-program globals imports init body (filter cdr (car library-env)))))))))) (process-imports exp* (quote ()) (make-set)))
{
// (let ((global-env (quote *uninitialized*)) (library-env (quote *uninitialized*)) (process-imports (quote *uninitialized*))) (begin (set! global-env (make-core-env)) (set! library-env (list (quote ()))) (set! process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env ast-transform) (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((body (astify-sequence exp* global-env #f)) (globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-program globals imports init body (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set))))
{let [global_minus_env, library_minus_env, process_minus_imports, ] = [Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*"),Scm::symbol("*uninitialized*")];{let process_minus_imports = process_minus_imports.into_boxed();{let library_minus_env = library_minus_env.into_boxed();{let global_minus_env = global_minus_env.into_boxed();{global_minus_env.set({
// (make-core-env)
imports::make_minus_core_minus_env(&[])});Scm::anything();library_minus_env.set({
// (list (quote ()))
imports::list(&[Scm::Nil])});Scm::anything();process_minus_imports.set({// Closure
let library_minus_env = library_minus_env.clone();let ast_minus_transform = ast_minus_transform.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();{
// (cond ...)
if ({
// (import? (car exp*))
imports::import_p(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])}).is_true() {{{
// (register-libraries (import-libnames (car exp*)) library-env ast-transform)
Scm::func(register_minus_libraries).invoke(&[{
// (import-libnames (car exp*))
imports::import_minus_libnames(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])},library_minus_env.get(),ast_minus_transform.clone()])};{
// (process-imports (cdr exp*) (append imports (astify-import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[{
// (cdr exp*)
imports::cdr(&[exp_star_.clone()])},{
// (append imports (astify-import (cdar exp*) global-env))
imports::append(&[imports.clone(),{
// (astify-import (cdar exp*) global-env)
imports::astify_minus_import(&[{
// (cdar exp*)
imports::cdar(&[exp_star_.clone()])},global_minus_env.get()])}])},{
// (set-add* init (import-libnames (car exp*)))
imports::set_minus_add_star_(&[init.clone(),{
// (import-libnames (car exp*))
imports::import_minus_libnames(&[{
// (car exp*)
imports::car(&[exp_star_.clone()])}])}])}])}}} else {{
// (let* ((body (astify-sequence exp* global-env #f)) (globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (ast-transform (make-program globals imports init body (filter cdr (car library-env)))))
{
// (let ((body (astify-sequence exp* global-env #f))) (let ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (begin (ast-transform (make-program globals imports init body (filter cdr (car library-env)))))))
{let body = {
// (astify-sequence exp* global-env #f)
imports::astify_minus_sequence(&[exp_star_.clone(),global_minus_env.get(),Scm::False])};
// (let ((globals (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env)))) (begin (ast-transform (make-program globals imports init body (filter cdr (car library-env))))))
let globals = {
// (sort (lambda (a b) (string<? (variable-name a) (variable-name b))) (cdr global-env))
imports::sort(&[{// Closure
Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let a = args[0].clone();let b = args[1].clone();{
// (string<? (variable-name a) (variable-name b))
imports::string_l__p(&[{
// (variable-name a)
imports::variable_minus_name(&[a.clone()])},{
// (variable-name b)
imports::variable_minus_name(&[b.clone()])}])}})},{
// (cdr global-env)
imports::cdr(&[global_minus_env.get()])}])};{
// (ast-transform (make-program globals imports init body (filter cdr (car library-env))))
ast_minus_transform.clone().invoke(&[{
// (make-program globals imports init body (filter cdr (car library-env)))
imports::make_minus_program(&[globals.clone(),imports.clone(),init.clone(),body.clone(),{
// (filter cdr (car library-env))
imports::filter(&[Scm::func(imports::cdr),{
// (car library-env)
imports::car(&[library_minus_env.get()])}])}])}])}}}}}}})});Scm::anything();{
// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(),Scm::Nil,{
// (make-set)
imports::make_minus_set(&[])}])}}}}}}}}}.into()
}
pub fn astify_minus_toplevel(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let exp_star_ = args[0].clone();
        let ast_minus_transform = args[1].clone();
        if ({
            // (library? (car exp*))
            imports::library_p(&[{
                // (car exp*)
                imports::car(&[exp_star_.clone()])
            }])
        })
        .is_true()
        {
            {
                // (astify-library (library-name (car exp*)) (library-decls (car exp*)) (list (quote ())) ast-transform)
                Scm::func(astify_minus_library).invoke(&[
                    {
                        // (library-name (car exp*))
                        imports::library_minus_name(&[{
                            // (car exp*)
                            imports::car(&[exp_star_.clone()])
                        }])
                    },
                    {
                        // (library-decls (car exp*))
                        imports::library_minus_decls(&[{
                            // (car exp*)
                            imports::car(&[exp_star_.clone()])
                        }])
                    },
                    {
                        // (list (quote ()))
                        imports::list(&[Scm::Nil])
                    },
                    ast_minus_transform.clone(),
                ])
            }
        } else {
            {
                // (astify-program exp* ast-transform)
                Scm::func(astify_minus_program)
                    .invoke(&[exp_star_.clone(), ast_minus_transform.clone()])
            }
        }
    }
    .into()
}
pub fn register_minus_libraries(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let libs = args[0].clone();
        let library_minus_env = args[1].clone();
        let ast_minus_transform = args[2].clone();
        {
            // (cond ...)
            if ({
                // (null? libs)
                imports::null_p(&[libs.clone()])
            })
            .is_true()
            {
                Scm::symbol("DONE")
            } else if ({
                // (equal? (quote (sunny testing)) (car libs))
                imports::equal_p(&[
                    Scm::pair(
                        Scm::symbol("sunny"),
                        Scm::pair(Scm::symbol("testing"), Scm::Nil),
                    ),
                    {
                        // (car libs)
                        imports::car(&[libs.clone()])
                    },
                ])
            })
            .is_true()
            {
                {
                    // (register-libraries (cdr libs) library-env ast-transform)
                    Scm::func(register_minus_libraries).invoke(&[
                        {
                            // (cdr libs)
                            imports::cdr(&[libs.clone()])
                        },
                        library_minus_env.clone(),
                        ast_minus_transform.clone(),
                    ])
                }
            } else if ({
                // (assoc (car libs) (car library-env))
                imports::assoc(&[
                    {
                        // (car libs)
                        imports::car(&[libs.clone()])
                    },
                    {
                        // (car library-env)
                        imports::car(&[library_minus_env.clone()])
                    },
                ])
            })
            .is_true()
            {
                {
                    // (register-libraries (cdr libs) library-env ast-transform)
                    Scm::func(register_minus_libraries).invoke(&[
                        {
                            // (cdr libs)
                            imports::cdr(&[libs.clone()])
                        },
                        library_minus_env.clone(),
                        ast_minus_transform.clone(),
                    ])
                }
            } else {
                {
                    {
                        // (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env ast-transform) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env))))
                        {
                            // (let ((lib (get-lib (car libs)))) (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env ast-transform) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env))))))
                            {
                                let lib = {
                                    // (get-lib (car libs))
                                    imports::get_minus_lib(&[{
                                        // (car libs)
                                        imports::car(&[libs.clone()])
                                    }])
                                };
                                // (let ((libast (if (library? lib) (astify-library (library-name lib) (library-decls lib) library-env ast-transform) #f))) (begin (set-car! library-env (cons (cons (car libs) libast) (car library-env)))))
                                let libast = if ({
                                    // (library? lib)
                                    imports::library_p(&[lib.clone()])
                                })
                                .is_true()
                                {
                                    {
                                        // (astify-library (library-name lib) (library-decls lib) library-env ast-transform)
                                        astify_minus_library(&[
                                            {
                                                // (library-name lib)
                                                imports::library_minus_name(&[lib.clone()])
                                            },
                                            {
                                                // (library-decls lib)
                                                imports::library_minus_decls(&[lib.clone()])
                                            },
                                            library_minus_env.clone(),
                                            ast_minus_transform.clone(),
                                        ])
                                    }
                                } else {
                                    Scm::False
                                };
                                {
                                    // (set-car! library-env (cons (cons (car libs) libast) (car library-env)))
                                    imports::set_minus_car_i(&[library_minus_env.clone(), {
                                        // (cons (cons (car libs) libast) (car library-env))
                                        imports::cons(&[
                                            {
                                                // (cons (car libs) libast)
                                                imports::cons(&[
                                                    {
                                                        // (car libs)
                                                        imports::car(&[libs.clone()])
                                                    },
                                                    libast.clone(),
                                                ])
                                            },
                                            {
                                                // (car library-env)
                                                imports::car(&[library_minus_env.clone()])
                                            },
                                        ])
                                    }])
                                }
                            }
                        }
                    };
                    {
                        // (register-libraries (cdr libs) library-env ast-transform)
                        Scm::func(register_minus_libraries).invoke(&[
                            {
                                // (cdr libs)
                                imports::cdr(&[libs.clone()])
                            },
                            library_minus_env.clone(),
                            ast_minus_transform.clone(),
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
    crate::sunny::variable::initialize();
    {
        (/*NOP*/);
        {
            // (define (astify-toplevel exp* ast-transform) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-library name exp* library-env ast-transform) ...)
            (/*NOP*/)
        };
        {
            // (define (astify-program exp* ast-transform) ...)
            (/*NOP*/)
        };
        {
            // (define (register-libraries libs library-env ast-transform) ...)
            (/*NOP*/)
        }
    };
}
