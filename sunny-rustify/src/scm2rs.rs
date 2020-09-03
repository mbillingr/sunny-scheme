#[allow(unused_imports)]
use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};
mod imports {
    pub use crate::scheme::base::exports::*;
    pub use crate::scheme::cxr::exports::*;
    pub use crate::scheme::file::exports::*;
    pub use crate::scheme::process_context::exports::command_minus_line;
    pub use crate::scheme::read::exports::*;
    pub use crate::scheme::write::exports::*;
    pub use crate::sunny::translate::exports::*;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ast"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static input_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL input-file-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static load_minus_sexpr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL load-sexpr"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_file: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-file"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static output_minus_file_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL output-file-name"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL program"))}
}

pub fn main() {
    eprintln!("built with");
    eprintln!("    '{}' memory model", MEMORY_MODEL_KIND);

    crate::scheme::base::initialize();
    crate::scheme::cxr::initialize();
    crate::scheme::file::initialize();
    crate::scheme::read::initialize();
    crate::scheme::write::initialize();
    crate::scheme::process_context::initialize();
    crate::sunny::translate::initialize();
    {
        // (define input-file-name (cadr (command-line)))
        globals::input_minus_file_minus_name.with(|value| {
            value.set(
                // (cadr (command-line))
                imports::cadr.with(|value| value.get()).invoke(&[
                    // (command-line)
                    imports::command_minus_line
                        .with(|value| value.get())
                        .invoke(&[]),
                ]),
            )
        });
        // (define output-file-name (caddr (command-line)))
        globals::output_minus_file_minus_name.with(|value| {
            value.set(
                // (caddr (command-line))
                imports::caddr.with(|value| value.get()).invoke(&[
                    // (command-line)
                    imports::command_minus_line
                        .with(|value| value.get())
                        .invoke(&[]),
                ]),
            )
        });
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (display input-file-name)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::input_minus_file_minus_name.with(|value| value.get())]);
        // (display " --> ")
        imports::display
            .with(|value| value.get())
            .invoke(&[Scm::from(" --> ")]);
        // (display output-file-name)
        imports::display
            .with(|value| value.get())
            .invoke(&[globals::output_minus_file_minus_name.with(|value| value.get())]);
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (newline)
        imports::newline.with(|value| value.get()).invoke(&[]);
        // (define input-file (open-input-file input-file-name))
        globals::input_minus_file.with(|value| {
            value.set(
                // (open-input-file input-file-name)
                imports::open_minus_input_minus_file
                    .with(|value| value.get())
                    .invoke(&[globals::input_minus_file_minus_name.with(|value| value.get())]),
            )
        });
        // (define (load-sexpr) (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr)))))
        globals::load_minus_sexpr.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr)))))
                    {
                        // (let ((expr (read input-file))) (if (eof-object? expr) (quote ()) (cons expr (load-sexpr))))
                        {
                            let [expr] = [
                                // (read input-file)
                                imports::read
                                    .with(|value| value.get())
                                    .invoke(&[globals::input_minus_file.with(|value| value.get())]),
                            ];
                            if (
                                // (eof-object? expr)
                                imports::eof_minus_object_p
                                    .with(|value| value.get())
                                    .invoke(&[expr.clone()])
                            )
                            .is_true()
                            {
                                Scm::Nil
                            } else {
                                // (cons expr (load-sexpr))
                                imports::cons.with(|value| value.get()).invoke(&[
                                    expr.clone(),
                                    // (load-sexpr)
                                    globals::load_minus_sexpr
                                        .with(|value| value.get())
                                        .invoke(&[]),
                                ])
                            }
                        }
                    }
                })
            })
        });
        // (define program (load-sexpr))
        globals::program.with(|value| {
            value.set(
                // (load-sexpr)
                globals::load_minus_sexpr
                    .with(|value| value.get())
                    .invoke(&[]),
            )
        });
        // (define ast (scm->ast program))
        globals::ast.with(|value| {
            value.set(
                // (scm->ast program)
                imports::scm_minus__g_ast
                    .with(|value| value.get())
                    .invoke(&[globals::program.with(|value| value.get())]),
            )
        });
        // (define output-file (open-output-file output-file-name))
        globals::output_minus_file.with(|value| {
            value.set(
                // (open-output-file output-file-name)
                imports::open_minus_output_minus_file
                    .with(|value| value.get())
                    .invoke(&[globals::output_minus_file_minus_name.with(|value| value.get())]),
            )
        });
        // (ast (quote gen-rust) output-file)
        globals::ast.with(|value| value.get()).invoke(&[
            Scm::symbol("gen-rust"),
            globals::output_minus_file.with(|value| value.get()),
        ]);
        // (close-port output-file)
        imports::close_minus_port
            .with(|value| value.get())
            .invoke(&[globals::output_minus_file.with(|value| value.get())])
    };
}
pub mod scheme {
    pub mod base {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::native::base::exports::*;
        }

        pub mod exports {
            pub use super::globals::assq;
            pub use super::globals::for_minus_each;
            pub use super::globals::length;
            pub use super::globals::list;
            pub use super::globals::list_minus_copy;
            pub use super::globals::list_p;
            pub use super::globals::map;
            pub use super::globals::memq;
            pub use super::globals::not;
            pub use super::globals::reverse;
            pub use super::globals::string_e__p;
            pub use super::globals::symbol_e__p;
            pub use super::imports::_e_;
            pub use super::imports::_g_;
            pub use super::imports::_l_;
            pub use super::imports::_minus_;
            pub use super::imports::_plus_;
            pub use super::imports::apply;
            pub use super::imports::caar;
            pub use super::imports::cadr;
            pub use super::imports::car;
            pub use super::imports::cdar;
            pub use super::imports::cddr;
            pub use super::imports::cdr;
            pub use super::imports::char_p;
            pub use super::imports::close_minus_port;
            pub use super::imports::cons;
            pub use super::imports::eof_minus_object_p;
            pub use super::imports::eq_p;
            pub use super::imports::equal_p;
            pub use super::imports::error;
            pub use super::imports::list_minus__g_string;
            pub use super::imports::null_p;
            pub use super::imports::pair_p;
            pub use super::imports::set_minus_car_i;
            pub use super::imports::set_minus_cdr_i;
            pub use super::imports::string_l__p;
            pub use super::imports::string_minus__g_list;
            pub use super::imports::string_minus_append;
            pub use super::imports::symbol_minus__g_string;
            pub use super::imports::symbol_p;
        }

        mod globals {
            use sunny_core::{Mut, Scm};
            thread_local! {#[allow(non_upper_case_globals)] pub static all_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL all?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static map_minus_1: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL map-1"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static any_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL any?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static __map: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _map"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static map: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL map"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static __for_minus_each: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _for-each"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static for_minus_each: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL for-each"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static __string_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _string=?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static string_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL string=?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static s: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL s"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static __symbol_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _symbol=?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static symbol_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL symbol=?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static memq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL memq"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static assq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assq"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static seq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL seq"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static fold_minus_right: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL fold-right"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static list_minus_copy: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL list-copy"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static reverse: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL reverse"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static fold_minus_left: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL fold-left"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static length: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL length"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static list_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL list?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL list"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static not: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL not"))}
        }

        thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

        pub fn initialize() {
            if INITIALIZED.with(|x| x.get()) {
                return;
            }
            INITIALIZED.with(|x| x.set(true));

            crate::native::base::initialize();
            {
                (/*NOP*/);
                // (define (not x) (if x #f #t))
                globals::not.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (if x #f #t))
                            {
                                if (x.clone()).is_true() {
                                    Scm::False
                                } else {
                                    Scm::True
                                }
                            }
                        })
                    })
                });
                // (define (list . x) x)
                globals::list.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 0 {
                                panic!("not enough args")
                            }
                            let x = Scm::list(&args[0..]);
                            // (letrec () x)
                            {
                                x.clone()
                            }
                        })
                    })
                });
                // (define (list? seq) (cond ((null? seq) #t) ((pair? seq) (list? (cdr seq))) (else #f)))
                globals::list_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (cond ((null? seq) #t) ((pair? seq) (list? (cdr seq))) (else #f)))
                            {
                                // (cond ((null? seq) #t) ((pair? seq) (list? (cdr seq))) (else #f))
                                if (
                                    // (null? seq)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    Scm::True
                                } else {
                                    if (
                                        // (pair? seq)
                                        imports::pair_p
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (list? (cdr seq))
                                        globals::list_p.with(|value| value.get()).invoke(&[
                                            // (cdr seq)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    } else {
                                        Scm::False
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (length seq) (fold-left (lambda (acc _) (+ acc 1)) 0 seq))
                globals::length.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (fold-left (lambda (acc _) (+ acc 1)) 0 seq))
                            {
                                // (fold-left (lambda (acc _) (+ acc 1)) 0 seq)
                                globals::fold_minus_left.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 2 {
                                                panic!("invalid arity")
                                            }
                                            let acc = args[0].clone();
                                            let __ = args[1].clone();
                                            // (letrec () (+ acc 1))
                                            {
                                                // (+ acc 1)
                                                imports::_plus_
                                                    .with(|value| value.get())
                                                    .invoke(&[acc.clone(), Scm::from(1)])
                                            }
                                        })
                                    },
                                    Scm::from(0),
                                    seq.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (reverse seq) (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq))
                globals::reverse.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq))
                            {
                                // (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq)
                                globals::fold_minus_left.with(|value| value.get()).invoke(&[
                                    {
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 2 {
                                                panic!("invalid arity")
                                            }
                                            let acc = args[0].clone();
                                            let x = args[1].clone();
                                            // (letrec () (cons x acc))
                                            {
                                                // (cons x acc)
                                                imports::cons
                                                    .with(|value| value.get())
                                                    .invoke(&[x.clone(), acc.clone()])
                                            }
                                        })
                                    },
                                    Scm::Nil,
                                    seq.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (list-copy obj) (fold-right cons (quote ()) seq))
                globals::list_minus_copy.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let obj = args[0].clone();
                            // (letrec () (fold-right cons (quote ()) seq))
                            {
                                // (fold-right cons (quote ()) seq)
                                globals::fold_minus_right
                                    .with(|value| value.get())
                                    .invoke(&[
                                        imports::cons.with(|value| value.get()),
                                        Scm::Nil,
                                        globals::seq.with(|value| value.get()),
                                    ])
                            }
                        })
                    })
                });
                // (define (assq obj seq) (if (pair? seq) (if (eq? obj (caar seq)) (car seq) (assq obj (cdr seq))) #f))
                globals::assq.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let obj = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (if (pair? seq) (if (eq? obj (caar seq)) (car seq) (assq obj (cdr seq))) #f))
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
                                        // (eq? obj (caar seq))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
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
                                        // (assq obj (cdr seq))
                                        globals::assq.with(|value| value.get()).invoke(&[
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
                // (define (memq obj seq) (if (pair? seq) (if (eq? obj (car seq)) seq (memq obj (cdr seq))) #f))
                globals::memq.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let obj = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (if (pair? seq) (if (eq? obj (car seq)) seq (memq obj (cdr seq))) #f))
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
                                        // (eq? obj (car seq))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            obj.clone(),
                                            // (car seq)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        seq.clone()
                                    } else {
                                        // (memq obj (cdr seq))
                                        globals::memq.with(|value| value.get()).invoke(&[
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
                // (define (symbol=? s1 s2 . args) (_symbol=? s1 (cons s2 args)))
                globals::symbol_e__p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 2 {
                                panic!("not enough args")
                            }
                            let s1 = args[0].clone();
                            let s2 = args[1].clone();
                            let args_ = Scm::list(&args[2..]);
                            // (letrec () (_symbol=? s1 (cons s2 args)))
                            {
                                // (_symbol=? s1 (cons s2 args))
                                globals::__symbol_e__p.with(|value| value.get()).invoke(&[
                                    s1.clone(),
                                    // (cons s2 args)
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[s2.clone(), args_.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (_symbol=? s1 s*) (cond ((null? s*) #t) ((eq? s1 (car s)) (_symbol=? s1 (cdr s*))) (else #f)))
                globals::__symbol_e__p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let s1 = args[0].clone();
                            let s_star_ = args[1].clone();
                            // (letrec () (cond ((null? s*) #t) ((eq? s1 (car s)) (_symbol=? s1 (cdr s*))) (else #f)))
                            {
                                // (cond ((null? s*) #t) ((eq? s1 (car s)) (_symbol=? s1 (cdr s*))) (else #f))
                                if (
                                    // (null? s*)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[s_star_.clone()])
                                )
                                .is_true()
                                {
                                    Scm::True
                                } else {
                                    if (
                                        // (eq? s1 (car s))
                                        imports::eq_p.with(|value| value.get()).invoke(&[
                                            s1.clone(),
                                            // (car s)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[globals::s.with(|value| value.get())]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (_symbol=? s1 (cdr s*))
                                        globals::__symbol_e__p.with(|value| value.get()).invoke(&[
                                            s1.clone(),
                                            // (cdr s*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[s_star_.clone()]),
                                        ])
                                    } else {
                                        Scm::False
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (string=? s1 s2 . args) (_string=? s1 (cons s2 args)))
                globals::string_e__p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 2 {
                                panic!("not enough args")
                            }
                            let s1 = args[0].clone();
                            let s2 = args[1].clone();
                            let args_ = Scm::list(&args[2..]);
                            // (letrec () (_string=? s1 (cons s2 args)))
                            {
                                // (_string=? s1 (cons s2 args))
                                globals::__string_e__p.with(|value| value.get()).invoke(&[
                                    s1.clone(),
                                    // (cons s2 args)
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[s2.clone(), args_.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (_string=? s1 s*) (cond ((null? s*) #t) ((equal? s1 (car s*)) (_string=? s1 (cdr s*))) (else #f)))
                globals::__string_e__p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let s1 = args[0].clone();
                            let s_star_ = args[1].clone();
                            // (letrec () (cond ((null? s*) #t) ((equal? s1 (car s*)) (_string=? s1 (cdr s*))) (else #f)))
                            {
                                // (cond ((null? s*) #t) ((equal? s1 (car s*)) (_string=? s1 (cdr s*))) (else #f))
                                if (
                                    // (null? s*)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[s_star_.clone()])
                                )
                                .is_true()
                                {
                                    Scm::True
                                } else {
                                    if (
                                        // (equal? s1 (car s*))
                                        imports::equal_p.with(|value| value.get()).invoke(&[
                                            s1.clone(),
                                            // (car s*)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[s_star_.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (_string=? s1 (cdr s*))
                                        globals::__string_e__p.with(|value| value.get()).invoke(&[
                                            s1.clone(),
                                            // (cdr s*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[s_star_.clone()]),
                                        ])
                                    } else {
                                        Scm::False
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (for-each proc . seq*) (_for-each proc seq*))
                globals::for_minus_each.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 1 {
                                panic!("not enough args")
                            }
                            let proc = args[0].clone();
                            let seq_star_ = Scm::list(&args[1..]);
                            // (letrec () (_for-each proc seq*))
                            {
                                // (_for-each proc seq*)
                                globals::__for_minus_each
                                    .with(|value| value.get())
                                    .invoke(&[proc.clone(), seq_star_.clone()])
                            }
                        })
                    })
                });
                // (define (map func . seq*) (_map func seq*))
                globals::map.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 1 {
                                panic!("not enough args")
                            }
                            let func = args[0].clone();
                            let seq_star_ = Scm::list(&args[1..]);
                            // (letrec () (_map func seq*))
                            {
                                // (_map func seq*)
                                globals::__map
                                    .with(|value| value.get())
                                    .invoke(&[func.clone(), seq_star_.clone()])
                            }
                        })
                    })
                });
                // (define (_for-each proc seq*) (if (any? null? seq*) (quote ()) (begin (apply proc (map-1 car seq*)) (_for-each proc (map-1 cdr seq*)))))
                globals::__for_minus_each.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let proc = args[0].clone();
                            let seq_star_ = args[1].clone();
                            // (letrec () (if (any? null? seq*) (quote ()) (begin (apply proc (map-1 car seq*)) (_for-each proc (map-1 cdr seq*)))))
                            {
                                if (
                                    // (any? null? seq*)
                                    globals::any_p.with(|value| value.get()).invoke(&[
                                        imports::null_p.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ])
                                )
                                .is_true()
                                {
                                    Scm::Nil
                                } else {
                                    {
                                        // (apply proc (map-1 car seq*))
                                        imports::apply.with(|value| value.get()).invoke(&[
                                            proc.clone(),
                                            // (map-1 car seq*)
                                            globals::map_minus_1.with(|value| value.get()).invoke(
                                                &[
                                                    imports::car.with(|value| value.get()),
                                                    seq_star_.clone(),
                                                ],
                                            ),
                                        ]);
                                        // (_for-each proc (map-1 cdr seq*))
                                        globals::__for_minus_each.with(|value| value.get()).invoke(
                                            &[
                                                proc.clone(),
                                                // (map-1 cdr seq*)
                                                globals::map_minus_1
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        imports::cdr.with(|value| value.get()),
                                                        seq_star_.clone(),
                                                    ]),
                                            ],
                                        )
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (_map func seq*) (if (any? null? seq*) (quote ()) (cons (apply func (map-1 car seq*)) (_map func (map-1 cdr seq*)))))
                globals::__map.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let func = args[0].clone();
                            let seq_star_ = args[1].clone();
                            // (letrec () (if (any? null? seq*) (quote ()) (cons (apply func (map-1 car seq*)) (_map func (map-1 cdr seq*)))))
                            {
                                if (
                                    // (any? null? seq*)
                                    globals::any_p.with(|value| value.get()).invoke(&[
                                        imports::null_p.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ])
                                )
                                .is_true()
                                {
                                    Scm::Nil
                                } else {
                                    // (cons (apply func (map-1 car seq*)) (_map func (map-1 cdr seq*)))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        // (apply func (map-1 car seq*))
                                        imports::apply.with(|value| value.get()).invoke(&[
                                            func.clone(),
                                            // (map-1 car seq*)
                                            globals::map_minus_1.with(|value| value.get()).invoke(
                                                &[
                                                    imports::car.with(|value| value.get()),
                                                    seq_star_.clone(),
                                                ],
                                            ),
                                        ]),
                                        // (_map func (map-1 cdr seq*))
                                        globals::__map.with(|value| value.get()).invoke(&[
                                            func.clone(),
                                            // (map-1 cdr seq*)
                                            globals::map_minus_1.with(|value| value.get()).invoke(
                                                &[
                                                    imports::cdr.with(|value| value.get()),
                                                    seq_star_.clone(),
                                                ],
                                            ),
                                        ]),
                                    ])
                                }
                            }
                        })
                    })
                });
                // (define (all? pred seq) (cond ((null? seq) #t) ((pred (car seq)) (all? pred (cdr seq))) (else #f)))
                globals::all_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let pred = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (cond ((null? seq) #t) ((pred (car seq)) (all? pred (cdr seq))) (else #f)))
                            {
                                // (cond ((null? seq) #t) ((pred (car seq)) (all? pred (cdr seq))) (else #f))
                                if (
                                    // (null? seq)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    Scm::True
                                } else {
                                    if (
                                        // (pred (car seq))
                                        pred.clone().invoke(&[
                                            // (car seq)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (all? pred (cdr seq))
                                        globals::all_p.with(|value| value.get()).invoke(&[
                                            pred.clone(),
                                            // (cdr seq)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    } else {
                                        Scm::False
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (any? pred seq) (cond ((null? seq) #f) ((pred (car seq)) #t) (else (any? pred (cdr seq)))))
                globals::any_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let pred = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (cond ((null? seq) #f) ((pred (car seq)) #t) (else (any? pred (cdr seq)))))
                            {
                                // (cond ((null? seq) #f) ((pred (car seq)) #t) (else (any? pred (cdr seq))))
                                if (
                                    // (null? seq)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    Scm::False
                                } else {
                                    if (
                                        // (pred (car seq))
                                        pred.clone().invoke(&[
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
                                        // (any? pred (cdr seq))
                                        globals::any_p.with(|value| value.get()).invoke(&[
                                            pred.clone(),
                                            // (cdr seq)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (map-1 func seq) (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq))
                globals::map_minus_1.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let func = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq))
                            {
                                // (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq)
                                globals::fold_minus_right
                                    .with(|value| value.get())
                                    .invoke(&[
                                        {
                                            let func = func.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 2 {
                                                    panic!("invalid arity")
                                                }
                                                let x = args[0].clone();
                                                let acc = args[1].clone();
                                                // (letrec () (cons (func x) acc))
                                                {
                                                    // (cons (func x) acc)
                                                    imports::cons.with(|value| value.get()).invoke(
                                                        &[
                                                            // (func x)
                                                            func.clone().invoke(&[x.clone()]),
                                                            acc.clone(),
                                                        ],
                                                    )
                                                }
                                            })
                                        },
                                        Scm::Nil,
                                        seq.clone(),
                                    ])
                            }
                        })
                    })
                });
                // (define (fold-right op init seq) (if (null? seq) init (op (car seq) (fold-right op init (cdr seq)))))
                globals::fold_minus_right.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 3 {
                                panic!("invalid arity")
                            }
                            let op = args[0].clone();
                            let init = args[1].clone();
                            let seq = args[2].clone();
                            // (letrec () (if (null? seq) init (op (car seq) (fold-right op init (cdr seq)))))
                            {
                                if (
                                    // (null? seq)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    init.clone()
                                } else {
                                    // (op (car seq) (fold-right op init (cdr seq)))
                                    op.clone().invoke(&[
                                        // (car seq)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                        // (fold-right op init (cdr seq))
                                        globals::fold_minus_right.with(|value| value.get()).invoke(
                                            &[
                                                op.clone(),
                                                init.clone(),
                                                // (cdr seq)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[seq.clone()]),
                                            ],
                                        ),
                                    ])
                                }
                            }
                        })
                    })
                });
                // (define (fold-left op init seq) (if (null? seq) init (fold-left op (op init (car seq)) (cdr seq))))
                globals::fold_minus_left.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 3 {
                                panic!("invalid arity")
                            }
                            let op = args[0].clone();
                            let init = args[1].clone();
                            let seq = args[2].clone();
                            // (letrec () (if (null? seq) init (fold-left op (op init (car seq)) (cdr seq))))
                            {
                                if (
                                    // (null? seq)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    init.clone()
                                } else {
                                    // (fold-left op (op init (car seq)) (cdr seq))
                                    globals::fold_minus_left.with(|value| value.get()).invoke(&[
                                        op.clone(),
                                        // (op init (car seq))
                                        op.clone().invoke(&[
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
                                }
                            }
                        })
                    })
                })
            };
        }
    }
    pub mod cxr {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::scheme::base::exports::{caar, cadr, car, cdar, cddr, cdr};
        }

        pub mod exports {
            pub use super::globals::caaaar;
            pub use super::globals::caaadr;
            pub use super::globals::caaar;
            pub use super::globals::caadar;
            pub use super::globals::caaddr;
            pub use super::globals::caadr;
            pub use super::globals::cadaar;
            pub use super::globals::cadadr;
            pub use super::globals::cadar;
            pub use super::globals::caddar;
            pub use super::globals::cadddr;
            pub use super::globals::caddr;
            pub use super::globals::cdaaar;
            pub use super::globals::cdaadr;
            pub use super::globals::cdaar;
            pub use super::globals::cdadar;
            pub use super::globals::cdaddr;
            pub use super::globals::cdadr;
            pub use super::globals::cddaar;
            pub use super::globals::cddadr;
            pub use super::globals::cddar;
            pub use super::globals::cdddar;
            pub use super::globals::cddddr;
            pub use super::globals::cdddr;
        }

        mod globals {
            use sunny_core::{Mut, Scm};
            thread_local! {#[allow(non_upper_case_globals)] pub static cddddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cddadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cddaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddaar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaaar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cadddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cadadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cadaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadaar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caaddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caaadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caaaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaaar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cddar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cddar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cdaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cdaar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caddr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caddr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static cadar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL cadar"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caadr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caadr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static caaar: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL caaar"))}
        }

        thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

        pub fn initialize() {
            if INITIALIZED.with(|x| x.get()) {
                return;
            }
            INITIALIZED.with(|x| x.set(true));

            crate::scheme::base::initialize();
            {
                (/*NOP*/);
                // (define (caaar x) (car (caar x)))
                globals::caaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (car (caar x)))
                            {
                                // (car (caar x))
                                imports::car.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caadr x) (car (cadr x)))
                globals::caadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (car (cadr x)))
                            {
                                // (car (cadr x))
                                imports::car.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cadar x) (car (cdar x)))
                globals::cadar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (car (cdar x)))
                            {
                                // (car (cdar x))
                                imports::car.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caddr x) (car (cddr x)))
                globals::caddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (car (cddr x)))
                            {
                                // (car (cddr x))
                                imports::car.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdaar x) (cdr (caar x)))
                globals::cdaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdr (caar x)))
                            {
                                // (cdr (caar x))
                                imports::cdr.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdadr x) (cdr (cadr x)))
                globals::cdadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdr (cadr x)))
                            {
                                // (cdr (cadr x))
                                imports::cdr.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cddar x) (cdr (cdar x)))
                globals::cddar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdr (cdar x)))
                            {
                                // (cdr (cdar x))
                                imports::cdr.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdddr x) (cdr (cddr x)))
                globals::cdddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdr (cddr x)))
                            {
                                // (cdr (cddr x))
                                imports::cdr.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caaaar x) (caar (caar x)))
                globals::caaaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (caar (caar x)))
                            {
                                // (caar (caar x))
                                imports::caar.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caaadr x) (caar (cadr x)))
                globals::caaadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (caar (cadr x)))
                            {
                                // (caar (cadr x))
                                imports::caar.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caadar x) (caar (cdar x)))
                globals::caadar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (caar (cdar x)))
                            {
                                // (caar (cdar x))
                                imports::caar.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caaddr x) (caar (cddr x)))
                globals::caaddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (caar (cddr x)))
                            {
                                // (caar (cddr x))
                                imports::caar.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cadaar x) (cadr (caar x)))
                globals::cadaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cadr (caar x)))
                            {
                                // (cadr (caar x))
                                imports::cadr.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cadadr x) (cadr (cadr x)))
                globals::cadadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cadr (cadr x)))
                            {
                                // (cadr (cadr x))
                                imports::cadr.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (caddar x) (cadr (cdar x)))
                globals::caddar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cadr (cdar x)))
                            {
                                // (cadr (cdar x))
                                imports::cadr.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cadddr x) (cadr (cddr x)))
                globals::cadddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cadr (cddr x)))
                            {
                                // (cadr (cddr x))
                                imports::cadr.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdaaar x) (cdar (caar x)))
                globals::cdaaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdar (caar x)))
                            {
                                // (cdar (caar x))
                                imports::cdar.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdaadr x) (cdar (cadr x)))
                globals::cdaadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdar (cadr x)))
                            {
                                // (cdar (cadr x))
                                imports::cdar.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdadar x) (cdar (cdar x)))
                globals::cdadar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdar (cdar x)))
                            {
                                // (cdar (cdar x))
                                imports::cdar.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdaddr x) (cdar (cddr x)))
                globals::cdaddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cdar (cddr x)))
                            {
                                // (cdar (cddr x))
                                imports::cdar.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cddaar x) (cddr (caar x)))
                globals::cddaar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cddr (caar x)))
                            {
                                // (cddr (caar x))
                                imports::cddr.with(|value| value.get()).invoke(&[
                                    // (caar x)
                                    imports::caar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cddadr x) (cddr (cadr x)))
                globals::cddadr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cddr (cadr x)))
                            {
                                // (cddr (cadr x))
                                imports::cddr.with(|value| value.get()).invoke(&[
                                    // (cadr x)
                                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cdddar x) (cddr (cdar x)))
                globals::cdddar.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cddr (cdar x)))
                            {
                                // (cddr (cdar x))
                                imports::cddr.with(|value| value.get()).invoke(&[
                                    // (cdar x)
                                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (cddddr x) (cddr (cddr x)))
                globals::cddddr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (cddr (cddr x)))
                            {
                                // (cddr (cddr x))
                                imports::cddr.with(|value| value.get()).invoke(&[
                                    // (cddr x)
                                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()]),
                                ])
                            }
                        })
                    })
                })
            };
        }
    }
    pub mod file {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::native::file::exports::*;
        }

        pub mod exports {
            pub use super::imports::file_minus_exists_p;
            pub use super::imports::open_minus_input_minus_file;
            pub use super::imports::open_minus_output_minus_file;
        }

        mod globals {}

        pub fn initialize() {
            crate::native::file::initialize();
            (/*NOP*/);
        }
    }
    pub mod read {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::native::read::exports::*;
        }

        pub mod exports {
            pub use super::imports::read;
        }

        mod globals {}

        pub fn initialize() {
            crate::native::read::initialize();
            (/*NOP*/);
        }
    }
    pub mod write {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::native::write::exports::*;
        }

        pub mod exports {
            pub use super::imports::display;
            pub use super::imports::newline;
            pub use super::imports::write;
        }

        mod globals {}

        pub fn initialize() {
            crate::native::write::initialize();
            (/*NOP*/);
        }
    }
    pub mod process_context {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::native::process_context::exports::*;
        }

        pub mod exports {
            pub use super::imports::command_minus_line;
        }

        mod globals {}

        pub fn initialize() {
            crate::native::process_context::initialize();
            (/*NOP*/);
        }
    }
}
pub mod sunny {
    pub mod utils {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::scheme::base::exports::*;
        }

        pub mod exports {
            pub use super::globals::dotted_minus_list_p;
            pub use super::globals::last_minus_cdr;
            pub use super::globals::proper_minus_list_minus_part;
        }

        mod globals {
            use sunny_core::{Mut, Scm};
            thread_local! {#[allow(non_upper_case_globals)] pub static proper_minus_list_minus_part: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL proper-list-part"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static last_minus_cdr: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL last-cdr"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static dotted_minus_list_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL dotted-list?"))}
        }

        thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }

        pub fn initialize() {
            if INITIALIZED.with(|x| x.get()) {
                return;
            }
            INITIALIZED.with(|x| x.set(true));

            crate::scheme::base::initialize();
            {
                (/*NOP*/);
                // (define (dotted-list? seq) (not (null? (last-cdr seq))))
                globals::dotted_minus_list_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (not (null? (last-cdr seq))))
                            {
                                // (not (null? (last-cdr seq)))
                                imports::not.with(|value| value.get()).invoke(&[
                                    // (null? (last-cdr seq))
                                    imports::null_p.with(|value| value.get()).invoke(&[
                                        // (last-cdr seq)
                                        globals::last_minus_cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                    ]),
                                ])
                            }
                        })
                    })
                });
                // (define (last-cdr seq) (if (pair? seq) (last-cdr (cdr seq)) seq))
                globals::last_minus_cdr.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (if (pair? seq) (last-cdr (cdr seq)) seq))
                            {
                                if (
                                    // (pair? seq)
                                    imports::pair_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    // (last-cdr (cdr seq))
                                    globals::last_minus_cdr.with(|value| value.get()).invoke(&[
                                        // (cdr seq)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                    ])
                                } else {
                                    seq.clone()
                                }
                            }
                        })
                    })
                });
                // (define (proper-list-part seq) (if (pair? seq) (cons (car seq) (proper-list-part (cdr seq))) (quote ())))
                globals::proper_minus_list_minus_part.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let seq = args[0].clone();
                            // (letrec () (if (pair? seq) (cons (car seq) (proper-list-part (cdr seq))) (quote ())))
                            {
                                if (
                                    // (pair? seq)
                                    imports::pair_p
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                )
                                .is_true()
                                {
                                    // (cons (car seq) (proper-list-part (cdr seq)))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        // (car seq)
                                        imports::car
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                        // (proper-list-part (cdr seq))
                                        globals::proper_minus_list_minus_part
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
                })
            };
        }
    }
    pub mod translate {
        #[allow(unused_imports)]
        use sunny_core::{Mut, Scm};
        mod imports {
            pub use crate::scheme::base::exports::*;
            pub use crate::scheme::cxr::exports::{
                caadr, cadar, cadddr, caddr, cdadr, cddar, cdddr,
            };
            pub use crate::scheme::file::exports::{
                file_minus_exists_p, open_minus_input_minus_file, open_minus_output_minus_file,
            };
            pub use crate::scheme::read::exports::read;
            pub use crate::scheme::write::exports::*;
            pub use crate::sunny::utils::exports::*;
        }

        pub mod exports {
            pub use super::globals::scm_minus__g_ast;
        }

        mod globals {
            use sunny_core::{Mut, Scm};
            thread_local! {#[allow(non_upper_case_globals)] pub static writeln: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL writeln"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_do_star_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-do*"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_remove: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-remove"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-vararg-abstraction"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static boxify_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify-abstraction"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_setter_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-setter!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_getter_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-getter!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_mut_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-mut?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable"))}
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
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_append_minus_child_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-append-child!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_find_minus_child: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-find-child"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_set_minus_children_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-set-children!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_module_minus_tree_minus_leaf: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-module-tree-leaf"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_libobj: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-libobj"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_leaf_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-leaf?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_name: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-name"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-module-tree"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_children: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-children"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_module_minus_tree_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-module-tree-list"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static module_minus_tree_minus_insert_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL module-tree-insert!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_module_minus_tree_minus_node: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-module-tree-node"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_imported_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-imported?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static list_minus_find_minus_free_minus_vars: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL list-find-free-vars"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static transform_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL transform-list"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static print_minus_list: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL print-list"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-boxify"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_modules: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-modules"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rustify_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rustify-libname"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_gen_minus_global_minus_defs: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-gen-global-defs"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static global_minus_regular_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL global-regular?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static any: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL any"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static println: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL println"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rust_minus_block: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rust-block"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_remove_star_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-remove*"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_union: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-union"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_setter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-setter"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static rustify_minus_identifier: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL rustify-identifier"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_add: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-add"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_getter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-getter"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static print: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL print"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static importset_minus_libname: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL importset-libname"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static definition_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition?"))}
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
            thread_local! {#[allow(non_upper_case_globals)] pub static lookup: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL lookup"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_vararg_minus_abstraction: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-vararg-abstraction"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static scan_minus_out_minus_defines: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL scan-out-defines"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_scope: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-scope"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_boxed_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-boxed-env"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-args"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_null_minus_arg: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-null-arg"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-fixlet"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static adjoin_minus_local_minus_env: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL adjoin-local-env"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-application"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_args: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->args"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_regular_minus_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->regular-application"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_fixlet: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->fixlet"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_alternative: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-alternative"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_value: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-value"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static definition_minus_variable: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL definition-variable"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_assignment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-assignment"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static variable_minus_set_minus_mutable_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL variable-set-mutable!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_reference: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-reference"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static ensure_minus_var_i: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL ensure-var!"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_constant: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-constant"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_comment: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-comment"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_application: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->application"))}
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
            thread_local! {#[allow(non_upper_case_globals)] pub static atom_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL atom?"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->ast"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static get_minus_lib: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL get-lib"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static assoc: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assoc"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-sequence"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_export: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->export"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_library: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-library"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_nop: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-nop"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static library_minus_decls_minus__g_ast: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL library-decls->ast"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_set: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-set"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static filter: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL filter"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static make_minus_program: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL make-program"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sort: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sort"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_sequence: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->sequence"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static boxify: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL boxify"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static sexpr_minus__g_import: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL sexpr->import"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL append"))}
            thread_local! {#[allow(non_upper_case_globals)] pub static set_minus_add_star_: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL set-add*"))}
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
            crate::sunny::utils::initialize();
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
                                            imports::list
                                                .with(|value| value.get())
                                                .invoke(&[Scm::Nil]),
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
                // (define (program->ast exp*) (define global-env (make-global-env)) (define library-env (list (quote ()))) (define (process-imports exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))))) (process-imports exp* (quote ()) (make-set)))
                globals::program_minus__g_ast.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let exp_star_ = args[0].clone();
// (letrec ((global-env (make-global-env)) (library-env (list (quote ()))) (process-imports (lambda (exp* imports init) (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))))))) (process-imports exp* (quote ()) (make-set)))
{let global_minus_env = Scm::uninitialized().into_boxed();
let library_minus_env = Scm::uninitialized().into_boxed();
let process_minus_imports = Scm::uninitialized().into_boxed();
global_minus_env.set(
// (make-global-env)
globals::make_minus_global_minus_env.with(|value| value.get()).invoke(&[]));
library_minus_env.set(
// (list (quote ()))
imports::list.with(|value| value.get()).invoke(&[Scm::Nil, ]));
process_minus_imports.set({let library_minus_env = library_minus_env.clone();let process_minus_imports = process_minus_imports.clone();let global_minus_env = global_minus_env.clone();Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp_star_ = args[0].clone();let imports = args[1].clone();let init = args[2].clone();
// (letrec () (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env)))))))
{
// (cond ((import? (car exp*)) (register-libraries (import-libnames (car exp*)) library-env) (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))) (else (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))))
if (
// (import? (car exp*))
globals::import_p.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(), ]), ])).is_true() {{
// (register-libraries (import-libnames (car exp*)) library-env)
globals::register_minus_libraries.with(|value| value.get()).invoke(&[
// (import-libnames (car exp*))
globals::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(), ]), ]), library_minus_env.get(), ]);
// (process-imports (cdr exp*) (append imports (sexpr->import (cdar exp*) global-env)) (set-add* init (import-libnames (car exp*))))
process_minus_imports.get().invoke(&[
// (cdr exp*)
imports::cdr.with(|value| value.get()).invoke(&[exp_star_.clone(), ]), 
// (append imports (sexpr->import (cdar exp*) global-env))
globals::append.with(|value| value.get()).invoke(&[imports.clone(), 
// (sexpr->import (cdar exp*) global-env)
globals::sexpr_minus__g_import.with(|value| value.get()).invoke(&[
// (cdar exp*)
imports::cdar.with(|value| value.get()).invoke(&[exp_star_.clone(), ]), global_minus_env.get(), ]), ]), 
// (set-add* init (import-libnames (car exp*)))
globals::set_minus_add_star_.with(|value| value.get()).invoke(&[init.clone(), 
// (import-libnames (car exp*))
globals::import_minus_libnames.with(|value| value.get()).invoke(&[
// (car exp*)
imports::car.with(|value| value.get()).invoke(&[exp_star_.clone(), ]), ]), ]), ])}} else {
// (let* ((main (boxify (sexpr->sequence exp* global-env #f))) (globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))
{let [main, ] = [
// (boxify (sexpr->sequence exp* global-env #f))
globals::boxify.with(|value| value.get()).invoke(&[
// (sexpr->sequence exp* global-env #f)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[exp_star_.clone(), global_minus_env.get(), Scm::False, ]), ]), ];
// (let* ((globals (sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) (cdr global-env)))) (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))
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
imports::car.with(|value| value.get()).invoke(&[a.clone(), ]), ]), 
// (symbol->string (car b))
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[
// (car b)
imports::car.with(|value| value.get()).invoke(&[b.clone(), ]), ]), ])}})}, 
// (cdr global-env)
imports::cdr.with(|value| value.get()).invoke(&[global_minus_env.get(), ]), ]), ];
// (let* () (display library-env) (newline) (make-program globals imports init main (filter cdr (car library-env))))
{
// (display library-env)
imports::display.with(|value| value.get()).invoke(&[library_minus_env.get(), ]);
// (newline)
imports::newline.with(|value| value.get()).invoke(&[]);
// (make-program globals imports init main (filter cdr (car library-env)))
globals::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(), imports.clone(), init.clone(), main.clone(), 
// (filter cdr (car library-env))
globals::filter.with(|value| value.get()).invoke(&[imports::cdr.with(|value| value.get()), 
// (car library-env)
imports::car.with(|value| value.get()).invoke(&[library_minus_env.get(), ]), ]), ])}}}}}})});

// (process-imports exp* (quote ()) (make-set))
process_minus_imports.get().invoke(&[exp_star_.clone(), Scm::Nil, 
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[]), ])}})}));
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
                                        globals::make_minus_set
                                            .with(|value| value.get())
                                            .invoke(&[]),
                                        // (make-nop)
                                        globals::make_minus_nop
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
                                } else {
                                    if (
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
                                                globals::append.with(|value| value.get()).invoke(
                                                    &[
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
                                                    ],
                                                ),
                                            ])
                                    } else {
                                        if (
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
                                                        globals::set_minus_add_star_
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                init.clone(),
                                                                // (import-libnames (car exp*))
                                                                globals::import_minus_libnames
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (car exp*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ]),
                                                                    ]),
                                                            ]),
                                                        body.clone(),
                                                        global_minus_env.clone(),
                                                        library_minus_env.clone(),
                                                        // (append imports (sexpr->import (cdar exp*) global-env))
                                                        globals::append
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                imports.clone(),
                                                                // (sexpr->import (cdar exp*) global-env)
                                                                globals::sexpr_minus__g_import
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (cdar exp*)
                                                                        imports::cdar
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ]),
                                                                        global_minus_env.clone(),
                                                                    ]),
                                                            ]),
                                                        exports.clone(),
                                                    ])
                                            }
                                        } else {
                                            if (
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
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ]),
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
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (register-libraries libs library-env) (cond ((null? libs) (quote DONE)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env))))
                globals::register_minus_libraries.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let libs = args[0].clone();
                            let library_minus_env = args[1].clone();
                            // (letrec () (cond ((null? libs) (quote DONE)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env))))
                            {
                                // (cond ((null? libs) (quote DONE)) ((assoc (car libs) (car library-env)) (register-libraries (cdr libs) library-env)) (else (let* ((lib (get-lib (car libs))) (libast (if (library? lib) (library->ast (library-name lib) (library-decls lib) library-env) #f))) (set-car! library-env (cons (cons (car libs) libast) (car library-env)))) (register-libraries (cdr libs) library-env)))
                                if (
                                    // (null? libs)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[libs.clone()])
                                )
                                .is_true()
                                {
                                    Scm::symbol("DONE")
                                } else {
                                    if (
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
                                                    globals::get_minus_lib
                                                        .with(|value| value.get())
                                                        .invoke(&[
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
                                                                                .invoke(&[
                                                                                    libs.clone()
                                                                                ]),
                                                                            libast.clone(),
                                                                        ]),
                                                                    // (car library-env)
                                                                    imports::car
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            library_minus_env
                                                                                .clone(),
                                                                        ]),
                                                                ]),
                                                        ])
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
                            }
                        })
                    })
                });
                // (define (sexpr->ast exp env tail?) (if (atom? exp) (if (symbol? exp) (sexpr->reference exp env) (sexpr->constant exp env)) (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))))
                globals::sexpr_minus__g_ast.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let exp = args[0].clone();let env = args[1].clone();let tail_p = args[2].clone();
// (letrec () (if (atom? exp) (if (symbol? exp) (sexpr->reference exp env) (sexpr->constant exp env)) (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))))
{if (
// (atom? exp)
globals::atom_p.with(|value| value.get()).invoke(&[exp.clone(), ])).is_true() {if (
// (symbol? exp)
imports::symbol_p.with(|value| value.get()).invoke(&[exp.clone(), ])).is_true() {
// (sexpr->reference exp env)
globals::sexpr_minus__g_reference.with(|value| value.get()).invoke(&[exp.clone(), env.clone(), ])} else {
// (sexpr->constant exp env)
globals::sexpr_minus__g_constant.with(|value| value.get()).invoke(&[exp.clone(), env.clone(), ])}} else {
// (cond ((eq? (quote quote) (car exp)) (sexpr->constant (cadr exp) env)) ((eq? (quote set!) (car exp)) (sexpr->assignment (cadr exp) (caddr exp) env)) ((eq? (quote define) (car exp)) (wrap-sexpr exp (sexpr->definition exp env))) ((eq? (quote lambda) (car exp)) (sexpr->abstraction (cadr exp) (cddr exp) env)) ((eq? (quote begin) (car exp)) (sexpr->sequence (cdr exp) env tail?)) ((eq? (quote let) (car exp)) (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))) ((eq? (quote let*) (car exp)) (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))) ((eq? (quote letrec) (car exp)) (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))) ((eq? (quote if) (car exp)) (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)) ((eq? (quote cond) (car exp)) (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))) ((eq? (quote and) (car exp)) (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))) (else (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))))
if (
// (eq? (quote quote) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("quote"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (sexpr->constant (cadr exp) env)
globals::sexpr_minus__g_constant.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), ])} else {if (
// (eq? (quote set!) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("set!"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (sexpr->assignment (cadr exp) (caddr exp) env)
globals::sexpr_minus__g_assignment.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (caddr exp)
imports::caddr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), ])} else {if (
// (eq? (quote define) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("define"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->definition exp env))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->definition exp env)
globals::sexpr_minus__g_definition.with(|value| value.get()).invoke(&[exp.clone(), env.clone(), ]), ])} else {if (
// (eq? (quote lambda) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("lambda"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (sexpr->abstraction (cadr exp) (cddr exp) env)
globals::sexpr_minus__g_abstraction.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (cddr exp)
imports::cddr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), ])} else {if (
// (eq? (quote begin) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("begin"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (sexpr->sequence (cdr exp) env tail?)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[
// (cdr exp)
imports::cdr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ])} else {if (
// (eq? (quote let) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("let"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->scope-let (cadr exp) (cddr exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->scope-let (cadr exp) (cddr exp) env tail?)
globals::sexpr_minus__g_scope_minus_let.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (cddr exp)
imports::cddr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])} else {if (
// (eq? (quote let*) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("let*"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->scope-seq (cadr exp) (cddr exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->scope-seq (cadr exp) (cddr exp) env tail?)
globals::sexpr_minus__g_scope_minus_seq.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (cddr exp)
imports::cddr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])} else {if (
// (eq? (quote letrec) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("letrec"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->scope-rec (cadr exp) (cddr exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->scope-rec (cadr exp) (cddr exp) env tail?)
globals::sexpr_minus__g_scope_minus_rec.with(|value| value.get()).invoke(&[
// (cadr exp)
imports::cadr.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (cddr exp)
imports::cddr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])} else {if (
// (eq? (quote if) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("if"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (sexpr->alternative (if-condition exp) (if-consequence exp) (if-alternative exp) env tail?)
globals::sexpr_minus__g_alternative.with(|value| value.get()).invoke(&[
// (if-condition exp)
globals::if_minus_condition.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (if-consequence exp)
globals::if_minus_consequence.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (if-alternative exp)
globals::if_minus_alternative.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ])} else {if (
// (eq? (quote cond) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("cond"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->cond (cond-clauses exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->cond (cond-clauses exp) env tail?)
globals::sexpr_minus__g_cond.with(|value| value.get()).invoke(&[
// (cond-clauses exp)
globals::cond_minus_clauses.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])} else {if (
// (eq? (quote and) (car exp))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("and"), 
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), ])).is_true() {
// (wrap-sexpr exp (sexpr->and (cdr exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->and (cdr exp) env tail?)
globals::sexpr_minus__g_and.with(|value| value.get()).invoke(&[
// (cdr exp)
imports::cdr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])} else {
// (wrap-sexpr exp (sexpr->application (car exp) (cdr exp) env tail?))
globals::wrap_minus_sexpr.with(|value| value.get()).invoke(&[exp.clone(), 
// (sexpr->application (car exp) (cdr exp) env tail?)
globals::sexpr_minus__g_application.with(|value| value.get()).invoke(&[
// (car exp)
imports::car.with(|value| value.get()).invoke(&[exp.clone(), ]), 
// (cdr exp)
imports::cdr.with(|value| value.get()).invoke(&[exp.clone(), ]), env.clone(), tail_p.clone(), ]), ])}}}}}}}}}}}}}})}));
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
                                globals::make_minus_comment
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
                                globals::make_minus_constant
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
                                    globals::make_minus_reference
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
                                        globals::variable_minus_set_minus_mutable_i
                                            .with(|value| value.get())
                                            .invoke(&[var.clone()]);
                                        // (make-assignment name var val)
                                        globals::make_minus_assignment
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
                                                        .invoke(&[
                                                            value.clone(),
                                                            env.clone(),
                                                            Scm::False,
                                                        ]),
                                                ];
                                                // (let* () (make-assignment name var val))

                                                // (make-assignment name var val)
                                                globals::make_minus_assignment
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        name.clone(),
                                                        var.clone(),
                                                        val.clone(),
                                                    ])
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
                                                .invoke(&[
                                                    consequent.clone(),
                                                    env.clone(),
                                                    tail_p.clone(),
                                                ]),
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
                                            globals::make_minus_alternative
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
                                        globals::make_minus_application
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
                                            globals::make_minus_fixlet
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
                                    globals::make_minus_null_minus_arg
                                        .with(|value| value.get())
                                        .invoke(&[])
                                } else {
                                    // (make-args (sexpr->ast (car arg*) env #f) (sexpr->args (cdr arg*) env))
                                    globals::make_minus_args.with(|value| value.get()).invoke(&[
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
                                                    imports::cons.with(|value| value.get()).invoke(
                                                        &[
                                                            // (cdr bindings)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[bindings.clone()]),
                                                            body.clone(),
                                                        ],
                                                    ),
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
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
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
                                            globals::make_minus_scope
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
                                                        let local_minus_env =
                                                            local_minus_env.clone();
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
                                                globals::lookup.with(|value| value.get()).invoke(
                                                    &[
                                                        // (last-cdr param*)
                                                        imports::last_minus_cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[param_star_.clone()]),
                                                        local_minus_env.clone(),
                                                    ],
                                                ),
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
                                                        let local_minus_env =
                                                            local_minus_env.clone();
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
                globals::sexpr_minus__g_cond.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let clauses = args[0].clone();let env = args[1].clone();let tail_p = args[2].clone();
// (letrec () (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest)))))
{
// (cond ((null? clauses) (make-constant (quote *UNSPECIFIED*))) ((eq? (quote else) (cond-clause-condition (car clauses))) (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) ((pair? clauses) (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))))
if (
// (null? clauses)
imports::null_p.with(|value| value.get()).invoke(&[clauses.clone(), ])).is_true() {
// (make-constant (quote *UNSPECIFIED*))
globals::make_minus_constant.with(|value| value.get()).invoke(&[Scm::symbol("*UNSPECIFIED*"), ])} else {if (
// (eq? (quote else) (cond-clause-condition (car clauses)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("else"), 
// (cond-clause-condition (car clauses))
globals::cond_minus_clause_minus_condition.with(|value| value.get()).invoke(&[
// (car clauses)
imports::car.with(|value| value.get()).invoke(&[clauses.clone(), ]), ]), ])).is_true() {
// (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[
// (cond-clause-sequence (car clauses))
globals::cond_minus_clause_minus_sequence.with(|value| value.get()).invoke(&[
// (car clauses)
imports::car.with(|value| value.get()).invoke(&[clauses.clone(), ]), ]), env.clone(), tail_p.clone(), ])} else {if (
// (pair? clauses)
imports::pair_p.with(|value| value.get()).invoke(&[clauses.clone(), ])).is_true() {
// (let* ((condition (sexpr->ast (cond-clause-condition (car clauses)) env #f)) (sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
{let [condition, ] = [
// (sexpr->ast (cond-clause-condition (car clauses)) env #f)
globals::sexpr_minus__g_ast.with(|value| value.get()).invoke(&[
// (cond-clause-condition (car clauses))
globals::cond_minus_clause_minus_condition.with(|value| value.get()).invoke(&[
// (car clauses)
imports::car.with(|value| value.get()).invoke(&[clauses.clone(), ]), ]), env.clone(), Scm::False, ]), ];
// (let* ((sequence (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)) (rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
{let [sequence, ] = [
// (sexpr->sequence (cond-clause-sequence (car clauses)) env tail?)
globals::sexpr_minus__g_sequence.with(|value| value.get()).invoke(&[
// (cond-clause-sequence (car clauses))
globals::cond_minus_clause_minus_sequence.with(|value| value.get()).invoke(&[
// (car clauses)
imports::car.with(|value| value.get()).invoke(&[clauses.clone(), ]), ]), env.clone(), tail_p.clone(), ]), ];
// (let* ((rest (sexpr->cond (cdr clauses) env tail?))) (make-alternative condition sequence rest))
{let [rest, ] = [
// (sexpr->cond (cdr clauses) env tail?)
globals::sexpr_minus__g_cond.with(|value| value.get()).invoke(&[
// (cdr clauses)
imports::cdr.with(|value| value.get()).invoke(&[clauses.clone(), ]), env.clone(), tail_p.clone(), ]), ];
// (let* () (make-alternative condition sequence rest))

// (make-alternative condition sequence rest)
globals::make_minus_alternative.with(|value| value.get()).invoke(&[condition.clone(), sequence.clone(), rest.clone(), ])}}}} else {Scm::symbol("*UNSPECIFIED*")}}}}})}));
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
                                    globals::make_minus_constant
                                        .with(|value| value.get())
                                        .invoke(&[Scm::True])
                                } else {
                                    if (
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
                                        globals::make_minus_alternative
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
                                                globals::make_minus_constant
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::False]),
                                            ])
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (sexpr->import stmt* env) (cond ((null? stmt*) (quote ())) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
                globals::sexpr_minus__g_import.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let stmt_star_ = args[0].clone();
                            let env = args[1].clone();
                            // (letrec () (cond ((null? stmt*) (quote ())) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env)))))
                            {
                                // (cond ((null? stmt*) (quote ())) ((eq? (quote only) (caar stmt*)) (cons (import-only (cadar stmt*) (cddar stmt*) env) (sexpr->import (cdr stmt*) env))) (else (cons (import-all (car stmt*) env) (sexpr->import (cdr stmt*) env))))
                                if (
                                    // (null? stmt*)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[stmt_star_.clone()])
                                )
                                .is_true()
                                {
                                    Scm::Nil
                                } else {
                                    if (
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
                // (define (get-lib lib) (let ((full-path (find-library (quote ("." "./lib" "scm-libs" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib))))
                globals::get_minus_lib.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let lib = args[0].clone();
                            // (letrec () (let ((full-path (find-library (quote ("." "./lib" "scm-libs" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib))))
                            {
                                // (let ((full-path (find-library (quote ("." "./lib" "scm-libs" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx"))))) (if full-path (read (open-input-file full-path)) (error "Unknown library" lib)))
                                {
                                    let [full_minus_path] = [
                                        // (find-library (quote ("." "./lib" "scm-libs" "../scm-libs" "../../scm-libs")) (library-path lib) (quote (".sld" ".slx")))
                                        globals::find_minus_library
                                            .with(|value| value.get())
                                            .invoke(&[
                                                Scm::pair(
                                                    Scm::from("."),
                                                    Scm::pair(
                                                        Scm::from("./lib"),
                                                        Scm::pair(
                                                            Scm::from("scm-libs"),
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
                                                    .invoke(&[
                                                        path.clone(),
                                                        extension_star_.clone(),
                                                    ]),
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
                                                            .invoke(&[
                                                                base_minus_path_star_.clone()
                                                            ]),
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
                                                            .invoke(&[
                                                                Scm::from("/"),
                                                                right.clone(),
                                                            ]),
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
                                } else {
                                    if (
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
                // (define (import-libnames exp*) (map importset-libname (cdr exp*)))
                globals::import_minus_libnames.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let exp_star_ = args[0].clone();
                            // (letrec () (map importset-libname (cdr exp*)))
                            {
                                // (map importset-libname (cdr exp*))
                                imports::map.with(|value| value.get()).invoke(&[
                                    globals::importset_minus_libname.with(|value| value.get()),
                                    // (cdr exp*)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[exp_star_.clone()]),
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
                                } else {
                                    if (
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
                                } else {
                                    if (
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
                                            } else {
                                                if (
                                                    // (definition? (car exp*))
                                                    globals::definition_p
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (car exp*)
                                                            imports::car
                                                                .with(|value| value.get())
                                                                .invoke(&[exp_star_.clone()]),
                                                        ])
                                                )
                                                .is_true()
                                                {
                                                    // (cons (list (definition-variable (car exp*)) (definition-value (car exp*))) (initializations (cdr exp*)))
                                                    imports::cons
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                        // (list (definition-variable (car exp*)) (definition-value (car exp*)))
                                                        imports::list
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (definition-variable (car exp*))
                                                                globals::definition_minus_variable
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (car exp*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ]),
                                                                    ]),
                                                                // (definition-value (car exp*))
                                                                globals::definition_minus_value
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        // (car exp*)
                                                                        imports::car
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                exp_star_.clone()
                                                                            ]),
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
                                            } else {
                                                if (
                                                    // (definition? (car exp*))
                                                    globals::definition_p
                                                        .with(|value| value.get())
                                                        .invoke(&[
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
                                                    imports::cons.with(|value| value.get()).invoke(
                                                        &[
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
                                                        ],
                                                    )
                                                }
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
                // (define (make-comment comment node) (define (repr) (cons (quote COMMENT) (cons comment (node (quote repr))))) (define (transform func) (func self (lambda () (make-comment comment (node (quote transform) func))))) (define (free-vars) (node (quote free-vars))) (define (gen-rust port) (newline port) (display "// " port) (write comment port) (newline port) (node (quote gen-rust) port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))) self)
                globals::make_minus_comment.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let comment = args[0].clone();let node = args[1].clone();
// (letrec ((repr (lambda () (cons (quote COMMENT) (cons comment (node (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-comment comment (node (quote transform) func)))))) (free-vars (lambda () (node (quote free-vars)))) (gen-rust (lambda (port) (newline port) (display "// " port) (write comment port) (newline port) (node (quote gen-rust) port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote COMMENT) (cons comment (node (quote repr)))))
{
// (cons (quote COMMENT) (cons comment (node (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("COMMENT"), 
// (cons comment (node (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[comment.clone(), 
// (node (quote repr))
node.clone().invoke(&[Scm::symbol("repr"), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-comment comment (node (quote transform) func)))))
{
// (func self (lambda () (make-comment comment (node (quote transform) func))))
func.clone().invoke(&[self_.get(), {let comment = comment.clone();let node = node.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-comment comment (node (quote transform) func)))
{
// (make-comment comment (node (quote transform) func))
globals::make_minus_comment.with(|value| value.get()).invoke(&[comment.clone(), 
// (node (quote transform) func)
node.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (node (quote free-vars)))
{
// (node (quote free-vars))
node.clone().invoke(&[Scm::symbol("free-vars"), ])}})});
gen_minus_rust.set({let comment = comment.clone();let node = node.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (newline port) (display "// " port) (write comment port) (newline port) (node (quote gen-rust) port))
{{
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (display "// " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("// "), port.clone(), ]);
// (write comment port)
imports::write.with(|value| value.get()).invoke(&[comment.clone(), port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (node (quote gen-rust) port)
node.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote COMMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message COMMENT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("COMMENT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message COMMENT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message COMMENT"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-nop) (define (repr) (quote (NOP))) (define (transform func) (func self (lambda () self))) (define (free-vars) (make-set)) (define (gen-rust port) (display "(/*NOP*/)" port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))) self)
                globals::make_minus_nop.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 0 {
                                panic!("invalid arity")
                            }
                            // (letrec ((repr (lambda () (quote (NOP)))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (port) (display "(/*NOP*/)" port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
                            {
                                let repr = Scm::uninitialized().into_boxed();
                                let transform = Scm::uninitialized().into_boxed();
                                let free_minus_vars = Scm::uninitialized().into_boxed();
                                let gen_minus_rust = Scm::uninitialized().into_boxed();
                                let self_ = Scm::uninitialized().into_boxed();
                                repr.set({
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 0 {
                                            panic!("invalid arity")
                                        }
                                        // (letrec () (quote (NOP)))
                                        {
                                            Scm::pair(Scm::symbol("NOP"), Scm::Nil)
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
                                free_minus_vars.set({
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 0 {
                                            panic!("invalid arity")
                                        }
                                        // (letrec () (make-set))
                                        {
                                            // (make-set)
                                            globals::make_minus_set
                                                .with(|value| value.get())
                                                .invoke(&[])
                                        }
                                    })
                                });
                                gen_minus_rust.set({
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let port = args[0].clone();
                                        // (letrec () (display "(/*NOP*/)" port))
                                        {
                                            // (display "(/*NOP*/)" port)
                                            imports::display
                                                .with(|value| value.get())
                                                .invoke(&[Scm::from("(/*NOP*/)"), port.clone()])
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
                                        // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))
                                        {
                                            // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))
                                            if (
                                                // (eq? (quote repr) msg)
                                                imports::eq_p
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::symbol("repr"), msg.clone()])
                                            )
                                            .is_true()
                                            {
                                                // (print)
                                                globals::print.with(|value| value.get()).invoke(&[])
                                            } else {
                                                if (
                                                    // (eq? (quote transform) msg)
                                                    imports::eq_p.with(|value| value.get()).invoke(
                                                        &[Scm::symbol("transform"), msg.clone()],
                                                    )
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
                                                } else {
                                                    if (
                                                        // (eq? (quote free-vars) msg)
                                                        imports::eq_p
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                    )
                                                    .is_true()
                                                    {
                                                        // (free-vars)
                                                        free_minus_vars.get().invoke(&[])
                                                    } else {
                                                        if (
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    Scm::symbol("kind"),
                                                                    msg.clone(),
                                                                ])
                                                        )
                                                        .is_true()
                                                        {
                                                            Scm::symbol("NOP")
                                                        } else {
                                                            if (
                                                                // (eq? (quote gen-rust) msg)
                                                                imports::eq_p
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        Scm::symbol("gen-rust"),
                                                                        msg.clone(),
                                                                    ])
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
                                                                // (error "Unknown message NOP" msg)
                                                                imports::error
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        Scm::from(
                                                                            "Unknown message NOP",
                                                                        ),
                                                                        msg.clone(),
                                                                    ])
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    })
                                });
                                self_.get()
                            }
                        })
                    })
                });
                // (define (make-constant val) (define (repr) (cons (quote CONSTANT) val)) (define (transform func) (func self (lambda () self))) (define (free-vars) (make-set)) (define (gen-constant port val) (cond ((null? val) (display "Scm::Nil" port)) ((eq? val #t) (display "Scm::True" port)) ((eq? val #f) (display "Scm::False" port)) ((symbol? val) (print port "Scm::symbol(\"" val "\")")) ((char? val) (print port "Scm::char('" val "')")) ((pair? val) (display "Scm::pair(" port) (gen-constant port (car val)) (display ", " port) (gen-constant port (cdr val)) (display ")" port)) (else (display "Scm::from(" port) (write val port) (display ")" port)))) (define (gen-rust port) (gen-constant port val)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))) self)
                globals::make_minus_constant.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let val = args[0].clone();
// (letrec ((repr (lambda () (cons (quote CONSTANT) val))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-constant (lambda (port val) (cond ((null? val) (display "Scm::Nil" port)) ((eq? val #t) (display "Scm::True" port)) ((eq? val #f) (display "Scm::False" port)) ((symbol? val) (print port "Scm::symbol(\"" val "\")")) ((char? val) (print port "Scm::char('" val "')")) ((pair? val) (display "Scm::pair(" port) (gen-constant port (car val)) (display ", " port) (gen-constant port (cdr val)) (display ")" port)) (else (display "Scm::from(" port) (write val port) (display ")" port))))) (gen-rust (lambda (port) (gen-constant port val))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_constant = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote CONSTANT) val))
{
// (cons (quote CONSTANT) val)
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("CONSTANT"), val.clone(), ])}})});
transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () self)))
{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(), {let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () self)
{self_.get()}})}, ])}})});
free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-set))
{
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])}})});
gen_minus_constant.set({let gen_minus_constant = gen_minus_constant.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let val = args[1].clone();
// (letrec () (cond ((null? val) (display "Scm::Nil" port)) ((eq? val #t) (display "Scm::True" port)) ((eq? val #f) (display "Scm::False" port)) ((symbol? val) (print port "Scm::symbol(\"" val "\")")) ((char? val) (print port "Scm::char('" val "')")) ((pair? val) (display "Scm::pair(" port) (gen-constant port (car val)) (display ", " port) (gen-constant port (cdr val)) (display ")" port)) (else (display "Scm::from(" port) (write val port) (display ")" port))))
{
// (cond ((null? val) (display "Scm::Nil" port)) ((eq? val #t) (display "Scm::True" port)) ((eq? val #f) (display "Scm::False" port)) ((symbol? val) (print port "Scm::symbol(\"" val "\")")) ((char? val) (print port "Scm::char('" val "')")) ((pair? val) (display "Scm::pair(" port) (gen-constant port (car val)) (display ", " port) (gen-constant port (cdr val)) (display ")" port)) (else (display "Scm::from(" port) (write val port) (display ")" port)))
if (
// (null? val)
imports::null_p.with(|value| value.get()).invoke(&[val.clone(), ])).is_true() {
// (display "Scm::Nil" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::Nil"), port.clone(), ])} else {if (
// (eq? val #t)
imports::eq_p.with(|value| value.get()).invoke(&[val.clone(), Scm::True, ])).is_true() {
// (display "Scm::True" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::True"), port.clone(), ])} else {if (
// (eq? val #f)
imports::eq_p.with(|value| value.get()).invoke(&[val.clone(), Scm::False, ])).is_true() {
// (display "Scm::False" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::False"), port.clone(), ])} else {if (
// (symbol? val)
imports::symbol_p.with(|value| value.get()).invoke(&[val.clone(), ])).is_true() {
// (print port "Scm::symbol(\"" val "\")")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("Scm::symbol(\""), val.clone(), Scm::from("\")"), ])} else {if (
// (char? val)
imports::char_p.with(|value| value.get()).invoke(&[val.clone(), ])).is_true() {
// (print port "Scm::char('" val "')")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("Scm::char('"), val.clone(), Scm::from("')"), ])} else {if (
// (pair? val)
imports::pair_p.with(|value| value.get()).invoke(&[val.clone(), ])).is_true() {{
// (display "Scm::pair(" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::pair("), port.clone(), ]);
// (gen-constant port (car val))
gen_minus_constant.get().invoke(&[port.clone(), 
// (car val)
imports::car.with(|value| value.get()).invoke(&[val.clone(), ]), ]);
// (display ", " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(", "), port.clone(), ]);
// (gen-constant port (cdr val))
gen_minus_constant.get().invoke(&[port.clone(), 
// (cdr val)
imports::cdr.with(|value| value.get()).invoke(&[val.clone(), ]), ]);
// (display ")" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(")"), port.clone(), ])}} else {{
// (display "Scm::from(" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::from("), port.clone(), ]);
// (write val port)
imports::write.with(|value| value.get()).invoke(&[val.clone(), port.clone(), ]);
// (display ")" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(")"), port.clone(), ])}}}}}}}}})});
gen_minus_rust.set({let gen_minus_constant = gen_minus_constant.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (gen-constant port val))
{
// (gen-constant port val)
gen_minus_constant.get().invoke(&[port.clone(), val.clone(), ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote CONSTANT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message CONSTANT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("CONSTANT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message CONSTANT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message CONSTANT"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-reference name var) (define (global?) (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var)))) (define (repr) (list (variable-getter var) name)) (define (transform func) (func self (lambda () self))) (define (free-vars) (if (global?) (make-set) (set-add (make-set) name))) (define (gen-rust port) (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print port (rustify-identifier name) ".get()")) (else (print port (rustify-identifier name) ".clone()"))))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))) self)
                globals::make_minus_reference.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();
// (letrec ((global? (lambda () (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var))))) (repr (lambda () (list (variable-getter var) name))) (transform (lambda (func) (func self (lambda () self)))) (free-vars (lambda () (if (global?) (make-set) (set-add (make-set) name)))) (gen-rust (lambda (port) (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print port (rustify-identifier name) ".get()")) (else (print port (rustify-identifier name) ".clone()")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))))) self)
{let global_p = Scm::uninitialized().into_boxed();
let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
global_p.set({let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (eq? (quote GLOBAL-REF) (variable-getter var)) #t (eq? (quote IMPORT-REF) (variable-getter var))))
{if (
// (eq? (quote GLOBAL-REF) (variable-getter var))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("GLOBAL-REF"), 
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), ])).is_true() {Scm::True} else {
// (eq? (quote IMPORT-REF) (variable-getter var))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT-REF"), 
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), ])}}})});
repr.set({let var = var.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (variable-getter var) name))
{
// (list (variable-getter var) name)
imports::list.with(|value| value.get()).invoke(&[
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), name.clone(), ])}})});
transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () self)))
{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(), {let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () self)
{self_.get()}})}, ])}})});
free_minus_vars.set({let global_p = global_p.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (global?) (make-set) (set-add (make-set) name)))
{if (
// (global?)
global_p.get().invoke(&[])).is_true() {
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])} else {
// (set-add (make-set) name)
globals::set_minus_add.with(|value| value.get()).invoke(&[
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[]), name.clone(), ])}}})});
gen_minus_rust.set({let name = name.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print port (rustify-identifier name) ".get()")) (else (print port (rustify-identifier name) ".clone()")))))
{
// (let ((getter (variable-getter var))) (cond ((eq? (quote GLOBAL-REF) getter) (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print port (rustify-identifier name) ".get()")) (else (print port (rustify-identifier name) ".clone()"))))
{let [getter, ] = [
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), ];
// (cond ((eq? (quote GLOBAL-REF) getter) (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote IMPORT-REF) getter) (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")) ((eq? (quote BOXED-REF) getter) (print port (rustify-identifier name) ".get()")) (else (print port (rustify-identifier name) ".clone()")))
if (
// (eq? (quote GLOBAL-REF) getter)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("GLOBAL-REF"), getter.clone(), ])).is_true() {
// (print port "globals::" (rustify-identifier name) ".with(|value| value.get())")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("globals::"), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".with(|value| value.get())"), ])} else {if (
// (eq? (quote IMPORT-REF) getter)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT-REF"), getter.clone(), ])).is_true() {
// (print port "imports::" (rustify-identifier name) ".with(|value| value.get())")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("imports::"), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".with(|value| value.get())"), ])} else {if (
// (eq? (quote BOXED-REF) getter)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("BOXED-REF"), getter.clone(), ])).is_true() {
// (print port (rustify-identifier name) ".get()")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".get()"), ])} else {
// (print port (rustify-identifier name) ".clone()")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".clone()"), ])}}}}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote REFERENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message REFERENCE" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("REFERENCE")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message REFERENCE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message REFERENCE"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-assignment name var val) (define (repr) (list (variable-setter var) name (val (quote repr)))) (define (transform func) (func self (lambda () (make-assignment name var (val (quote transform) func))))) (define (free-vars) (set-add (val (quote free-vars)) name)) (define (gen-rust port) (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print port "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) port) (print port "))")) ((eq? (quote BOXED-SET) setter) (print port (rustify-identifier name) ".set(") (val (quote gen-rust) port) (print port ")")) (else (error "set! on unboxed variable"))))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))) self)
                globals::make_minus_assignment.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let name = args[0].clone();let var = args[1].clone();let val = args[2].clone();
// (letrec ((repr (lambda () (list (variable-setter var) name (val (quote repr))))) (transform (lambda (func) (func self (lambda () (make-assignment name var (val (quote transform) func)))))) (free-vars (lambda () (set-add (val (quote free-vars)) name))) (gen-rust (lambda (port) (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print port "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) port) (print port "))")) ((eq? (quote BOXED-SET) setter) (print port (rustify-identifier name) ".set(") (val (quote gen-rust) port) (print port ")")) (else (error "set! on unboxed variable")))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let var = var.clone();let name = name.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (variable-setter var) name (val (quote repr))))
{
// (list (variable-setter var) name (val (quote repr)))
imports::list.with(|value| value.get()).invoke(&[
// (variable-setter var)
globals::variable_minus_setter.with(|value| value.get()).invoke(&[var.clone(), ]), name.clone(), 
// (val (quote repr))
val.clone().invoke(&[Scm::symbol("repr"), ]), ])}})});
transform.set({let self_ = self_.clone();let name = name.clone();let var = var.clone();let val = val.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-assignment name var (val (quote transform) func)))))
{
// (func self (lambda () (make-assignment name var (val (quote transform) func))))
func.clone().invoke(&[self_.get(), {let name = name.clone();let var = var.clone();let val = val.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-assignment name var (val (quote transform) func)))
{
// (make-assignment name var (val (quote transform) func))
globals::make_minus_assignment.with(|value| value.get()).invoke(&[name.clone(), var.clone(), 
// (val (quote transform) func)
val.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let val = val.clone();let name = name.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-add (val (quote free-vars)) name))
{
// (set-add (val (quote free-vars)) name)
globals::set_minus_add.with(|value| value.get()).invoke(&[
// (val (quote free-vars))
val.clone().invoke(&[Scm::symbol("free-vars"), ]), name.clone(), ])}})});
gen_minus_rust.set({let name = name.clone();let val = val.clone();let var = var.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print port "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) port) (print port "))")) ((eq? (quote BOXED-SET) setter) (print port (rustify-identifier name) ".set(") (val (quote gen-rust) port) (print port ")")) (else (error "set! on unboxed variable")))))
{
// (let ((setter (variable-setter var))) (cond ((eq? (quote GLOBAL-SET) setter) (print port "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) port) (print port "))")) ((eq? (quote BOXED-SET) setter) (print port (rustify-identifier name) ".set(") (val (quote gen-rust) port) (print port ")")) (else (error "set! on unboxed variable"))))
{let [setter, ] = [
// (variable-setter var)
globals::variable_minus_setter.with(|value| value.get()).invoke(&[var.clone(), ]), ];
// (cond ((eq? (quote GLOBAL-SET) setter) (print port "globals::" (rustify-identifier name) ".with(|value| value.set(") (val (quote gen-rust) port) (print port "))")) ((eq? (quote BOXED-SET) setter) (print port (rustify-identifier name) ".set(") (val (quote gen-rust) port) (print port ")")) (else (error "set! on unboxed variable")))
if (
// (eq? (quote GLOBAL-SET) setter)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("GLOBAL-SET"), setter.clone(), ])).is_true() {{
// (print port "globals::" (rustify-identifier name) ".with(|value| value.set(")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("globals::"), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".with(|value| value.set("), ]);
// (val (quote gen-rust) port)
val.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (print port "))")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from("))"), ])}} else {if (
// (eq? (quote BOXED-SET) setter)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("BOXED-SET"), setter.clone(), ])).is_true() {{
// (print port (rustify-identifier name) ".set(")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(".set("), ]);
// (val (quote gen-rust) port)
val.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (print port ")")
globals::print.with(|value| value.get()).invoke(&[port.clone(), Scm::from(")"), ])}} else {
// (error "set! on unboxed variable")
imports::error.with(|value| value.get()).invoke(&[Scm::from("set! on unboxed variable"), ])}}}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ASSIGNMENT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ASSIGNMENT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("ASSIGNMENT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message ASSIGNMENT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ASSIGNMENT"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-alternative condition consequent alternative) (define (repr) (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))) (define (transform func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))) (define (free-vars) (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))) (define (gen-rust port) (display "if (" port) (condition (quote gen-rust) port) (display ").is_true() {" port) (consequent (quote gen-rust) port) (display "} else {" port) (alternative (quote gen-rust) port) (display "}" port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))) self)
                globals::make_minus_alternative.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let condition = args[0].clone();let consequent = args[1].clone();let alternative = args[2].clone();
// (letrec ((repr (lambda () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))) (transform (lambda (func) (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))) (free-vars (lambda () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))) (gen-rust (lambda (port) (display "if (" port) (condition (quote gen-rust) port) (display ").is_true() {" port) (consequent (quote gen-rust) port) (display "} else {" port) (alternative (quote gen-rust) port) (display "}" port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr))))
{
// (list (quote IF) (condition (quote repr)) (consequent (quote repr)) (alternative (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("IF"), 
// (condition (quote repr))
condition.clone().invoke(&[Scm::symbol("repr"), ]), 
// (consequent (quote repr))
consequent.clone().invoke(&[Scm::symbol("repr"), ]), 
// (alternative (quote repr))
alternative.clone().invoke(&[Scm::symbol("repr"), ]), ])}})});
transform.set({let self_ = self_.clone();let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))))
{
// (func self (lambda () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))))
func.clone().invoke(&[self_.get(), {let condition = condition.clone();let func = func.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func)))
{
// (make-alternative (condition (quote transform) func) (consequent (quote transform) func) (alternative (quote transform) func))
globals::make_minus_alternative.with(|value| value.get()).invoke(&[
// (condition (quote transform) func)
condition.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), 
// (consequent (quote transform) func)
consequent.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), 
// (alternative (quote transform) func)
alternative.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars))))
{
// (set-union (set-union (condition (quote free-vars)) (consequent (quote free-vars))) (alternative (quote free-vars)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (set-union (condition (quote free-vars)) (consequent (quote free-vars)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (condition (quote free-vars))
condition.clone().invoke(&[Scm::symbol("free-vars"), ]), 
// (consequent (quote free-vars))
consequent.clone().invoke(&[Scm::symbol("free-vars"), ]), ]), 
// (alternative (quote free-vars))
alternative.clone().invoke(&[Scm::symbol("free-vars"), ]), ])}})});
gen_minus_rust.set({let condition = condition.clone();let consequent = consequent.clone();let alternative = alternative.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "if (" port) (condition (quote gen-rust) port) (display ").is_true() {" port) (consequent (quote gen-rust) port) (display "} else {" port) (alternative (quote gen-rust) port) (display "}" port))
{{
// (display "if (" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("if ("), port.clone(), ]);
// (condition (quote gen-rust) port)
condition.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display ").is_true() {" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(").is_true() {"), port.clone(), ]);
// (consequent (quote gen-rust) port)
consequent.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display "} else {" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("} else {"), port.clone(), ]);
// (alternative (quote gen-rust) port)
alternative.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display "}" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("}"), port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ALTERNATIVE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ALTERNATIVE" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("ALTERNATIVE")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message ALTERNATIVE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ALTERNATIVE"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-application func args tail?) (define (repr) (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))) (define (transform fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))) (define (free-vars) (set-union (func (quote free-vars)) (args (quote free-vars)))) (define (gen-rust port) (func (quote gen-rust) port) (display ".invoke(&[" port) (args (quote gen-rust) port) (display "])" port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))) self)
                globals::make_minus_application.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let func = args[0].clone();let args_ = args[1].clone();let tail_p = args[2].clone();
// (letrec ((repr (lambda () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))) (transform (lambda (fnc) (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))) (free-vars (lambda () (set-union (func (quote free-vars)) (args (quote free-vars))))) (gen-rust (lambda (port) (func (quote gen-rust) port) (display ".invoke(&[" port) (args (quote gen-rust) port) (display "])" port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let tail_p = tail_p.clone();let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr)))))
{
// (cons (if tail? (quote APPLY-TC) (quote APPLY)) (cons (func (quote repr)) (args (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[if (tail_p.clone()).is_true() {Scm::symbol("APPLY-TC")} else {Scm::symbol("APPLY")}, 
// (cons (func (quote repr)) (args (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[
// (func (quote repr))
func.clone().invoke(&[Scm::symbol("repr"), ]), 
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr"), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let func = func.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();
// (letrec () (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))))
{
// (fnc self (lambda () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)))
fnc.clone().invoke(&[self_.get(), {let func = func.clone();let fnc = fnc.clone();let args_ = args_.clone();let tail_p = tail_p.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?))
{
// (make-application (func (quote transform) fnc) (args (quote transform) fnc) tail?)
globals::make_minus_application.with(|value| value.get()).invoke(&[
// (func (quote transform) fnc)
func.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ]), 
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ]), tail_p.clone(), ])}})}, ])}})});
free_minus_vars.set({let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-union (func (quote free-vars)) (args (quote free-vars))))
{
// (set-union (func (quote free-vars)) (args (quote free-vars)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (func (quote free-vars))
func.clone().invoke(&[Scm::symbol("free-vars"), ]), 
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars"), ]), ])}})});
gen_minus_rust.set({let func = func.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (func (quote gen-rust) port) (display ".invoke(&[" port) (args (quote gen-rust) port) (display "])" port))
{{
// (func (quote gen-rust) port)
func.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display ".invoke(&[" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(".invoke(&["), port.clone(), ]);
// (args (quote gen-rust) port)
args_.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display "])" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("])"), port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote APPLICATION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message APPLICATION" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("APPLICATION")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message APPLICATION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message APPLICATION"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-null-arg) (define (repr) (list (quote NULL-ARG))) (define (transform fnc) (fnc self (lambda () self))) (define (free-vars) (make-set)) (define (gen-rust port) (display "" port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))) self)
                globals::make_minus_null_minus_arg.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec ((repr (lambda () (list (quote NULL-ARG)))) (transform (lambda (fnc) (fnc self (lambda () self)))) (free-vars (lambda () (make-set))) (gen-rust (lambda (port) (display "" port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (quote NULL-ARG)))
{
// (list (quote NULL-ARG))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("NULL-ARG"), ])}})});
transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();
// (letrec () (fnc self (lambda () self)))
{
// (fnc self (lambda () self))
fnc.clone().invoke(&[self_.get(), {let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () self)
{self_.get()}})}, ])}})});
free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-set))
{
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])}})});
gen_minus_rust.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "" port))
{
// (display "" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(""), port.clone(), ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote NULL-ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NULL-ARG" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("NULL-ARG")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message NULL-ARG" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message NULL-ARG"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-args arg next) (define (repr) (cons (quote ARG) (cons arg next))) (define (transform fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))) (define (free-vars) (set-union (arg (quote free-vars)) (next (quote free-vars)))) (define (gen-rust port) (arg (quote gen-rust) port) (display ", " port) (next (quote gen-rust) port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))) self)
                globals::make_minus_args.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let arg = args[0].clone();
                            let next = args[1].clone();
                            // (letrec ((repr (lambda () (cons (quote ARG) (cons arg next)))) (transform (lambda (fnc) (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))) (free-vars (lambda () (set-union (arg (quote free-vars)) (next (quote free-vars))))) (gen-rust (lambda (port) (arg (quote gen-rust) port) (display ", " port) (next (quote gen-rust) port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))))) self)
                            {
                                let repr = Scm::uninitialized().into_boxed();
                                let transform = Scm::uninitialized().into_boxed();
                                let free_minus_vars = Scm::uninitialized().into_boxed();
                                let gen_minus_rust = Scm::uninitialized().into_boxed();
                                let self_ = Scm::uninitialized().into_boxed();
                                repr.set({
                                    let arg = arg.clone();
                                    let next = next.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 0 {
                                            panic!("invalid arity")
                                        }
                                        // (letrec () (cons (quote ARG) (cons arg next)))
                                        {
                                            // (cons (quote ARG) (cons arg next))
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                Scm::symbol("ARG"),
                                                // (cons arg next)
                                                imports::cons
                                                    .with(|value| value.get())
                                                    .invoke(&[arg.clone(), next.clone()]),
                                            ])
                                        }
                                    })
                                });
                                transform.set({
                                    let self_ = self_.clone();
                                    let arg = arg.clone();
                                    let next = next.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let fnc = args[0].clone();
                                        // (letrec () (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))))
                                        {
                                            // (fnc self (lambda () (make-args (arg (quote transform) fnc) (next (quote transform) fnc))))
                                            fnc.clone().invoke(&[self_.get(), {
                                                let arg = arg.clone();
                                                let fnc = fnc.clone();
                                                let next = next.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 0 {
                                                        panic!("invalid arity")
                                                    }
                                                    // (letrec () (make-args (arg (quote transform) fnc) (next (quote transform) fnc)))
                                                    {
                                                        // (make-args (arg (quote transform) fnc) (next (quote transform) fnc))
                                                        globals::make_minus_args
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                // (arg (quote transform) fnc)
                                                                arg.clone().invoke(&[
                                                                    Scm::symbol("transform"),
                                                                    fnc.clone(),
                                                                ]),
                                                                // (next (quote transform) fnc)
                                                                next.clone().invoke(&[
                                                                    Scm::symbol("transform"),
                                                                    fnc.clone(),
                                                                ]),
                                                            ])
                                                    }
                                                })
                                            }])
                                        }
                                    })
                                });
                                free_minus_vars.set({
                                    let arg = arg.clone();
                                    let next = next.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 0 {
                                            panic!("invalid arity")
                                        }
                                        // (letrec () (set-union (arg (quote free-vars)) (next (quote free-vars))))
                                        {
                                            // (set-union (arg (quote free-vars)) (next (quote free-vars)))
                                            globals::set_minus_union
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (arg (quote free-vars))
                                                    arg.clone().invoke(&[Scm::symbol("free-vars")]),
                                                    // (next (quote free-vars))
                                                    next.clone()
                                                        .invoke(&[Scm::symbol("free-vars")]),
                                                ])
                                        }
                                    })
                                });
                                gen_minus_rust.set({
                                    let arg = arg.clone();
                                    let next = next.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let port = args[0].clone();
                                        // (letrec () (arg (quote gen-rust) port) (display ", " port) (next (quote gen-rust) port))
                                        {
                                            {
                                                // (arg (quote gen-rust) port)
                                                arg.clone().invoke(&[
                                                    Scm::symbol("gen-rust"),
                                                    port.clone(),
                                                ]);
                                                // (display ", " port)
                                                imports::display
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::from(", "), port.clone()]);
                                                // (next (quote gen-rust) port)
                                                next.clone().invoke(&[
                                                    Scm::symbol("gen-rust"),
                                                    port.clone(),
                                                ])
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
                                        // (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg))))
                                        {
                                            // (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ARG)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message ARG" msg)))
                                            if (
                                                // (eq? (quote repr) msg)
                                                imports::eq_p
                                                    .with(|value| value.get())
                                                    .invoke(&[Scm::symbol("repr"), msg.clone()])
                                            )
                                            .is_true()
                                            {
                                                // (print)
                                                globals::print.with(|value| value.get()).invoke(&[])
                                            } else {
                                                if (
                                                    // (eq? (quote transform) msg)
                                                    imports::eq_p.with(|value| value.get()).invoke(
                                                        &[Scm::symbol("transform"), msg.clone()],
                                                    )
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
                                                } else {
                                                    if (
                                                        // (eq? (quote free-vars) msg)
                                                        imports::eq_p
                                                            .with(|value| value.get())
                                                            .invoke(&[
                                                                Scm::symbol("free-vars"),
                                                                msg.clone(),
                                                            ])
                                                    )
                                                    .is_true()
                                                    {
                                                        // (free-vars)
                                                        free_minus_vars.get().invoke(&[])
                                                    } else {
                                                        if (
                                                            // (eq? (quote kind) msg)
                                                            imports::eq_p
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    Scm::symbol("kind"),
                                                                    msg.clone(),
                                                                ])
                                                        )
                                                        .is_true()
                                                        {
                                                            Scm::symbol("ARG")
                                                        } else {
                                                            if (
                                                                // (eq? (quote gen-rust) msg)
                                                                imports::eq_p
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        Scm::symbol("gen-rust"),
                                                                        msg.clone(),
                                                                    ])
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
                                                                // (error "Unknown message ARG" msg)
                                                                imports::error
                                                                    .with(|value| value.get())
                                                                    .invoke(&[
                                                                        Scm::from(
                                                                            "Unknown message ARG",
                                                                        ),
                                                                        msg.clone(),
                                                                    ])
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    })
                                });
                                self_.get()
                            }
                        })
                    })
                });
                // (define (make-fixlet params body args) (define (repr) (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr)))))) (define (transform fnc) (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))))) (define (free-vars) (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))) (define (gen-rust port) (define (gen-params p*) (if (pair? p*) (begin (print port (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (rust-block port (lambda () (display "let [" port) (gen-params params) (display "] = [" port) (args (quote gen-rust) port) (display "];" port) (body (quote gen-rust) port)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))) self)
                globals::make_minus_fixlet.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let body = args[1].clone();let args_ = args[2].clone();
// (letrec ((repr (lambda () (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr))))))) (transform (lambda (fnc) (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))))) (free-vars (lambda () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))) (gen-rust (lambda (port) (define (gen-params p*) (if (pair? p*) (begin (print port (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))) (rust-block port (lambda () (display "let [" port) (gen-params params) (display "] = [" port) (args (quote gen-rust) port) (display "];" port) (body (quote gen-rust) port))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr))))))
{
// (cons (quote FIXLET) (cons params (cons (args (quote repr)) (body (quote repr)))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("FIXLET"), 
// (cons params (cons (args (quote repr)) (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[params.clone(), 
// (cons (args (quote repr)) (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr"), ]), 
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let body = body.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();
// (letrec () (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))))
{
// (fnc self (lambda () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))))
fnc.clone().invoke(&[self_.get(), {let params = params.clone();let body = body.clone();let fnc = fnc.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc)))
{
// (make-fixlet params (body (quote transform) fnc) (args (quote transform) fnc))
globals::make_minus_fixlet.with(|value| value.get()).invoke(&[params.clone(), 
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ]), 
// (args (quote transform) fnc)
args_.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let body = body.clone();let params = params.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars))))
{
// (set-union (set-remove* (body (quote free-vars)) params) (args (quote free-vars)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (set-remove* (body (quote free-vars)) params)
globals::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"), ]), params.clone(), ]), 
// (args (quote free-vars))
args_.clone().invoke(&[Scm::symbol("free-vars"), ]), ])}})});
gen_minus_rust.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec ((gen-params (lambda (p*) (if (pair? p*) (begin (print port (rustify-identifier (car p*)) ", ") (gen-params (cdr p*))))))) (rust-block port (lambda () (display "let [" port) (gen-params params) (display "] = [" port) (args (quote gen-rust) port) (display "];" port) (body (quote gen-rust) port))))
{let gen_minus_params = Scm::uninitialized().into_boxed();
gen_minus_params.set({let port = port.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p_star_ = args[0].clone();
// (letrec () (if (pair? p*) (begin (print port (rustify-identifier (car p*)) ", ") (gen-params (cdr p*)))))
{if (
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone(), ])).is_true() {{
// (print port (rustify-identifier (car p*)) ", ")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier (car p*))
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone(), ]), ]), Scm::from(", "), ]);
// (gen-params (cdr p*))
gen_minus_params.get().invoke(&[
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone(), ]), ])}} else {Scm::symbol("*UNSPECIFIED*")}}})});

// (rust-block port (lambda () (display "let [" port) (gen-params params) (display "] = [" port) (args (quote gen-rust) port) (display "];" port) (body (quote gen-rust) port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let gen_minus_params = gen_minus_params.clone();let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (display "let [" port) (gen-params params) (display "] = [" port) (args (quote gen-rust) port) (display "];" port) (body (quote gen-rust) port))
{{
// (display "let [" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let ["), port.clone(), ]);
// (gen-params params)
gen_minus_params.get().invoke(&[params.clone(), ]);
// (display "] = [" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("] = ["), port.clone(), ]);
// (args (quote gen-rust) port)
args_.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display "];" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("];"), port.clone(), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})}, ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote FIXLET)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message FIXLET" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("FIXLET")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message FIXLET" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message FIXLET"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-scope params body args) (define (repr) (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr)))))) (define (transform fnc) (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))))) (define (free-vars-args args) (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args))))) (define (free-vars) (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params)) (define (gen-rust port) (rust-block port (lambda () (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args) (body (quote gen-rust) port)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))) self)
                globals::make_minus_scope.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let body = args[1].clone();let args_ = args[2].clone();
// (letrec ((repr (lambda () (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr))))))) (transform (lambda (fnc) (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))))) (free-vars-args (lambda (args) (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))))) (free-vars (lambda () (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params))) (gen-rust (lambda (port) (rust-block port (lambda () (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args) (body (quote gen-rust) port))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars_minus_args = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr))))))
{
// (cons (quote SCOPE) (cons params (cons (args (quote repr)) (body (quote repr)))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("SCOPE"), 
// (cons params (cons (args (quote repr)) (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[params.clone(), 
// (cons (args (quote repr)) (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[
// (args (quote repr))
args_.clone().invoke(&[Scm::symbol("repr"), ]), 
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let body = body.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let fnc = args[0].clone();
// (letrec () (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))))
{
// (fnc self (lambda () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))))
fnc.clone().invoke(&[self_.get(), {let params = params.clone();let body = body.clone();let fnc = fnc.clone();let args_ = args_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args)))
{
// (make-scope params (body (quote transform) fnc) (map (lambda (a) (a (quote transform) fnc)) args))
globals::make_minus_scope.with(|value| value.get()).invoke(&[params.clone(), 
// (body (quote transform) fnc)
body.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ]), 
// (map (lambda (a) (a (quote transform) fnc)) args)
imports::map.with(|value| value.get()).invoke(&[{let fnc = fnc.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let a = args[0].clone();
// (letrec () (a (quote transform) fnc))
{
// (a (quote transform) fnc)
a.clone().invoke(&[Scm::symbol("transform"), fnc.clone(), ])}})}, args_.clone(), ]), ])}})}, ])}})});
free_minus_vars_minus_args.set({let free_minus_vars_minus_args = free_minus_vars_minus_args.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let args_ = args[0].clone();
// (letrec () (if (null? args) (make-set) (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))))
{if (
// (null? args)
imports::null_p.with(|value| value.get()).invoke(&[args_.clone(), ])).is_true() {
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])} else {
// (set-union ((car args) (quote free-vars)) (free-vars-args (cdr args)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// ((car args) (quote free-vars))

// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]).invoke(&[Scm::symbol("free-vars"), ]), 
// (free-vars-args (cdr args))
free_minus_vars_minus_args.get().invoke(&[
// (cdr args)
imports::cdr.with(|value| value.get()).invoke(&[args_.clone(), ]), ]), ])}}})});
free_minus_vars.set({let body = body.clone();let free_minus_vars_minus_args = free_minus_vars_minus_args.clone();let args_ = args_.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params))
{
// (set-remove* (set-union (body (quote free-vars)) (free-vars-args args)) params)
globals::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (set-union (body (quote free-vars)) (free-vars-args args))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"), ]), 
// (free-vars-args args)
free_minus_vars_minus_args.get().invoke(&[args_.clone(), ]), ]), params.clone(), ])}})});
gen_minus_rust.set({let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (rust-block port (lambda () (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args) (body (quote gen-rust) port))))
{
// (rust-block port (lambda () (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args) (body (quote gen-rust) port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let params = params.clone();let args_ = args_.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params) (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args) (body (quote gen-rust) port))
{{
// (for-each (lambda (p) (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")) params)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let p = args[0].clone();
// (letrec () (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();"))
{
// (println port "let " (rustify-identifier p) " = Scm::uninitialized().into_boxed();")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("let "), 
// (rustify-identifier p)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[p.clone(), ]), Scm::from(" = Scm::uninitialized().into_boxed();"), ])}})}, params.clone(), ]);
// (for-each (lambda (p a) (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");")) params args)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p = args[0].clone();let a = args[1].clone();
// (letrec () (print port (rustify-identifier p) ".set(") (a (quote gen-rust) port) (println port ");"))
{{
// (print port (rustify-identifier p) ".set(")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier p)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[p.clone(), ]), Scm::from(".set("), ]);
// (a (quote gen-rust) port)
a.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (println port ");")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from(");"), ])}}})}, params.clone(), args_.clone(), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})}, ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SCOPE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message SCOPE" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("SCOPE")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message SCOPE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message SCOPE"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-sequence first next) (define (repr) (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))) (define (transform func) (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func))))) (define (free-vars) (set-union (first (quote free-vars)) (next (quote free-vars)))) (define (gen-rust-inner port) (first (quote gen-rust) port) (display ";" port) (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) port) (next (quote gen-rust) port))) (define (gen-rust port) (display "{" port) (gen-rust-inner port) (display "}" port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))) self)
                globals::make_minus_sequence.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let first = args[0].clone();let next = args[1].clone();
// (letrec ((repr (lambda () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))) (transform (lambda (func) (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func)))))) (free-vars (lambda () (set-union (first (quote free-vars)) (next (quote free-vars))))) (gen-rust-inner (lambda (port) (first (quote gen-rust) port) (display ";" port) (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) port) (next (quote gen-rust) port)))) (gen-rust (lambda (port) (display "{" port) (gen-rust-inner port) (display "}" port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust_minus_inner = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (quote SEQUENCE) (first (quote repr)) (next (quote repr))))
{
// (list (quote SEQUENCE) (first (quote repr)) (next (quote repr)))
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("SEQUENCE"), 
// (first (quote repr))
first.clone().invoke(&[Scm::symbol("repr"), ]), 
// (next (quote repr))
next.clone().invoke(&[Scm::symbol("repr"), ]), ])}})});
transform.set({let self_ = self_.clone();let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func)))))
{
// (func self (lambda () (make-sequence (first (quote transform) func) (next (quote transform) func))))
func.clone().invoke(&[self_.get(), {let first = first.clone();let func = func.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-sequence (first (quote transform) func) (next (quote transform) func)))
{
// (make-sequence (first (quote transform) func) (next (quote transform) func))
globals::make_minus_sequence.with(|value| value.get()).invoke(&[
// (first (quote transform) func)
first.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), 
// (next (quote transform) func)
next.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-union (first (quote free-vars)) (next (quote free-vars))))
{
// (set-union (first (quote free-vars)) (next (quote free-vars)))
globals::set_minus_union.with(|value| value.get()).invoke(&[
// (first (quote free-vars))
first.clone().invoke(&[Scm::symbol("free-vars"), ]), 
// (next (quote free-vars))
next.clone().invoke(&[Scm::symbol("free-vars"), ]), ])}})});
gen_minus_rust_minus_inner.set({let first = first.clone();let next = next.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (first (quote gen-rust) port) (display ";" port) (if (eq? (quote SEQUENCE) (next (quote kind))) (next (quote gen-rust-inner) port) (next (quote gen-rust) port)))
{{
// (first (quote gen-rust) port)
first.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (display ";" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(";"), port.clone(), ]);if (
// (eq? (quote SEQUENCE) (next (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("SEQUENCE"), 
// (next (quote kind))
next.clone().invoke(&[Scm::symbol("kind"), ]), ])).is_true() {
// (next (quote gen-rust-inner) port)
next.clone().invoke(&[Scm::symbol("gen-rust-inner"), port.clone(), ])} else {
// (next (quote gen-rust) port)
next.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}}})});
gen_minus_rust.set({let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "{" port) (gen-rust-inner port) (display "}" port))
{{
// (display "{" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("{"), port.clone(), ]);
// (gen-rust-inner port)
gen_minus_rust_minus_inner.get().invoke(&[port.clone(), ]);
// (display "}" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("}"), port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let gen_minus_rust_minus_inner = gen_minus_rust_minus_inner.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote SEQUENCE)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote gen-rust-inner) msg) (gen-rust-inner (car args))) (else (error "Unknown message SEQUENCE" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("SEQUENCE")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote gen-rust-inner) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust-inner"), msg.clone(), ])).is_true() {
// (gen-rust-inner (car args))
gen_minus_rust_minus_inner.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message SEQUENCE" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message SEQUENCE"), msg.clone(), ])}}}}}}}})});
self_.get()}})}));
                // (define (make-abstraction params vars body) (define (repr) (cons (quote ABSTRACTION) (cons params (body (quote repr))))) (define (transform func) (func self (lambda () (make-abstraction params vars (body (quote transform) func))))) (define (free-vars) (set-remove* (body (quote free-vars)) params)) (define (prepare-closure port free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars))))) (define (gen-rust port) (define (gen-params p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))) self)
                globals::make_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let params = args[0].clone();let vars = args[1].clone();let body = args[2].clone();
// (letrec ((repr (lambda () (cons (quote ABSTRACTION) (cons params (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) params))) (prepare-closure (lambda (port free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))))) (gen-rust (lambda (port) (define (gen-params p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))))) self)
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
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-abstraction params vars (body (quote transform) func)))))
{
// (func self (lambda () (make-abstraction params vars (body (quote transform) func))))
func.clone().invoke(&[self_.get(), {let params = params.clone();let vars = vars.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-abstraction params vars (body (quote transform) func)))
{
// (make-abstraction params vars (body (quote transform) func))
globals::make_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(), vars.clone(), 
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let body = body.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (body (quote free-vars)) params))
{
// (set-remove* (body (quote free-vars)) params)
globals::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"), ]), params.clone(), ])}})});
prepare_minus_closure.set({let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let free_minus_vars = args[1].clone();
// (letrec () (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))))
{if (
// (pair? free-vars)
imports::pair_p.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ])).is_true() {
// (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))
{let [name, ] = [
// (car free-vars)
imports::car.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ]), ];{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier name) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), port.clone(), ]);
// (display " = " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = "), port.clone(), ]);
// (display (rustify-identifier name) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), port.clone(), ]);
// (display ".clone();" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(".clone();"), port.clone(), ]);
// (prepare-closure port (cdr free-vars))
prepare_minus_closure.get().invoke(&[port.clone(), 
// (cdr free-vars)
imports::cdr.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ]), ])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});
gen_minus_rust.set({let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))))))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))))
{let gen_minus_params = Scm::uninitialized().into_boxed();
gen_minus_params.set({let port = port.clone();let gen_minus_params = gen_minus_params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();
// (letrec () (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1)))))
{if (
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone(), ])).is_true() {{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier (car p*)) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier (car p*))
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone(), ]), ]), port.clone(), ]);
// (display " = args[" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = args["), port.clone(), ]);
// (display k port)
imports::display.with(|value| value.get()).invoke(&[k.clone(), port.clone(), ]);
// (display "].clone();" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("].clone();"), port.clone(), ]);
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone(), ]), 
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(), Scm::from(1), ]), ])}} else {Scm::symbol("*UNSPECIFIED*")}}})});

// (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let prepare_minus_closure = prepare_minus_closure.clone();let port = port.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))
{{
// (prepare-closure port (free-vars))
prepare_minus_closure.get().invoke(&[port.clone(), 
// (free-vars)
free_minus_vars.get().invoke(&[]), ]);
// (display "Scm::func(move |args: &[Scm]|" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::func(move |args: &[Scm]|"), port.clone(), ]);
// (rust-block port (lambda () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (display "if args.len() != " port) (display (length params) port) (display "{panic!(\"invalid arity\")}" port) (gen-params params 0) (body (quote gen-rust) port))
{{
// (display "if args.len() != " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("if args.len() != "), port.clone(), ]);
// (display (length params) port)
imports::display.with(|value| value.get()).invoke(&[
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone(), ]), port.clone(), ]);
// (display "{panic!(\"invalid arity\")}" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("{panic!(\"invalid arity\")}"), port.clone(), ]);
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(), Scm::from(0), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})}, ]);
// (display ")" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(")"), port.clone(), ])}}})}, ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vars = vars.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-body) msg) body) (else (error "Unknown message ABSTRACTION" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("ABSTRACTION")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"), msg.clone(), ])).is_true() {params.clone()} else {if (
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"), msg.clone(), ])).is_true() {vars.clone()} else {if (
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"), msg.clone(), ])).is_true() {body.clone()} else {
// (error "Unknown message ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message ABSTRACTION"), msg.clone(), ])}}}}}}}}}})});
self_.get()}})}));
                // (define (make-vararg-abstraction params vararg vars varvar body) (define (repr) (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr))))) (define (transform func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))) (define (free-vars) (set-remove* (body (quote free-vars)) (cons vararg params))) (define (prepare-closure port free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars))))) (define (gen-rust port) (define (gen-params p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))) (begin (display "let " port) (display (rustify-identifier vararg) port) (display " = Scm::list(&args[" port) (display k port) (display "..]);" port)))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))) self)
                globals::make_minus_vararg_minus_abstraction.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let params = args[0].clone();let vararg = args[1].clone();let vars = args[2].clone();let varvar = args[3].clone();let body = args[4].clone();
// (letrec ((repr (lambda () (cons (quote VARARG-ABSTRACTION) (cons params (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))) (free-vars (lambda () (set-remove* (body (quote free-vars)) (cons vararg params)))) (prepare-closure (lambda (port free-vars) (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))))) (gen-rust (lambda (port) (define (gen-params p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))) (begin (display "let " port) (display (rustify-identifier vararg) port) (display " = Scm::list(&args[" port) (display k port) (display "..]);" port)))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))))) self)
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
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))))
{
// (func self (lambda () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))))
func.clone().invoke(&[self_.get(), {let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-vararg-abstraction params vararg vars varvar (body (quote transform) func)))
{
// (make-vararg-abstraction params vararg vars varvar (body (quote transform) func))
globals::make_minus_vararg_minus_abstraction.with(|value| value.get()).invoke(&[params.clone(), vararg.clone(), vars.clone(), varvar.clone(), 
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let body = body.clone();let vararg = vararg.clone();let params = params.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (set-remove* (body (quote free-vars)) (cons vararg params)))
{
// (set-remove* (body (quote free-vars)) (cons vararg params))
globals::set_minus_remove_star_.with(|value| value.get()).invoke(&[
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"), ]), 
// (cons vararg params)
imports::cons.with(|value| value.get()).invoke(&[vararg.clone(), params.clone(), ]), ])}})});
prepare_minus_closure.set({let prepare_minus_closure = prepare_minus_closure.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let free_minus_vars = args[1].clone();
// (letrec () (if (pair? free-vars) (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))))
{if (
// (pair? free-vars)
imports::pair_p.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ])).is_true() {
// (let ((name (car free-vars))) (display "let " port) (display (rustify-identifier name) port) (display " = " port) (display (rustify-identifier name) port) (display ".clone();" port) (prepare-closure port (cdr free-vars)))
{let [name, ] = [
// (car free-vars)
imports::car.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ]), ];{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier name) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), port.clone(), ]);
// (display " = " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = "), port.clone(), ]);
// (display (rustify-identifier name) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), port.clone(), ]);
// (display ".clone();" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(".clone();"), port.clone(), ]);
// (prepare-closure port (cdr free-vars))
prepare_minus_closure.get().invoke(&[port.clone(), 
// (cdr free-vars)
imports::cdr.with(|value| value.get()).invoke(&[free_minus_vars.clone(), ]), ])}}} else {Scm::symbol("*UNSPECIFIED*")}}})});
gen_minus_rust.set({let prepare_minus_closure = prepare_minus_closure.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let body = body.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec ((gen-params (lambda (p* k) (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))) (begin (display "let " port) (display (rustify-identifier vararg) port) (display " = Scm::list(&args[" port) (display k port) (display "..]);" port)))))) (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))))
{let gen_minus_params = Scm::uninitialized().into_boxed();
gen_minus_params.set({let port = port.clone();let gen_minus_params = gen_minus_params.clone();let vararg = vararg.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let p_star_ = args[0].clone();let k = args[1].clone();
// (letrec () (if (pair? p*) (begin (display "let " port) (display (rustify-identifier (car p*)) port) (display " = args[" port) (display k port) (display "].clone();" port) (gen-params (cdr p*) (+ k 1))) (begin (display "let " port) (display (rustify-identifier vararg) port) (display " = Scm::list(&args[" port) (display k port) (display "..]);" port))))
{if (
// (pair? p*)
imports::pair_p.with(|value| value.get()).invoke(&[p_star_.clone(), ])).is_true() {{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier (car p*)) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier (car p*))
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car p*)
imports::car.with(|value| value.get()).invoke(&[p_star_.clone(), ]), ]), port.clone(), ]);
// (display " = args[" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = args["), port.clone(), ]);
// (display k port)
imports::display.with(|value| value.get()).invoke(&[k.clone(), port.clone(), ]);
// (display "].clone();" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("].clone();"), port.clone(), ]);
// (gen-params (cdr p*) (+ k 1))
gen_minus_params.get().invoke(&[
// (cdr p*)
imports::cdr.with(|value| value.get()).invoke(&[p_star_.clone(), ]), 
// (+ k 1)
imports::_plus_.with(|value| value.get()).invoke(&[k.clone(), Scm::from(1), ]), ])}} else {{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier vararg) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier vararg)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[vararg.clone(), ]), port.clone(), ]);
// (display " = Scm::list(&args[" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = Scm::list(&args["), port.clone(), ]);
// (display k port)
imports::display.with(|value| value.get()).invoke(&[k.clone(), port.clone(), ]);
// (display "..]);" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("..]);"), port.clone(), ])}}}})});

// (rust-block port (lambda () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let prepare_minus_closure = prepare_minus_closure.clone();let port = port.clone();let free_minus_vars = free_minus_vars.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (prepare-closure port (free-vars)) (display "Scm::func(move |args: &[Scm]|" port) (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))) (display ")" port))
{{
// (prepare-closure port (free-vars))
prepare_minus_closure.get().invoke(&[port.clone(), 
// (free-vars)
free_minus_vars.get().invoke(&[]), ]);
// (display "Scm::func(move |args: &[Scm]|" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("Scm::func(move |args: &[Scm]|"), port.clone(), ]);
// (rust-block port (lambda () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let params = params.clone();let gen_minus_params = gen_minus_params.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (display "if args.len() < " port) (display (length params) port) (display "{panic!(\"not enough args\")}" port) (gen-params params 0) (body (quote gen-rust) port))
{{
// (display "if args.len() < " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("if args.len() < "), port.clone(), ]);
// (display (length params) port)
imports::display.with(|value| value.get()).invoke(&[
// (length params)
imports::length.with(|value| value.get()).invoke(&[params.clone(), ]), port.clone(), ]);
// (display "{panic!(\"not enough args\")}" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("{panic!(\"not enough args\")}"), port.clone(), ]);
// (gen-params params 0)
gen_minus_params.get().invoke(&[params.clone(), Scm::from(0), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})}, ]);
// (display ")" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(")"), port.clone(), ])}}})}, ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();let params = params.clone();let vararg = vararg.clone();let vars = vars.clone();let varvar = varvar.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote VARARG-ABSTRACTION)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) ((eq? (quote get-params) msg) params) ((eq? (quote get-vararg) msg) vararg) ((eq? (quote get-vars) msg) vars) ((eq? (quote get-varvar) msg) varvar) ((eq? (quote get-body) msg) body) (else (error "Unknown message VARARG-ABSTRACTION" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("VARARG-ABSTRACTION")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote get-params) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-params"), msg.clone(), ])).is_true() {params.clone()} else {if (
// (eq? (quote get-vararg) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vararg"), msg.clone(), ])).is_true() {vararg.clone()} else {if (
// (eq? (quote get-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-vars"), msg.clone(), ])).is_true() {vars.clone()} else {if (
// (eq? (quote get-varvar) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-varvar"), msg.clone(), ])).is_true() {varvar.clone()} else {if (
// (eq? (quote get-body) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("get-body"), msg.clone(), ])).is_true() {body.clone()} else {
// (error "Unknown message VARARG-ABSTRACTION" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message VARARG-ABSTRACTION"), msg.clone(), ])}}}}}}}}}}}})});
self_.get()}})}));
                // (define (make-program globals imports init body libraries) (define (repr) (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr)))))) (define (transform func) (func self (lambda () (make-program globals imports init (body (quote transform) func))))) (define (gen-imports port) (for-each (lambda (i) (i (quote gen-rust) port)) imports)) (define (gen-rust port) (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (display "mod imports" port) (rust-block port (lambda () (gen-imports port))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (display "pub fn main()" port) (rust-block port (lambda () (newline port) (println port "eprintln!(\"built with\");") (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (newline port) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init) (body (quote gen-rust) port) (println port ";"))) (newline port) (rust-gen-modules port libraries)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))) self)
                globals::make_minus_program.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 5{panic!("invalid arity")}let globals = args[0].clone();let imports = args[1].clone();let init = args[2].clone();let body = args[3].clone();let libraries = args[4].clone();
// (letrec ((repr (lambda () (cons (quote PROGRAM) (cons globals (cons imports (body (quote repr))))))) (transform (lambda (func) (func self (lambda () (make-program globals imports init (body (quote transform) func)))))) (gen-imports (lambda (port) (for-each (lambda (i) (i (quote gen-rust) port)) imports))) (gen-rust (lambda (port) (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (display "mod imports" port) (rust-block port (lambda () (gen-imports port))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (display "pub fn main()" port) (rust-block port (lambda () (newline port) (println port "eprintln!(\"built with\");") (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (newline port) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init) (body (quote gen-rust) port) (println port ";"))) (newline port) (rust-gen-modules port libraries))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))))) self)
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
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-program globals imports init (body (quote transform) func)))))
{
// (func self (lambda () (make-program globals imports init (body (quote transform) func))))
func.clone().invoke(&[self_.get(), {let globals = globals.clone();let imports = imports.clone();let init = init.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-program globals imports init (body (quote transform) func)))
{
// (make-program globals imports init (body (quote transform) func))
globals::make_minus_program.with(|value| value.get()).invoke(&[globals.clone(), imports.clone(), init.clone(), 
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
gen_minus_imports.set({let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (for-each (lambda (i) (i (quote gen-rust) port)) imports))
{
// (for-each (lambda (i) (i (quote gen-rust) port)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();
// (letrec () (i (quote gen-rust) port))
{
// (i (quote gen-rust) port)
i.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}})}, imports.clone(), ])}})});
gen_minus_rust.set({let gen_minus_imports = gen_minus_imports.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let libraries = libraries.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};") (display "mod imports" port) (rust-block port (lambda () (gen-imports port))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (display "pub fn main()" port) (rust-block port (lambda () (newline port) (println port "eprintln!(\"built with\");") (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (newline port) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init) (body (quote gen-rust) port) (println port ";"))) (newline port) (rust-gen-modules port libraries))
{{
// (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};"), ]);
// (display "mod imports" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("mod imports"), port.clone(), ]);
// (rust-block port (lambda () (gen-imports port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let gen_minus_imports = gen_minus_imports.clone();let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (gen-imports port))
{
// (gen-imports port)
gen_minus_imports.get().invoke(&[port.clone(), ])}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (display "mod globals" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("mod globals"), port.clone(), ]);
// (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let globals = globals.clone();let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))
{{if (
// (any (lambda (g) (global-regular? (cdr g))) globals)
globals::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
globals::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(), ]), ])}})}, globals.clone(), ])).is_true() {
// (println port "use sunny_core::{Mut, Scm};")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("use sunny_core::{Mut, Scm};"), ])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs port globals)
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[port.clone(), globals.clone(), ])}}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (display "pub fn main()" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("pub fn main()"), port.clone(), ]);
// (rust-block port (lambda () (newline port) (println port "eprintln!(\"built with\");") (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (newline port) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init) (body (quote gen-rust) port) (println port ";")))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let init = init.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (newline port) (println port "eprintln!(\"built with\");") (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);") (newline port) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init) (body (quote gen-rust) port) (println port ";"))
{{
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (println port "eprintln!(\"built with\");")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("eprintln!(\"built with\");"), ]);
// (println port "eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("eprintln!(\"    '{}' memory model\", MEMORY_MODEL_KIND);"), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port)) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec () (display "crate::" port) (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib) (display "initialize();" port) (newline port))
{{
// (display "crate::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("crate::"), port.clone(), ]);
// (for-each (lambda (l) (display (rustify-libname l) port) (display "::" port)) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();
// (letrec () (display (rustify-libname l) port) (display "::" port))
{{
// (display (rustify-libname l) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-libname l)
globals::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone(), ]), port.clone(), ]);
// (display "::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("::"), port.clone(), ])}}})}, lib.clone(), ]);
// (display "initialize();" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("initialize();"), port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ])}}})}, init.clone(), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (println port ";")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from(";"), ])}}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (rust-gen-modules port libraries)
globals::rust_minus_gen_minus_modules.with(|value| value.get()).invoke(&[port.clone(), libraries.clone(), ])}}})});
self_.set({let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote PROGRAM)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message PROGRAM" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("PROGRAM")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message PROGRAM" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message PROGRAM"), msg.clone(), ])}}}}}})});
self_.get()}})}));
                // (define (make-library name globals init body imports exports) (define (repr) (append (quote LIBRARY) name exports imports globals (body (quote repr)))) (define (transform func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports)))) (define (gen-exports port exports) (for-each (lambda (expo) (expo (quote gen-rust) port)) exports)) (define (gen-rust port) (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (display "mod imports" port) (rust-block port (lambda () (for-each (lambda (i) (i (quote gen-rust) port)) imports))) (newline port) (newline port) (display "pub mod exports" port) (rust-block port (lambda () (gen-exports port exports))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (if (eq? (quote NOP) (body (quote kind))) (println port "pub fn initialize() {") (begin (println port "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (newline port) (println port "pub fn initialize() {") (println port "if INITIALIZED.with(|x| x.get()) { return }") (println port "INITIALIZED.with(|x| x.set(true));") (newline port))) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (print port (rustify-libname l) "::")) lib) (println port "initialize();")) init) (body (quote gen-rust) port) (println port ";}")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))) self)
                globals::make_minus_library.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 6{panic!("invalid arity")}let name = args[0].clone();let globals = args[1].clone();let init = args[2].clone();let body = args[3].clone();let imports = args[4].clone();let exports = args[5].clone();
// (letrec ((repr (lambda () (append (quote LIBRARY) name exports imports globals (body (quote repr))))) (transform (lambda (func) (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))) (gen-exports (lambda (port exports) (for-each (lambda (expo) (expo (quote gen-rust) port)) exports))) (gen-rust (lambda (port) (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (display "mod imports" port) (rust-block port (lambda () (for-each (lambda (i) (i (quote gen-rust) port)) imports))) (newline port) (newline port) (display "pub mod exports" port) (rust-block port (lambda () (gen-exports port exports))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (if (eq? (quote NOP) (body (quote kind))) (println port "pub fn initialize() {") (begin (println port "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (newline port) (println port "pub fn initialize() {") (println port "if INITIALIZED.with(|x| x.get()) { return }") (println port "INITIALIZED.with(|x| x.set(true));") (newline port))) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (print port (rustify-libname l) "::")) lib) (println port "initialize();")) init) (body (quote gen-rust) port) (println port ";}"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let gen_minus_exports = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let name = name.clone();let exports = exports.clone();let imports = imports.clone();let globals = globals.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (append (quote LIBRARY) name exports imports globals (body (quote repr))))
{
// (append (quote LIBRARY) name exports imports globals (body (quote repr)))
globals::append.with(|value| value.get()).invoke(&[Scm::symbol("LIBRARY"), name.clone(), exports.clone(), imports.clone(), globals.clone(), 
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"), ]), ])}})});
transform.set({let self_ = self_.clone();let globals = globals.clone();let init = init.clone();let body = body.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-library globals init (body (quote transform) func) imports exports))))
{
// (func self (lambda () (make-library globals init (body (quote transform) func) imports exports)))
func.clone().invoke(&[self_.get(), {let globals = globals.clone();let init = init.clone();let body = body.clone();let func = func.clone();let imports = imports.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-library globals init (body (quote transform) func) imports exports))
{
// (make-library globals init (body (quote transform) func) imports exports)
globals::make_minus_library.with(|value| value.get()).invoke(&[globals.clone(), init.clone(), 
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), imports.clone(), exports.clone(), ])}})}, ])}})});
gen_minus_exports.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let exports = args[1].clone();
// (letrec () (for-each (lambda (expo) (expo (quote gen-rust) port)) exports))
{
// (for-each (lambda (expo) (expo (quote gen-rust) port)) exports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let expo = args[0].clone();
// (letrec () (expo (quote gen-rust) port))
{
// (expo (quote gen-rust) port)
expo.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}})}, exports.clone(), ])}})});
gen_minus_rust.set({let imports = imports.clone();let gen_minus_exports = gen_minus_exports.clone();let exports = exports.clone();let globals = globals.clone();let body = body.clone();let init = init.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm};") (display "mod imports" port) (rust-block port (lambda () (for-each (lambda (i) (i (quote gen-rust) port)) imports))) (newline port) (newline port) (display "pub mod exports" port) (rust-block port (lambda () (gen-exports port exports))) (newline port) (newline port) (display "mod globals" port) (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))) (newline port) (newline port) (if (eq? (quote NOP) (body (quote kind))) (println port "pub fn initialize() {") (begin (println port "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }") (newline port) (println port "pub fn initialize() {") (println port "if INITIALIZED.with(|x| x.get()) { return }") (println port "INITIALIZED.with(|x| x.set(true));") (newline port))) (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (print port (rustify-libname l) "::")) lib) (println port "initialize();")) init) (body (quote gen-rust) port) (println port ";}"))
{{
// (println port "#[allow(unused_imports)] use sunny_core::{Mut, Scm};")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("#[allow(unused_imports)] use sunny_core::{Mut, Scm};"), ]);
// (display "mod imports" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("mod imports"), port.clone(), ]);
// (rust-block port (lambda () (for-each (lambda (i) (i (quote gen-rust) port)) imports)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let imports = imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (for-each (lambda (i) (i (quote gen-rust) port)) imports))
{
// (for-each (lambda (i) (i (quote gen-rust) port)) imports)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let i = args[0].clone();
// (letrec () (i (quote gen-rust) port))
{
// (i (quote gen-rust) port)
i.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}})}, imports.clone(), ])}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (display "pub mod exports" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("pub mod exports"), port.clone(), ]);
// (rust-block port (lambda () (gen-exports port exports)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let gen_minus_exports = gen_minus_exports.clone();let port = port.clone();let exports = exports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (gen-exports port exports))
{
// (gen-exports port exports)
gen_minus_exports.get().invoke(&[port.clone(), exports.clone(), ])}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (display "mod globals" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("mod globals"), port.clone(), ]);
// (rust-block port (lambda () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let globals = globals.clone();let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (if (any (lambda (g) (global-regular? (cdr g))) globals) (println port "use sunny_core::{Mut, Scm};")) (rust-gen-global-defs port globals))
{{if (
// (any (lambda (g) (global-regular? (cdr g))) globals)
globals::any.with(|value| value.get()).invoke(&[{Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let g = args[0].clone();
// (letrec () (global-regular? (cdr g)))
{
// (global-regular? (cdr g))
globals::global_minus_regular_p.with(|value| value.get()).invoke(&[
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(), ]), ])}})}, globals.clone(), ])).is_true() {
// (println port "use sunny_core::{Mut, Scm};")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("use sunny_core::{Mut, Scm};"), ])} else {Scm::symbol("*UNSPECIFIED*")};
// (rust-gen-global-defs port globals)
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[port.clone(), globals.clone(), ])}}})}, ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);if (
// (eq? (quote NOP) (body (quote kind)))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("NOP"), 
// (body (quote kind))
body.clone().invoke(&[Scm::symbol("kind"), ]), ])).is_true() {
// (println port "pub fn initialize() {")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("pub fn initialize() {"), ])} else {{
// (println port "thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("thread_local! { static INITIALIZED: std::cell::Cell<bool> = std::cell::Cell::new(false); }"), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ]);
// (println port "pub fn initialize() {")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("pub fn initialize() {"), ]);
// (println port "if INITIALIZED.with(|x| x.get()) { return }")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("if INITIALIZED.with(|x| x.get()) { return }"), ]);
// (println port "INITIALIZED.with(|x| x.set(true));")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("INITIALIZED.with(|x| x.set(true));"), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ])}};
// (for-each (lambda (lib) (display "crate::" port) (for-each (lambda (l) (print port (rustify-libname l) "::")) lib) (println port "initialize();")) init)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec () (display "crate::" port) (for-each (lambda (l) (print port (rustify-libname l) "::")) lib) (println port "initialize();"))
{{
// (display "crate::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("crate::"), port.clone(), ]);
// (for-each (lambda (l) (print port (rustify-libname l) "::")) lib)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let port = port.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let l = args[0].clone();
// (letrec () (print port (rustify-libname l) "::"))
{
// (print port (rustify-libname l) "::")
globals::print.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-libname l)
globals::rustify_minus_libname.with(|value| value.get()).invoke(&[l.clone(), ]), Scm::from("::"), ])}})}, lib.clone(), ]);
// (println port "initialize();")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("initialize();"), ])}}})}, init.clone(), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ]);
// (println port ";}")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from(";}"), ])}}})});
self_.set({let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote NOP)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message NOP" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("NOP")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message NOP" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message NOP"), msg.clone(), ])}}}}}})});
self_.get()}})}));
                // (define (make-boxify name body) (define (repr) (cons (quote BOXIFY) (cons name (body (quote repr))))) (define (transform func) (func self (lambda () (make-boxify name (body (quote transform) func))))) (define (free-vars) (body (quote free-vars))) (define (gen-rust port) (rust-block port (lambda () (display "let " port) (display (rustify-identifier name port)) (display " = " port) (display (rustify-identifier name port)) (display ".into_boxed( port);") (body (quote gen-rust) port)))) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))) self)
                globals::make_minus_boxify.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let name = args[0].clone();let body = args[1].clone();
// (letrec ((repr (lambda () (cons (quote BOXIFY) (cons name (body (quote repr)))))) (transform (lambda (func) (func self (lambda () (make-boxify name (body (quote transform) func)))))) (free-vars (lambda () (body (quote free-vars)))) (gen-rust (lambda (port) (rust-block port (lambda () (display "let " port) (display (rustify-identifier name port)) (display " = " port) (display (rustify-identifier name port)) (display ".into_boxed( port);") (body (quote gen-rust) port))))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote BOXIFY) (cons name (body (quote repr)))))
{
// (cons (quote BOXIFY) (cons name (body (quote repr))))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("BOXIFY"), 
// (cons name (body (quote repr)))
imports::cons.with(|value| value.get()).invoke(&[name.clone(), 
// (body (quote repr))
body.clone().invoke(&[Scm::symbol("repr"), ]), ]), ])}})});
transform.set({let self_ = self_.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-boxify name (body (quote transform) func)))))
{
// (func self (lambda () (make-boxify name (body (quote transform) func))))
func.clone().invoke(&[self_.get(), {let name = name.clone();let body = body.clone();let func = func.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-boxify name (body (quote transform) func)))
{
// (make-boxify name (body (quote transform) func))
globals::make_minus_boxify.with(|value| value.get()).invoke(&[name.clone(), 
// (body (quote transform) func)
body.clone().invoke(&[Scm::symbol("transform"), func.clone(), ]), ])}})}, ])}})});
free_minus_vars.set({let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (body (quote free-vars)))
{
// (body (quote free-vars))
body.clone().invoke(&[Scm::symbol("free-vars"), ])}})});
gen_minus_rust.set({let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (rust-block port (lambda () (display "let " port) (display (rustify-identifier name port)) (display " = " port) (display (rustify-identifier name port)) (display ".into_boxed( port);") (body (quote gen-rust) port))))
{
// (rust-block port (lambda () (display "let " port) (display (rustify-identifier name port)) (display " = " port) (display (rustify-identifier name port)) (display ".into_boxed( port);") (body (quote gen-rust) port)))
globals::rust_minus_block.with(|value| value.get()).invoke(&[port.clone(), {let port = port.clone();let name = name.clone();let body = body.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (display "let " port) (display (rustify-identifier name port)) (display " = " port) (display (rustify-identifier name port)) (display ".into_boxed( port);") (body (quote gen-rust) port))
{{
// (display "let " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("let "), port.clone(), ]);
// (display (rustify-identifier name port))
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name port)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), port.clone(), ]), ]);
// (display " = " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(" = "), port.clone(), ]);
// (display (rustify-identifier name port))
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier name port)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), port.clone(), ]), ]);
// (display ".into_boxed( port);")
imports::display.with(|value| value.get()).invoke(&[Scm::from(".into_boxed( port);"), ]);
// (body (quote gen-rust) port)
body.clone().invoke(&[Scm::symbol("gen-rust"), port.clone(), ])}}})}, ])}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote BOXIFY)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message BOXIFY" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("BOXIFY")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message BOXIFY" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message BOXIFY"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-export env name exname) (define (repr) (list (quote EXPORT) name (quote AS) exname)) (define (transform func) (func self (lambda () self))) (define (gen-rust port) (display "pub use super::" port) (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (display "globals::" port)) ((eq? (quote IMPORT-REF) (variable-getter var)) (display "imports::" port)) (else (error "invalid export variable" var name)))) (println port (rustify-identifier name) " as " (rustify-identifier exname) ";")) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))) self)
                globals::make_minus_export.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let env = args[0].clone();let name = args[1].clone();let exname = args[2].clone();
// (letrec ((repr (lambda () (list (quote EXPORT) name (quote AS) exname))) (transform (lambda (func) (func self (lambda () self)))) (gen-rust (lambda (port) (display "pub use super::" port) (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (display "globals::" port)) ((eq? (quote IMPORT-REF) (variable-getter var)) (display "imports::" port)) (else (error "invalid export variable" var name)))) (println port (rustify-identifier name) " as " (rustify-identifier exname) ";"))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let name = name.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (list (quote EXPORT) name (quote AS) exname))
{
// (list (quote EXPORT) name (quote AS) exname)
imports::list.with(|value| value.get()).invoke(&[Scm::symbol("EXPORT"), name.clone(), Scm::symbol("AS"), exname.clone(), ])}})});
transform.set({let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () self)))
{
// (func self (lambda () self))
func.clone().invoke(&[self_.get(), {let self_ = self_.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () self)
{self_.get()}})}, ])}})});
gen_minus_rust.set({let name = name.clone();let env = env.clone();let exname = exname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "pub use super::" port) (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (display "globals::" port)) ((eq? (quote IMPORT-REF) (variable-getter var)) (display "imports::" port)) (else (error "invalid export variable" var name)))) (println port (rustify-identifier name) " as " (rustify-identifier exname) ";"))
{{
// (display "pub use super::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("pub use super::"), port.clone(), ]);
// (let ((var (lookup name env))) (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (display "globals::" port)) ((eq? (quote IMPORT-REF) (variable-getter var)) (display "imports::" port)) (else (error "invalid export variable" var name))))
{let [var, ] = [
// (lookup name env)
globals::lookup.with(|value| value.get()).invoke(&[name.clone(), env.clone(), ]), ];
// (cond ((not var) (error "undefined export" name)) ((eq? (quote GLOBAL-REF) (variable-getter var)) (display "globals::" port)) ((eq? (quote IMPORT-REF) (variable-getter var)) (display "imports::" port)) (else (error "invalid export variable" var name)))
if (
// (not var)
imports::not.with(|value| value.get()).invoke(&[var.clone(), ])).is_true() {
// (error "undefined export" name)
imports::error.with(|value| value.get()).invoke(&[Scm::from("undefined export"), name.clone(), ])} else {if (
// (eq? (quote GLOBAL-REF) (variable-getter var))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("GLOBAL-REF"), 
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), ])).is_true() {
// (display "globals::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("globals::"), port.clone(), ])} else {if (
// (eq? (quote IMPORT-REF) (variable-getter var))
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT-REF"), 
// (variable-getter var)
globals::variable_minus_getter.with(|value| value.get()).invoke(&[var.clone(), ]), ])).is_true() {
// (display "imports::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("imports::"), port.clone(), ])} else {
// (error "invalid export variable" var name)
imports::error.with(|value| value.get()).invoke(&[Scm::from("invalid export variable"), var.clone(), name.clone(), ])}}}};
// (println port (rustify-identifier name) " as " (rustify-identifier exname) ";")
globals::println.with(|value| value.get()).invoke(&[port.clone(), 
// (rustify-identifier name)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[name.clone(), ]), Scm::from(" as "), 
// (rustify-identifier exname)
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[exname.clone(), ]), Scm::from(";"), ])}}})});
self_.set({let transform = transform.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote kind) msg) (quote EXPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message EXPORT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("EXPORT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message EXPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message EXPORT"), msg.clone(), ])}}}}}})});
self_.get()}})}));
                // (define (make-import lib) (define (repr) (cons (quote IMPORT) lib)) (define (transform func) (func self (lambda () (make-import lib)))) (define (free-vars) (make-set)) (define (gen-libname port lib) (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib))))) (define (gen-rust port) (display "pub use crate::" port) (gen-libname port lib) (display "::exports::*;" port) (newline port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))) self)
                globals::make_minus_import.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec ((repr (lambda () (cons (quote IMPORT) lib))) (transform (lambda (func) (func self (lambda () (make-import lib))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (port lib) (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib)))))) (gen-rust (lambda (port) (display "pub use crate::" port) (gen-libname port lib) (display "::exports::*;" port) (newline port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_libname = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote IMPORT) lib))
{
// (cons (quote IMPORT) lib)
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT"), lib.clone(), ])}})});
transform.set({let self_ = self_.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-import lib))))
{
// (func self (lambda () (make-import lib)))
func.clone().invoke(&[self_.get(), {let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-import lib))
{
// (make-import lib)
globals::make_minus_import.with(|value| value.get()).invoke(&[lib.clone(), ])}})}, ])}})});
free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-set))
{
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])}})});
gen_minus_libname.set({let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let lib = args[1].clone();
// (letrec () (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib)))))
{if (
// (null? lib)
imports::null_p.with(|value| value.get()).invoke(&[lib.clone(), ])).is_true() {
// (display "" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(""), port.clone(), ])} else {{
// (display (rustify-libname (car lib)) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-libname (car lib))
globals::rustify_minus_libname.with(|value| value.get()).invoke(&[
// (car lib)
imports::car.with(|value| value.get()).invoke(&[lib.clone(), ]), ]), port.clone(), ]);if (
// (null? (cdr lib))
imports::null_p.with(|value| value.get()).invoke(&[
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone(), ]), ])).is_true() {
// (display "" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(""), port.clone(), ])} else {
// (display "::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("::"), port.clone(), ])};
// (gen-libname port (cdr lib))
gen_minus_libname.get().invoke(&[port.clone(), 
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone(), ]), ])}}}})});
gen_minus_rust.set({let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "pub use crate::" port) (gen-libname port lib) (display "::exports::*;" port) (newline port))
{{
// (display "pub use crate::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("pub use crate::"), port.clone(), ]);
// (gen-libname port lib)
gen_minus_libname.get().invoke(&[port.clone(), lib.clone(), ]);
// (display "::exports::*;" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("::exports::*;"), port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("IMPORT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message IMPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message IMPORT"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
                // (define (make-import-only lib names) (define (repr) (cons (quote IMPORT-ONLY) (cons lib names))) (define (transform func) (func self (lambda () (make-import-only lib names)))) (define (free-vars) (make-set)) (define (gen-libname port lib) (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib))))) (define (gen-imports port names) (if (null? names) (quote DONE) (begin (display (rustify-identifier (car names)) port) (display ", " port) (gen-imports port (cdr names))))) (define (gen-rust port) (display "pub use crate::" port) (gen-libname port lib) (display "::exports::{" port) (gen-imports port names) (display "};" port) (newline port)) (define (self msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))) self)
                globals::make_minus_import_minus_only.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let lib = args[0].clone();let names = args[1].clone();
// (letrec ((repr (lambda () (cons (quote IMPORT-ONLY) (cons lib names)))) (transform (lambda (func) (func self (lambda () (make-import-only lib names))))) (free-vars (lambda () (make-set))) (gen-libname (lambda (port lib) (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib)))))) (gen-imports (lambda (port names) (if (null? names) (quote DONE) (begin (display (rustify-identifier (car names)) port) (display ", " port) (gen-imports port (cdr names)))))) (gen-rust (lambda (port) (display "pub use crate::" port) (gen-libname port lib) (display "::exports::{" port) (gen-imports port names) (display "};" port) (newline port))) (self (lambda (msg . args) (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))))) self)
{let repr = Scm::uninitialized().into_boxed();
let transform = Scm::uninitialized().into_boxed();
let free_minus_vars = Scm::uninitialized().into_boxed();
let gen_minus_libname = Scm::uninitialized().into_boxed();
let gen_minus_imports = Scm::uninitialized().into_boxed();
let gen_minus_rust = Scm::uninitialized().into_boxed();
let self_ = Scm::uninitialized().into_boxed();
repr.set({let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (cons (quote IMPORT-ONLY) (cons lib names)))
{
// (cons (quote IMPORT-ONLY) (cons lib names))
imports::cons.with(|value| value.get()).invoke(&[Scm::symbol("IMPORT-ONLY"), 
// (cons lib names)
imports::cons.with(|value| value.get()).invoke(&[lib.clone(), names.clone(), ]), ])}})});
transform.set({let self_ = self_.clone();let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let func = args[0].clone();
// (letrec () (func self (lambda () (make-import-only lib names))))
{
// (func self (lambda () (make-import-only lib names)))
func.clone().invoke(&[self_.get(), {let lib = lib.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-import-only lib names))
{
// (make-import-only lib names)
globals::make_minus_import_minus_only.with(|value| value.get()).invoke(&[lib.clone(), names.clone(), ])}})}, ])}})});
free_minus_vars.set({Scm::func(move |args: &[Scm]|{if args.len() != 0{panic!("invalid arity")}
// (letrec () (make-set))
{
// (make-set)
globals::make_minus_set.with(|value| value.get()).invoke(&[])}})});
gen_minus_libname.set({let gen_minus_libname = gen_minus_libname.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let lib = args[1].clone();
// (letrec () (if (null? lib) (display "" port) (begin (display (rustify-libname (car lib)) port) (if (null? (cdr lib)) (display "" port) (display "::" port)) (gen-libname port (cdr lib)))))
{if (
// (null? lib)
imports::null_p.with(|value| value.get()).invoke(&[lib.clone(), ])).is_true() {
// (display "" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(""), port.clone(), ])} else {{
// (display (rustify-libname (car lib)) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-libname (car lib))
globals::rustify_minus_libname.with(|value| value.get()).invoke(&[
// (car lib)
imports::car.with(|value| value.get()).invoke(&[lib.clone(), ]), ]), port.clone(), ]);if (
// (null? (cdr lib))
imports::null_p.with(|value| value.get()).invoke(&[
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone(), ]), ])).is_true() {
// (display "" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(""), port.clone(), ])} else {
// (display "::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("::"), port.clone(), ])};
// (gen-libname port (cdr lib))
gen_minus_libname.get().invoke(&[port.clone(), 
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone(), ]), ])}}}})});
gen_minus_imports.set({let gen_minus_imports = gen_minus_imports.clone();Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let names = args[1].clone();
// (letrec () (if (null? names) (quote DONE) (begin (display (rustify-identifier (car names)) port) (display ", " port) (gen-imports port (cdr names)))))
{if (
// (null? names)
imports::null_p.with(|value| value.get()).invoke(&[names.clone(), ])).is_true() {Scm::symbol("DONE")} else {{
// (display (rustify-identifier (car names)) port)
imports::display.with(|value| value.get()).invoke(&[
// (rustify-identifier (car names))
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (car names)
imports::car.with(|value| value.get()).invoke(&[names.clone(), ]), ]), port.clone(), ]);
// (display ", " port)
imports::display.with(|value| value.get()).invoke(&[Scm::from(", "), port.clone(), ]);
// (gen-imports port (cdr names))
gen_minus_imports.get().invoke(&[port.clone(), 
// (cdr names)
imports::cdr.with(|value| value.get()).invoke(&[names.clone(), ]), ])}}}})});
gen_minus_rust.set({let gen_minus_libname = gen_minus_libname.clone();let lib = lib.clone();let gen_minus_imports = gen_minus_imports.clone();let names = names.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let port = args[0].clone();
// (letrec () (display "pub use crate::" port) (gen-libname port lib) (display "::exports::{" port) (gen-imports port names) (display "};" port) (newline port))
{{
// (display "pub use crate::" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("pub use crate::"), port.clone(), ]);
// (gen-libname port lib)
gen_minus_libname.get().invoke(&[port.clone(), lib.clone(), ]);
// (display "::exports::{" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("::exports::{"), port.clone(), ]);
// (gen-imports port names)
gen_minus_imports.get().invoke(&[port.clone(), names.clone(), ]);
// (display "};" port)
imports::display.with(|value| value.get()).invoke(&[Scm::from("};"), port.clone(), ]);
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ])}}})});
self_.set({let transform = transform.clone();let free_minus_vars = free_minus_vars.clone();let gen_minus_rust = gen_minus_rust.clone();Scm::func(move |args: &[Scm]|{if args.len() < 1{panic!("not enough args")}let msg = args[0].clone();let args_ = Scm::list(&args[1..]);
// (letrec () (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg))))
{
// (cond ((eq? (quote repr) msg) (print)) ((eq? (quote transform) msg) (transform (car args))) ((eq? (quote free-vars) msg) (free-vars)) ((eq? (quote kind) msg) (quote IMPORT)) ((eq? (quote gen-rust) msg) (gen-rust (car args))) (else (error "Unknown message IMPORT" msg)))
if (
// (eq? (quote repr) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("repr"), msg.clone(), ])).is_true() {
// (print)
globals::print.with(|value| value.get()).invoke(&[])} else {if (
// (eq? (quote transform) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("transform"), msg.clone(), ])).is_true() {
// (transform (car args))
transform.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {if (
// (eq? (quote free-vars) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("free-vars"), msg.clone(), ])).is_true() {
// (free-vars)
free_minus_vars.get().invoke(&[])} else {if (
// (eq? (quote kind) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("kind"), msg.clone(), ])).is_true() {Scm::symbol("IMPORT")} else {if (
// (eq? (quote gen-rust) msg)
imports::eq_p.with(|value| value.get()).invoke(&[Scm::symbol("gen-rust"), msg.clone(), ])).is_true() {
// (gen-rust (car args))
gen_minus_rust.get().invoke(&[
// (car args)
imports::car.with(|value| value.get()).invoke(&[args_.clone(), ]), ])} else {
// (error "Unknown message IMPORT" msg)
imports::error.with(|value| value.get()).invoke(&[Scm::from("Unknown message IMPORT"), msg.clone(), ])}}}}}}})});
self_.get()}})}));
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
                                        globals::print_minus_list.with(|value| value.get()).invoke(
                                            &[
                                                // (cdr seq)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[seq.clone()]),
                                            ],
                                        ),
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
                                            .invoke(&[
                                                Scm::symbol("free-vars"),
                                                local_minus_env.clone(),
                                            ]),
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
                // (define (rust-gen-global-defs port g) (if (null? g) (newline port) (if (global-imported? (cdar g)) (rust-gen-global-defs port (cdr g)) (begin (println port "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}") (rust-gen-global-defs port (cdr g))))))
                globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let g = args[1].clone();
// (letrec () (if (null? g) (newline port) (if (global-imported? (cdar g)) (rust-gen-global-defs port (cdr g)) (begin (println port "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}") (rust-gen-global-defs port (cdr g))))))
{if (
// (null? g)
imports::null_p.with(|value| value.get()).invoke(&[g.clone(), ])).is_true() {
// (newline port)
imports::newline.with(|value| value.get()).invoke(&[port.clone(), ])} else {if (
// (global-imported? (cdar g))
globals::global_minus_imported_p.with(|value| value.get()).invoke(&[
// (cdar g)
imports::cdar.with(|value| value.get()).invoke(&[g.clone(), ]), ])).is_true() {
// (rust-gen-global-defs port (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[port.clone(), 
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(), ]), ])} else {{
// (println port "thread_local!{#[allow(non_upper_case_globals)] pub static " (rustify-identifier (caar g)) ": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL " (caar g) "\"))}")
globals::println.with(|value| value.get()).invoke(&[port.clone(), Scm::from("thread_local!{#[allow(non_upper_case_globals)] pub static "), 
// (rustify-identifier (caar g))
globals::rustify_minus_identifier.with(|value| value.get()).invoke(&[
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone(), ]), ]), Scm::from(": Mut<Scm> = Mut::new(Scm::symbol(\"UNINITIALIZED GLOBAL "), 
// (caar g)
imports::caar.with(|value| value.get()).invoke(&[g.clone(), ]), Scm::from("\"))}"), ]);
// (rust-gen-global-defs port (cdr g))
globals::rust_minus_gen_minus_global_minus_defs.with(|value| value.get()).invoke(&[port.clone(), 
// (cdr g)
imports::cdr.with(|value| value.get()).invoke(&[g.clone(), ]), ])}}}}})}));
                // (define (rust-gen-modules port libs) (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list port (module-tree-children module-tree))))
                globals::rust_minus_gen_minus_modules.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 2{panic!("invalid arity")}let port = args[0].clone();let libs = args[1].clone();
// (letrec () (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list port (module-tree-children module-tree))))
{
// (let ((module-tree (make-module-tree-node (quote root)))) (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs) (rust-gen-module-tree-list port (module-tree-children module-tree)))
{let [module_minus_tree, ] = [
// (make-module-tree-node (quote root))
globals::make_minus_module_minus_tree_minus_node.with(|value| value.get()).invoke(&[Scm::symbol("root"), ]), ];{
// (for-each (lambda (lib) (module-tree-insert! module-tree (car lib) (cdr lib))) libs)
imports::for_minus_each.with(|value| value.get()).invoke(&[{let module_minus_tree = module_minus_tree.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let lib = args[0].clone();
// (letrec () (module-tree-insert! module-tree (car lib) (cdr lib)))
{
// (module-tree-insert! module-tree (car lib) (cdr lib))
globals::module_minus_tree_minus_insert_i.with(|value| value.get()).invoke(&[module_minus_tree.clone(), 
// (car lib)
imports::car.with(|value| value.get()).invoke(&[lib.clone(), ]), 
// (cdr lib)
imports::cdr.with(|value| value.get()).invoke(&[lib.clone(), ]), ])}})}, libs.clone(), ]);
// (rust-gen-module-tree-list port (module-tree-children module-tree))
globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| value.get()).invoke(&[port.clone(), 
// (module-tree-children module-tree)
globals::module_minus_tree_minus_children.with(|value| value.get()).invoke(&[module_minus_tree.clone(), ]), ])}}}})}));
                // (define (rust-gen-module-tree port node) (println port "pub mod " (rustify-libname (module-tree-name node)) " {") (if (module-tree-leaf? node) ((module-tree-libobj node) (quote gen-rust) port) (rust-gen-module-tree-list port (module-tree-children node))) (println port "}"))
                globals::rust_minus_gen_minus_module_minus_tree.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let port = args[0].clone();
                            let node = args[1].clone();
                            // (letrec () (println port "pub mod " (rustify-libname (module-tree-name node)) " {") (if (module-tree-leaf? node) ((module-tree-libobj node) (quote gen-rust) port) (rust-gen-module-tree-list port (module-tree-children node))) (println port "}"))
                            {
                                {
                                    // (println port "pub mod " (rustify-libname (module-tree-name node)) " {")
                                    globals::println.with(|value| value.get()).invoke(&[
                                        port.clone(),
                                        Scm::from("pub mod "),
                                        // (rustify-libname (module-tree-name node))
                                        globals::rustify_minus_libname
                                            .with(|value| value.get())
                                            .invoke(&[
                                                // (module-tree-name node)
                                                globals::module_minus_tree_minus_name
                                                    .with(|value| value.get())
                                                    .invoke(&[node.clone()]),
                                            ]),
                                        Scm::from(" {"),
                                    ]);
                                    if (
                                        // (module-tree-leaf? node)
                                        globals::module_minus_tree_minus_leaf_p
                                            .with(|value| value.get())
                                            .invoke(&[node.clone()])
                                    )
                                    .is_true()
                                    {
                                        // ((module-tree-libobj node) (quote gen-rust) port)

                                        // (module-tree-libobj node)
                                        globals::module_minus_tree_minus_libobj
                                            .with(|value| value.get())
                                            .invoke(&[node.clone()])
                                            .invoke(&[Scm::symbol("gen-rust"), port.clone()])
                                    } else {
                                        // (rust-gen-module-tree-list port (module-tree-children node))
                                        globals::rust_minus_gen_minus_module_minus_tree_minus_list
                                            .with(|value| value.get())
                                            .invoke(&[
                                                port.clone(),
                                                // (module-tree-children node)
                                                globals::module_minus_tree_minus_children
                                                    .with(|value| value.get())
                                                    .invoke(&[node.clone()]),
                                            ])
                                    };
                                    // (println port "}")
                                    globals::println
                                        .with(|value| value.get())
                                        .invoke(&[port.clone(), Scm::from("}")])
                                }
                            }
                        })
                    })
                });
                // (define (rust-gen-module-tree-list port nodes) (for-each (lambda (child) (rust-gen-module-tree port child)) nodes))
                globals::rust_minus_gen_minus_module_minus_tree_minus_list.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let port = args[0].clone();
                            let nodes = args[1].clone();
                            // (letrec () (for-each (lambda (child) (rust-gen-module-tree port child)) nodes))
                            {
                                // (for-each (lambda (child) (rust-gen-module-tree port child)) nodes)
                                imports::for_minus_each.with(|value| value.get()).invoke(&[
                                    {
                                        let port = port.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let child = args[0].clone();
                                            // (letrec () (rust-gen-module-tree port child))
                                            {
                                                // (rust-gen-module-tree port child)
                                                globals::rust_minus_gen_minus_module_minus_tree
                                                    .with(|value| value.get())
                                                    .invoke(&[port.clone(), child.clone()])
                                            }
                                        })
                                    },
                                    nodes.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (make-module-tree-node name) (cons name (quote ())))
                globals::make_minus_module_minus_tree_minus_node.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let name = args[0].clone();
                            // (letrec () (cons name (quote ())))
                            {
                                // (cons name (quote ()))
                                imports::cons
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), Scm::Nil])
                            }
                        })
                    })
                });
                // (define (make-module-tree-leaf name lib) (cons name lib))
                globals::make_minus_module_minus_tree_minus_leaf.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let name = args[0].clone();
                            let lib = args[1].clone();
                            // (letrec () (cons name lib))
                            {
                                // (cons name lib)
                                imports::cons
                                    .with(|value| value.get())
                                    .invoke(&[name.clone(), lib.clone()])
                            }
                        })
                    })
                });
                // (define (module-tree-leaf? node) (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node)))))
                globals::module_minus_tree_minus_leaf_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            // (letrec () (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node)))))
                            {
                                // (and (pair? node) (symbol? (car node)) (not (null? (cdr node))) (not (pair? (cdr node))))
                                if (
                                    // (pair? node)
                                    imports::pair_p
                                        .with(|value| value.get())
                                        .invoke(&[node.clone()])
                                )
                                .is_true()
                                {
                                    if (
                                        // (symbol? (car node))
                                        imports::symbol_p.with(|value| value.get()).invoke(&[
                                            // (car node)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[node.clone()]),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        if (
                                            // (not (null? (cdr node)))
                                            imports::not.with(|value| value.get()).invoke(&[
                                                // (null? (cdr node))
                                                imports::null_p.with(|value| value.get()).invoke(
                                                    &[
                                                        // (cdr node)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[node.clone()]),
                                                    ],
                                                ),
                                            ])
                                        )
                                        .is_true()
                                        {
                                            // (not (pair? (cdr node)))
                                            imports::not.with(|value| value.get()).invoke(&[
                                                // (pair? (cdr node))
                                                imports::pair_p.with(|value| value.get()).invoke(
                                                    &[
                                                        // (cdr node)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[node.clone()]),
                                                    ],
                                                ),
                                            ])
                                        } else {
                                            Scm::False
                                        }
                                    } else {
                                        Scm::False
                                    }
                                } else {
                                    Scm::False
                                }
                            }
                        })
                    })
                });
                // (define (module-tree-name node) (car node))
                globals::module_minus_tree_minus_name.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            // (letrec () (car node))
                            {
                                // (car node)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[node.clone()])
                            }
                        })
                    })
                });
                // (define (module-tree-children node) (cdr node))
                globals::module_minus_tree_minus_children.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            // (letrec () (cdr node))
                            {
                                // (cdr node)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[node.clone()])
                            }
                        })
                    })
                });
                // (define (module-tree-libobj node) (cdr node))
                globals::module_minus_tree_minus_libobj.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            // (letrec () (cdr node))
                            {
                                // (cdr node)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[node.clone()])
                            }
                        })
                    })
                });
                // (define (module-tree-set-children! node children) (set-cdr! node children))
                globals::module_minus_tree_minus_set_minus_children_i.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            let children = args[1].clone();
                            // (letrec () (set-cdr! node children))
                            {
                                // (set-cdr! node children)
                                imports::set_minus_cdr_i
                                    .with(|value| value.get())
                                    .invoke(&[node.clone(), children.clone()])
                            }
                        })
                    })
                });
                // (define (module-tree-find-child node name) (if (module-tree-leaf? node) (error "called (module-tree-find-child) on leaf node" name node)) (assq name (module-tree-children node)))
                globals::module_minus_tree_minus_find_minus_child.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            let name = args[1].clone();
                            // (letrec () (if (module-tree-leaf? node) (error "called (module-tree-find-child) on leaf node" name node)) (assq name (module-tree-children node)))
                            {
                                {
                                    if (
                                        // (module-tree-leaf? node)
                                        globals::module_minus_tree_minus_leaf_p
                                            .with(|value| value.get())
                                            .invoke(&[node.clone()])
                                    )
                                    .is_true()
                                    {
                                        // (error "called (module-tree-find-child) on leaf node" name node)
                                        imports::error.with(|value| value.get()).invoke(&[
                                            Scm::from(
                                                "called (module-tree-find-child) on leaf node",
                                            ),
                                            name.clone(),
                                            node.clone(),
                                        ])
                                    } else {
                                        Scm::symbol("*UNSPECIFIED*")
                                    };
                                    // (assq name (module-tree-children node))
                                    imports::assq.with(|value| value.get()).invoke(&[
                                        name.clone(),
                                        // (module-tree-children node)
                                        globals::module_minus_tree_minus_children
                                            .with(|value| value.get())
                                            .invoke(&[node.clone()]),
                                    ])
                                }
                            }
                        })
                    })
                });
                // (define (module-tree-append-child! node child) (module-tree-set-children! node (cons child (module-tree-children node))))
                globals::module_minus_tree_minus_append_minus_child_i.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let node = args[0].clone();
                            let child = args[1].clone();
                            // (letrec () (module-tree-set-children! node (cons child (module-tree-children node))))
                            {
                                // (module-tree-set-children! node (cons child (module-tree-children node)))
                                globals::module_minus_tree_minus_set_minus_children_i
                                    .with(|value| value.get())
                                    .invoke(&[
                                        node.clone(),
                                        // (cons child (module-tree-children node))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            child.clone(),
                                            // (module-tree-children node)
                                            globals::module_minus_tree_minus_children
                                                .with(|value| value.get())
                                                .invoke(&[node.clone()]),
                                        ]),
                                    ])
                            }
                        })
                    })
                });
                // (define (module-tree-insert! tree libname libobj) (if (null? libname) (error "invalid insert")) (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))))))
                globals::module_minus_tree_minus_insert_i.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 3{panic!("invalid arity")}let tree = args[0].clone();let libname = args[1].clone();let libobj = args[2].clone();
// (letrec () (if (null? libname) (error "invalid insert")) (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))))))
{{if (
// (null? libname)
imports::null_p.with(|value| value.get()).invoke(&[libname.clone(), ])).is_true() {
// (error "invalid insert")
imports::error.with(|value| value.get()).invoke(&[Scm::from("invalid insert"), ])} else {Scm::symbol("*UNSPECIFIED*")};
// (let ((child (module-tree-find-child tree (car libname)))) (if child (module-tree-insert! child (cdr libname) libobj) (if (null? (cdr libname)) (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj)) (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node)))))
{let [child, ] = [
// (module-tree-find-child tree (car libname))
globals::module_minus_tree_minus_find_minus_child.with(|value| value.get()).invoke(&[tree.clone(), 
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(), ]), ]), ];if (child.clone()).is_true() {
// (module-tree-insert! child (cdr libname) libobj)
globals::module_minus_tree_minus_insert_i.with(|value| value.get()).invoke(&[child.clone(), 
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(), ]), libobj.clone(), ])} else {if (
// (null? (cdr libname))
imports::null_p.with(|value| value.get()).invoke(&[
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(), ]), ])).is_true() {
// (module-tree-append-child! tree (make-module-tree-leaf (car libname) libobj))
globals::module_minus_tree_minus_append_minus_child_i.with(|value| value.get()).invoke(&[tree.clone(), 
// (make-module-tree-leaf (car libname) libobj)
globals::make_minus_module_minus_tree_minus_leaf.with(|value| value.get()).invoke(&[
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(), ]), libobj.clone(), ]), ])} else {
// (let ((new-node (make-module-tree-node (car libname)))) (module-tree-insert! new-node (cdr libname) libobj) (module-tree-append-child! tree new-node))
{let [new_minus_node, ] = [
// (make-module-tree-node (car libname))
globals::make_minus_module_minus_tree_minus_node.with(|value| value.get()).invoke(&[
// (car libname)
imports::car.with(|value| value.get()).invoke(&[libname.clone(), ]), ]), ];{
// (module-tree-insert! new-node (cdr libname) libobj)
globals::module_minus_tree_minus_insert_i.with(|value| value.get()).invoke(&[new_minus_node.clone(), 
// (cdr libname)
imports::cdr.with(|value| value.get()).invoke(&[libname.clone(), ]), libobj.clone(), ]);
// (module-tree-append-child! tree new-node)
globals::module_minus_tree_minus_append_minus_child_i.with(|value| value.get()).invoke(&[tree.clone(), new_minus_node.clone(), ])}}}}}}}})}));
                // (define (rust-block port code) (display "{" port) (code) (display "}" port))
                globals::rust_minus_block.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let port = args[0].clone();
                            let code = args[1].clone();
                            // (letrec () (display "{" port) (code) (display "}" port))
                            {
                                {
                                    // (display "{" port)
                                    imports::display
                                        .with(|value| value.get())
                                        .invoke(&[Scm::from("{"), port.clone()]);
                                    // (code)
                                    code.clone().invoke(&[]);
                                    // (display "}" port)
                                    imports::display
                                        .with(|value| value.get())
                                        .invoke(&[Scm::from("}"), port.clone()])
                                }
                            }
                        })
                    })
                });
                // (define (rustify-identifier name) (define (char-map ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch))))) (define (append-all strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))) (cond ((eq? name (quote args)) "args_") ((eq? name (quote fn)) "fn_") ((eq? name (quote loop)) "loop_") ((eq? name (quote let)) "let_") ((eq? name (quote mut)) "mut_") ((eq? name (quote ref)) "ref_") ((eq? name (quote self)) "self_") (else (append-all (map char-map (string->list (symbol->string name)))))))
                globals::rustify_minus_identifier.with(|value| value.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let name = args[0].clone();
// (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((eq? name (quote args)) "args_") ((eq? name (quote fn)) "fn_") ((eq? name (quote loop)) "loop_") ((eq? name (quote let)) "let_") ((eq? name (quote mut)) "mut_") ((eq? name (quote ref)) "ref_") ((eq? name (quote self)) "self_") (else (append-all (map char-map (string->list (symbol->string name)))))))
{let char_minus_map = Scm::uninitialized().into_boxed();
let append_minus_all = Scm::uninitialized().into_boxed();
char_minus_map.set({Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let ch = args[0].clone();
// (letrec () (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch)))))
{
// (cond ((eq? ch #\_) "__") ((eq? ch #\?) "_p") ((eq? ch #\!) "_i") ((eq? ch #\<) "_l_") ((eq? ch #\>) "_g_") ((eq? ch #\=) "_e_") ((eq? ch #\-) "_minus_") ((eq? ch #\+) "_plus_") ((eq? ch #\*) "_star_") ((eq? ch #\/) "_slash_") (else (list->string (list ch))))
if (
// (eq? ch #\_)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('_'), ])).is_true() {Scm::from("__")} else {if (
// (eq? ch #\?)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('?'), ])).is_true() {Scm::from("_p")} else {if (
// (eq? ch #\!)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('!'), ])).is_true() {Scm::from("_i")} else {if (
// (eq? ch #\<)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('<'), ])).is_true() {Scm::from("_l_")} else {if (
// (eq? ch #\>)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('>'), ])).is_true() {Scm::from("_g_")} else {if (
// (eq? ch #\=)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('='), ])).is_true() {Scm::from("_e_")} else {if (
// (eq? ch #\-)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('-'), ])).is_true() {Scm::from("_minus_")} else {if (
// (eq? ch #\+)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('+'), ])).is_true() {Scm::from("_plus_")} else {if (
// (eq? ch #\*)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('*'), ])).is_true() {Scm::from("_star_")} else {if (
// (eq? ch #\/)
imports::eq_p.with(|value| value.get()).invoke(&[ch.clone(), Scm::char('/'), ])).is_true() {Scm::from("_slash_")} else {
// (list->string (list ch))
imports::list_minus__g_string.with(|value| value.get()).invoke(&[
// (list ch)
imports::list.with(|value| value.get()).invoke(&[ch.clone(), ]), ])}}}}}}}}}}}})});
append_minus_all.set({let append_minus_all = append_minus_all.clone();Scm::func(move |args: &[Scm]|{if args.len() != 1{panic!("invalid arity")}let strs = args[0].clone();
// (letrec () (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))
{if (
// (null? strs)
imports::null_p.with(|value| value.get()).invoke(&[strs.clone(), ])).is_true() {Scm::from("")} else {
// (string-append (car strs) (append-all (cdr strs)))
imports::string_minus_append.with(|value| value.get()).invoke(&[
// (car strs)
imports::car.with(|value| value.get()).invoke(&[strs.clone(), ]), 
// (append-all (cdr strs))
append_minus_all.get().invoke(&[
// (cdr strs)
imports::cdr.with(|value| value.get()).invoke(&[strs.clone(), ]), ]), ])}}})});

// (cond ((eq? name (quote args)) "args_") ((eq? name (quote fn)) "fn_") ((eq? name (quote loop)) "loop_") ((eq? name (quote let)) "let_") ((eq? name (quote mut)) "mut_") ((eq? name (quote ref)) "ref_") ((eq? name (quote self)) "self_") (else (append-all (map char-map (string->list (symbol->string name))))))
if (
// (eq? name (quote args))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("args"), ])).is_true() {Scm::from("args_")} else {if (
// (eq? name (quote fn))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("fn"), ])).is_true() {Scm::from("fn_")} else {if (
// (eq? name (quote loop))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("loop"), ])).is_true() {Scm::from("loop_")} else {if (
// (eq? name (quote let))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("let"), ])).is_true() {Scm::from("let_")} else {if (
// (eq? name (quote mut))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("mut"), ])).is_true() {Scm::from("mut_")} else {if (
// (eq? name (quote ref))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("ref"), ])).is_true() {Scm::from("ref_")} else {if (
// (eq? name (quote self))
imports::eq_p.with(|value| value.get()).invoke(&[name.clone(), Scm::symbol("self"), ])).is_true() {Scm::from("self_")} else {
// (append-all (map char-map (string->list (symbol->string name))))
append_minus_all.get().invoke(&[
// (map char-map (string->list (symbol->string name)))
imports::map.with(|value| value.get()).invoke(&[char_minus_map.get(), 
// (string->list (symbol->string name))
imports::string_minus__g_list.with(|value| value.get()).invoke(&[
// (symbol->string name)
imports::symbol_minus__g_string.with(|value| value.get()).invoke(&[name.clone(), ]), ]), ]), ])}}}}}}}}})}));
                // (define (rustify-libname name) (define (char-map ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch))))) (define (append-all strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list (symbol->string name)))))))
                globals::rustify_minus_libname.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let name = args[0].clone();
                            // (letrec ((char-map (lambda (ch) (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))) (append-all (lambda (strs) (if (null? strs) "" (string-append (car strs) (append-all (cdr strs))))))) (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list (symbol->string name)))))))
                            {
                                let char_minus_map = Scm::uninitialized().into_boxed();
                                let append_minus_all = Scm::uninitialized().into_boxed();
                                char_minus_map.set({
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let ch = args[0].clone();
                                        // (letrec () (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch)))))
                                        {
                                            // (cond ((eq? ch #\_) "__") ((eq? ch #\-) "_") (else (list->string (list ch))))
                                            if (
                                                // (eq? ch #\_)
                                                imports::eq_p
                                                    .with(|value| value.get())
                                                    .invoke(&[ch.clone(), Scm::char('_')])
                                            )
                                            .is_true()
                                            {
                                                Scm::from("__")
                                            } else {
                                                if (
                                                    // (eq? ch #\-)
                                                    imports::eq_p
                                                        .with(|value| value.get())
                                                        .invoke(&[ch.clone(), Scm::char('-')])
                                                )
                                                .is_true()
                                                {
                                                    Scm::from("_")
                                                } else {
                                                    // (list->string (list ch))
                                                    imports::list_minus__g_string
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (list ch)
                                                            imports::list
                                                                .with(|value| value.get())
                                                                .invoke(&[ch.clone()]),
                                                        ])
                                                }
                                            }
                                        }
                                    })
                                });
                                append_minus_all.set({
                                    let append_minus_all = append_minus_all.clone();
                                    Scm::func(move |args: &[Scm]| {
                                        if args.len() != 1 {
                                            panic!("invalid arity")
                                        }
                                        let strs = args[0].clone();
                                        // (letrec () (if (null? strs) "" (string-append (car strs) (append-all (cdr strs)))))
                                        {
                                            if (
                                                // (null? strs)
                                                imports::null_p
                                                    .with(|value| value.get())
                                                    .invoke(&[strs.clone()])
                                            )
                                            .is_true()
                                            {
                                                Scm::from("")
                                            } else {
                                                // (string-append (car strs) (append-all (cdr strs)))
                                                imports::string_minus_append
                                                    .with(|value| value.get())
                                                    .invoke(&[
                                                        // (car strs)
                                                        imports::car
                                                            .with(|value| value.get())
                                                            .invoke(&[strs.clone()]),
                                                        // (append-all (cdr strs))
                                                        append_minus_all.get().invoke(&[
                                                            // (cdr strs)
                                                            imports::cdr
                                                                .with(|value| value.get())
                                                                .invoke(&[strs.clone()]),
                                                        ]),
                                                    ])
                                            }
                                        }
                                    })
                                });

                                // (cond ((eq? name (quote fn)) "fn_") (else (append-all (map char-map (string->list (symbol->string name))))))
                                if (
                                    // (eq? name (quote fn))
                                    imports::eq_p
                                        .with(|value| value.get())
                                        .invoke(&[name.clone(), Scm::symbol("fn")])
                                )
                                .is_true()
                                {
                                    Scm::from("fn_")
                                } else {
                                    // (append-all (map char-map (string->list (symbol->string name))))
                                    append_minus_all.get().invoke(&[
                                        // (map char-map (string->list (symbol->string name)))
                                        imports::map.with(|value| value.get()).invoke(&[
                                            char_minus_map.get(),
                                            // (string->list (symbol->string name))
                                            imports::string_minus__g_list
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (symbol->string name)
                                                    imports::symbol_minus__g_string
                                                        .with(|value| value.get())
                                                        .invoke(&[name.clone()]),
                                                ]),
                                        ]),
                                    ])
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
                                } else {
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
                                        // (lookup name (cdr env))
                                        globals::lookup.with(|value| value.get()).invoke(&[
                                            name.clone(),
                                            // (cdr env)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[env.clone()]),
                                        ])
                                    } else {
                                        if (
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
                                        imports::set_minus_cdr_i.with(|value| value.get()).invoke(
                                            &[
                                                genv.clone(),
                                                // (cons var (cdr genv))
                                                imports::cons.with(|value| value.get()).invoke(&[
                                                    var.clone(),
                                                    // (cdr genv)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[genv.clone()]),
                                                ]),
                                            ],
                                        );
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
                                } else {
                                    if (
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
                                                            imports::cons
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (new-import (car name*))
                                                                    globals::new_minus_import
                                                                        .with(|value| value.get())
                                                                        .invoke(&[
                                                                            // (car name*)
                                                                            imports::car
                                                                                .with(|value| {
                                                                                    value.get()
                                                                                })
                                                                                .invoke(&[
                                                                                    name_star_
                                                                                        .clone(),
                                                                                ]),
                                                                        ]),
                                                                    // (cdr genv)
                                                                    imports::cdr
                                                                        .with(|value| value.get())
                                                                        .invoke(&[genv.clone()]),
                                                                ]),
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
                                } else {
                                    if (
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
                                    globals::variable.with(|value| value.get()).invoke(&[
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
                                    globals::variable.with(|value| value.get()).invoke(&[
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
                                    globals::variable.with(|value| value.get()).invoke(&[
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
                                    globals::variable.with(|value| value.get()).invoke(&[
                                        Scm::symbol("BOXED-REF"),
                                        Scm::symbol("BOXED-SET"),
                                        Scm::False,
                                    ]),
                                ])
                            }
                        })
                    })
                });
                // (define (variable getter setter mut?) (list getter setter mut?))
                globals::variable.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 3 {
                                panic!("invalid arity")
                            }
                            let getter = args[0].clone();
                            let setter = args[1].clone();
                            let mut_p = args[2].clone();
                            // (letrec () (list getter setter mut?))
                            {
                                // (list getter setter mut?)
                                imports::list.with(|value| value.get()).invoke(&[
                                    getter.clone(),
                                    setter.clone(),
                                    mut_p.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (variable-getter var) (car var))
                globals::variable_minus_getter.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (car var))
                            {
                                // (car var)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[var.clone()])
                            }
                        })
                    })
                });
                // (define (variable-setter var) (cadr var))
                globals::variable_minus_setter.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (cadr var))
                            {
                                // (cadr var)
                                imports::cadr
                                    .with(|value| value.get())
                                    .invoke(&[var.clone()])
                            }
                        })
                    })
                });
                // (define (variable-mut? var) (caddr var))
                globals::variable_minus_mut_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (caddr var))
                            {
                                // (caddr var)
                                imports::caddr
                                    .with(|value| value.get())
                                    .invoke(&[var.clone()])
                            }
                        })
                    })
                });
                // (define (variable-set-mutable! var) (set-car! (cddr var) #t))
                globals::variable_minus_set_minus_mutable_i.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (set-car! (cddr var) #t))
                            {
                                // (set-car! (cddr var) #t)
                                imports::set_minus_car_i.with(|value| value.get()).invoke(&[
                                    // (cddr var)
                                    imports::cddr
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()]),
                                    Scm::True,
                                ])
                            }
                        })
                    })
                });
                // (define (variable-set-getter! var getter) (set-car! var getter))
                globals::variable_minus_set_minus_getter_i.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            let getter = args[1].clone();
                            // (letrec () (set-car! var getter))
                            {
                                // (set-car! var getter)
                                imports::set_minus_car_i
                                    .with(|value| value.get())
                                    .invoke(&[var.clone(), getter.clone()])
                            }
                        })
                    })
                });
                // (define (variable-set-setter! var setter) (set-car! (cdr var) setter))
                globals::variable_minus_set_minus_setter_i.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            let setter = args[1].clone();
                            // (letrec () (set-car! (cdr var) setter))
                            {
                                // (set-car! (cdr var) setter)
                                imports::set_minus_car_i.with(|value| value.get()).invoke(&[
                                    // (cdr var)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()]),
                                    setter.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (global-imported? var) (eq? (quote IMPORT-REF) (car var)))
                globals::global_minus_imported_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (eq? (quote IMPORT-REF) (car var)))
                            {
                                // (eq? (quote IMPORT-REF) (car var))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("IMPORT-REF"),
                                    // (car var)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (global-regular? var) (eq? (quote GLOBAL-REF) (car var)))
                globals::global_minus_regular_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let var = args[0].clone();
                            // (letrec () (eq? (quote GLOBAL-REF) (car var)))
                            {
                                // (eq? (quote GLOBAL-REF) (car var))
                                imports::eq_p.with(|value| value.get()).invoke(&[
                                    Scm::symbol("GLOBAL-REF"),
                                    // (car var)
                                    imports::car
                                        .with(|value| value.get())
                                        .invoke(&[var.clone()]),
                                ])
                            }
                        })
                    })
                });
                // (define (atom? x) (if (pair? x) #f #t))
                globals::atom_p.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let x = args[0].clone();
                            // (letrec () (if (pair? x) #f #t))
                            {
                                if (
                                    // (pair? x)
                                    imports::pair_p
                                        .with(|value| value.get())
                                        .invoke(&[x.clone()])
                                )
                                .is_true()
                                {
                                    Scm::False
                                } else {
                                    Scm::True
                                }
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
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-params")]),
                                                        // (node (quote get-vars))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-vars")]),
                                                        // (node (quote get-params))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-params")]),
                                                        // (node (quote get-vars))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-vars")]),
                                                        // (node (quote get-body))
                                                        node.clone()
                                                            .invoke(&[Scm::symbol("get-body")]),
                                                    ])
                                            } else {
                                                if (
                                                    // (eq? (node (quote kind)) (quote VARARG-ABSTRACTION))
                                                    imports::eq_p.with(|value| value.get()).invoke(
                                                        &[
                                                            // (node (quote kind))
                                                            node.clone()
                                                                .invoke(&[Scm::symbol("kind")]),
                                                            Scm::symbol("VARARG-ABSTRACTION"),
                                                        ],
                                                    )
                                                )
                                                .is_true()
                                                {
                                                    // (boxify-vararg-abstraction (node (quote get-params)) (node (quote get-vararg)) (node (quote get-vars)) (node (quote get-varvar)) (cons (node (quote get-vararg)) (node (quote get-params))) (cons (node (quote get-varvar)) (node (quote get-vars))) (node (quote get-body)))
                                                    globals::boxify_minus_vararg_minus_abstraction
                                                        .with(|value| value.get())
                                                        .invoke(&[
                                                            // (node (quote get-params))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-params",
                                                            )]),
                                                            // (node (quote get-vararg))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-vararg",
                                                            )]),
                                                            // (node (quote get-vars))
                                                            node.clone()
                                                                .invoke(&[Scm::symbol("get-vars")]),
                                                            // (node (quote get-varvar))
                                                            node.clone().invoke(&[Scm::symbol(
                                                                "get-varvar",
                                                            )]),
                                                            // (cons (node (quote get-vararg)) (node (quote get-params)))
                                                            imports::cons
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (node (quote get-vararg))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-vararg"),
                                                                    ]),
                                                                    // (node (quote get-params))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-params"),
                                                                    ]),
                                                                ]),
                                                            // (cons (node (quote get-varvar)) (node (quote get-vars)))
                                                            imports::cons
                                                                .with(|value| value.get())
                                                                .invoke(&[
                                                                    // (node (quote get-varvar))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-varvar"),
                                                                    ]),
                                                                    // (node (quote get-vars))
                                                                    node.clone().invoke(&[
                                                                        Scm::symbol("get-vars"),
                                                                    ]),
                                                                ]),
                                                            // (node (quote get-body))
                                                            node.clone()
                                                                .invoke(&[Scm::symbol("get-body")]),
                                                        ])
                                                } else {
                                                    // (ignore)
                                                    ignore.clone().invoke(&[])
                                                }
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
                                } else {
                                    if (
                                        // (variable-mut? (car var*))
                                        globals::variable_minus_mut_p
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
                                            globals::variable_minus_set_minus_setter_i
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (car var*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[var_star_.clone()]),
                                                    Scm::symbol("BOXED-SET"),
                                                ]);
                                            // (variable-set-getter! (car var*) (quote BOXED-REF))
                                            globals::variable_minus_set_minus_getter_i
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
                                } else {
                                    if (
                                        // (variable-mut? (car var*))
                                        globals::variable_minus_mut_p
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
                                            globals::variable_minus_set_minus_setter_i
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (car var*)
                                                    imports::car
                                                        .with(|value| value.get())
                                                        .invoke(&[var_star_.clone()]),
                                                    Scm::symbol("BOXED-SET"),
                                                ]);
                                            // (variable-set-getter! (car var*) (quote BOXED-REF))
                                            globals::variable_minus_set_minus_getter_i
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
                            }
                        })
                    })
                });
                // (define (make-set) (quote ()))
                globals::make_minus_set.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 0 {
                                panic!("invalid arity")
                            }
                            // (letrec () (quote ()))
                            {
                                Scm::Nil
                            }
                        })
                    })
                });
                // (define (set-add set item) (cond ((null? set) (cons item (quote ()))) ((equal? (car set) item) set) (else (cons (car set) (set-add (cdr set) item)))))
                globals::set_minus_add.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let set = args[0].clone();
                            let item = args[1].clone();
                            // (letrec () (cond ((null? set) (cons item (quote ()))) ((equal? (car set) item) set) (else (cons (car set) (set-add (cdr set) item)))))
                            {
                                // (cond ((null? set) (cons item (quote ()))) ((equal? (car set) item) set) (else (cons (car set) (set-add (cdr set) item))))
                                if (
                                    // (null? set)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[set.clone()])
                                )
                                .is_true()
                                {
                                    // (cons item (quote ()))
                                    imports::cons
                                        .with(|value| value.get())
                                        .invoke(&[item.clone(), Scm::Nil])
                                } else {
                                    if (
                                        // (equal? (car set) item)
                                        imports::equal_p.with(|value| value.get()).invoke(&[
                                            // (car set)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[set.clone()]),
                                            item.clone(),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        set.clone()
                                    } else {
                                        // (cons (car set) (set-add (cdr set) item))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (car set)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[set.clone()]),
                                            // (set-add (cdr set) item)
                                            globals::set_minus_add
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdr set)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[set.clone()]),
                                                    item.clone(),
                                                ]),
                                        ])
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (set-remove set item) (cond ((null? set) (quote ())) ((equal? (car set) item) (cdr set)) (else (cons (car set) (set-remove (cdr set) item)))))
                globals::set_minus_remove.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let set = args[0].clone();
                            let item = args[1].clone();
                            // (letrec () (cond ((null? set) (quote ())) ((equal? (car set) item) (cdr set)) (else (cons (car set) (set-remove (cdr set) item)))))
                            {
                                // (cond ((null? set) (quote ())) ((equal? (car set) item) (cdr set)) (else (cons (car set) (set-remove (cdr set) item))))
                                if (
                                    // (null? set)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[set.clone()])
                                )
                                .is_true()
                                {
                                    Scm::Nil
                                } else {
                                    if (
                                        // (equal? (car set) item)
                                        imports::equal_p.with(|value| value.get()).invoke(&[
                                            // (car set)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[set.clone()]),
                                            item.clone(),
                                        ])
                                    )
                                    .is_true()
                                    {
                                        // (cdr set)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[set.clone()])
                                    } else {
                                        // (cons (car set) (set-remove (cdr set) item))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (car set)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[set.clone()]),
                                            // (set-remove (cdr set) item)
                                            globals::set_minus_remove
                                                .with(|value| value.get())
                                                .invoke(&[
                                                    // (cdr set)
                                                    imports::cdr
                                                        .with(|value| value.get())
                                                        .invoke(&[set.clone()]),
                                                    item.clone(),
                                                ]),
                                        ])
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (set-add* set item*) (set-do* set-add set item*))
                globals::set_minus_add_star_.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let set = args[0].clone();
                            let item_star_ = args[1].clone();
                            // (letrec () (set-do* set-add set item*))
                            {
                                // (set-do* set-add set item*)
                                globals::set_minus_do_star_
                                    .with(|value| value.get())
                                    .invoke(&[
                                        globals::set_minus_add.with(|value| value.get()),
                                        set.clone(),
                                        item_star_.clone(),
                                    ])
                            }
                        })
                    })
                });
                // (define (set-remove* set item*) (set-do* set-remove set item*))
                globals::set_minus_remove_star_.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let set = args[0].clone();
                            let item_star_ = args[1].clone();
                            // (letrec () (set-do* set-remove set item*))
                            {
                                // (set-do* set-remove set item*)
                                globals::set_minus_do_star_
                                    .with(|value| value.get())
                                    .invoke(&[
                                        globals::set_minus_remove.with(|value| value.get()),
                                        set.clone(),
                                        item_star_.clone(),
                                    ])
                            }
                        })
                    })
                });
                // (define (set-do* func set item*) (if (null? item*) set (set-do* func (func set (car item*)) (cdr item*))))
                globals::set_minus_do_star_.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 3 {
                                panic!("invalid arity")
                            }
                            let func = args[0].clone();
                            let set = args[1].clone();
                            let item_star_ = args[2].clone();
                            // (letrec () (if (null? item*) set (set-do* func (func set (car item*)) (cdr item*))))
                            {
                                if (
                                    // (null? item*)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[item_star_.clone()])
                                )
                                .is_true()
                                {
                                    set.clone()
                                } else {
                                    // (set-do* func (func set (car item*)) (cdr item*))
                                    globals::set_minus_do_star_
                                        .with(|value| value.get())
                                        .invoke(&[
                                            func.clone(),
                                            // (func set (car item*))
                                            func.clone().invoke(&[
                                                set.clone(),
                                                // (car item*)
                                                imports::car
                                                    .with(|value| value.get())
                                                    .invoke(&[item_star_.clone()]),
                                            ]),
                                            // (cdr item*)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[item_star_.clone()]),
                                        ])
                                }
                            }
                        })
                    })
                });
                // (define (set-union set1 set2) (cond ((null? set1) set2) ((null? set2) set1) (else (set-add* set1 set2))))
                globals::set_minus_union.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let set1 = args[0].clone();
                            let set2 = args[1].clone();
                            // (letrec () (cond ((null? set1) set2) ((null? set2) set1) (else (set-add* set1 set2))))
                            {
                                // (cond ((null? set1) set2) ((null? set2) set1) (else (set-add* set1 set2)))
                                if (
                                    // (null? set1)
                                    imports::null_p
                                        .with(|value| value.get())
                                        .invoke(&[set1.clone()])
                                )
                                .is_true()
                                {
                                    set2.clone()
                                } else {
                                    if (
                                        // (null? set2)
                                        imports::null_p
                                            .with(|value| value.get())
                                            .invoke(&[set2.clone()])
                                    )
                                    .is_true()
                                    {
                                        set1.clone()
                                    } else {
                                        // (set-add* set1 set2)
                                        globals::set_minus_add_star_
                                            .with(|value| value.get())
                                            .invoke(&[set1.clone(), set2.clone()])
                                    }
                                }
                            }
                        })
                    })
                });
                // (define (println port . args) (for-each (lambda (a) (display a port)) args) (newline port))
                globals::println.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 1 {
                                panic!("not enough args")
                            }
                            let port = args[0].clone();
                            let args_ = Scm::list(&args[1..]);
                            // (letrec () (for-each (lambda (a) (display a port)) args) (newline port))
                            {
                                {
                                    // (for-each (lambda (a) (display a port)) args)
                                    imports::for_minus_each.with(|value| value.get()).invoke(&[
                                        {
                                            let port = port.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let a = args[0].clone();
                                                // (letrec () (display a port))
                                                {
                                                    // (display a port)
                                                    imports::display
                                                        .with(|value| value.get())
                                                        .invoke(&[a.clone(), port.clone()])
                                                }
                                            })
                                        },
                                        args_.clone(),
                                    ]);
                                    // (newline port)
                                    imports::newline
                                        .with(|value| value.get())
                                        .invoke(&[port.clone()])
                                }
                            }
                        })
                    })
                });
                // (define (print port . args) (for-each (lambda (a) (display a port)) args))
                globals::print.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 1 {
                                panic!("not enough args")
                            }
                            let port = args[0].clone();
                            let args_ = Scm::list(&args[1..]);
                            // (letrec () (for-each (lambda (a) (display a port)) args))
                            {
                                // (for-each (lambda (a) (display a port)) args)
                                imports::for_minus_each.with(|value| value.get()).invoke(&[
                                    {
                                        let port = port.clone();
                                        Scm::func(move |args: &[Scm]| {
                                            if args.len() != 1 {
                                                panic!("invalid arity")
                                            }
                                            let a = args[0].clone();
                                            // (letrec () (display a port))
                                            {
                                                // (display a port)
                                                imports::display
                                                    .with(|value| value.get())
                                                    .invoke(&[a.clone(), port.clone()])
                                            }
                                        })
                                    },
                                    args_.clone(),
                                ])
                            }
                        })
                    })
                });
                // (define (writeln port . args) (for-each (lambda (a) (write a port)) args) (newline port))
                globals::writeln.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() < 1 {
                                panic!("not enough args")
                            }
                            let port = args[0].clone();
                            let args_ = Scm::list(&args[1..]);
                            // (letrec () (for-each (lambda (a) (write a port)) args) (newline port))
                            {
                                {
                                    // (for-each (lambda (a) (write a port)) args)
                                    imports::for_minus_each.with(|value| value.get()).invoke(&[
                                        {
                                            let port = port.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let a = args[0].clone();
                                                // (letrec () (write a port))
                                                {
                                                    // (write a port)
                                                    imports::write
                                                        .with(|value| value.get())
                                                        .invoke(&[a.clone(), port.clone()])
                                                }
                                            })
                                        },
                                        args_.clone(),
                                    ]);
                                    // (newline port)
                                    imports::newline
                                        .with(|value| value.get())
                                        .invoke(&[port.clone()])
                                }
                            }
                        })
                    })
                });
                // (define (filter f seq) (if (pair? seq) (if (f (car seq)) (cons (car seq) (filter f (cdr seq))) (filter f (cdr seq))) (quote ())))
                globals::filter.with(|value| {
                    value.set({
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let f = args[0].clone();
                            let seq = args[1].clone();
                            // (letrec () (if (pair? seq) (if (f (car seq)) (cons (car seq) (filter f (cdr seq))) (filter f (cdr seq))) (quote ())))
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
                                        // (cons (car seq) (filter f (cdr seq)))
                                        imports::cons.with(|value| value.get()).invoke(&[
                                            // (car seq)
                                            imports::car
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                            // (filter f (cdr seq))
                                            globals::filter.with(|value| value.get()).invoke(&[
                                                f.clone(),
                                                // (cdr seq)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[seq.clone()]),
                                            ]),
                                        ])
                                    } else {
                                        // (filter f (cdr seq))
                                        globals::filter.with(|value| value.get()).invoke(&[
                                            f.clone(),
                                            // (cdr seq)
                                            imports::cdr
                                                .with(|value| value.get())
                                                .invoke(&[seq.clone()]),
                                        ])
                                    }
                                } else {
                                    Scm::Nil
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
                                                globals::filter.with(|value| value.get()).invoke(
                                                    &[
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
                                                                    cmp.clone().invoke(&[
                                                                        x.clone(),
                                                                        pivot.clone(),
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
                                            // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                pivot.clone(),
                                                // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                                globals::sort.with(|value| value.get()).invoke(&[
                                                    cmp.clone(),
                                                    // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                                    globals::filter
                                                        .with(|value| value.get())
                                                        .invoke(&[
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
                                                                            .with(|value| {
                                                                                value.get()
                                                                            })
                                                                            .invoke(&[
                                                                                // (cmp x pivot)
                                                                                cmp.clone().invoke(
                                                                                    &[
                                                                                        x.clone(),
                                                                                        pivot
                                                                                            .clone(
                                                                                            ),
                                                                                    ],
                                                                                ),
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
    }
}
