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
    pub use super::globals::string_minus_append;
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
    pub use super::imports::symbol_minus__g_string;
    pub use super::imports::symbol_p;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static any_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL any?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static map_minus_1: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL map-1"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static __map: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _map"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static map: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL map"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static __for_minus_each: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL _for-each"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static for_minus_each: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL for-each"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static string_minus_append: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL string-append"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static string_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL string=?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static all_p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL all?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static symbol_e__p: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL symbol=?"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static memq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL memq"))}
    thread_local! {#[allow(non_upper_case_globals)] pub static assq: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL assq"))}
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
                        // (cond ...)
                        if (
                            // (null? seq)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            Scm::True
                        } else if (
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
        // (define (list-copy seq) (fold-right cons (quote ()) seq))
        globals::list_minus_copy.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 1 {
                        panic!("invalid arity")
                    }
                    let seq = args[0].clone();
                    // (letrec () (fold-right cons (quote ()) seq))
                    {
                        // (fold-right cons (quote ()) seq)
                        globals::fold_minus_right
                            .with(|value| value.get())
                            .invoke(&[
                                imports::cons.with(|value| value.get()),
                                Scm::Nil,
                                seq.clone(),
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
        // (define (symbol=? s1 . args) (all? (lambda (s) (eq? s1 s)) args))
        globals::symbol_e__p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() < 1 {
                        panic!("not enough args")
                    }
                    let s1 = args[0].clone();
                    let args_ = Scm::list(&args[1..]);
                    // (letrec () (all? (lambda (s) (eq? s1 s)) args))
                    {
                        // (all? (lambda (s) (eq? s1 s)) args)
                        globals::all_p.with(|value| value.get()).invoke(&[
                            {
                                let s1 = s1.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let s = args[0].clone();
                                    // (letrec () (eq? s1 s))
                                    {
                                        // (eq? s1 s)
                                        imports::eq_p
                                            .with(|value| value.get())
                                            .invoke(&[s1.clone(), s.clone()])
                                    }
                                })
                            },
                            args_.clone(),
                        ])
                    }
                })
            })
        });
        // (define (string=? s1 . args) (all? (lambda (s) (equal? s1 s)) args))
        globals::string_e__p.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() < 1 {
                        panic!("not enough args")
                    }
                    let s1 = args[0].clone();
                    let args_ = Scm::list(&args[1..]);
                    // (letrec () (all? (lambda (s) (equal? s1 s)) args))
                    {
                        // (all? (lambda (s) (equal? s1 s)) args)
                        globals::all_p.with(|value| value.get()).invoke(&[
                            {
                                let s1 = s1.clone();
                                Scm::func(move |args: &[Scm]| {
                                    if args.len() != 1 {
                                        panic!("invalid arity")
                                    }
                                    let s = args[0].clone();
                                    // (letrec () (equal? s1 s))
                                    {
                                        // (equal? s1 s)
                                        imports::equal_p
                                            .with(|value| value.get())
                                            .invoke(&[s1.clone(), s.clone()])
                                    }
                                })
                            },
                            args_.clone(),
                        ])
                    }
                })
            })
        });
        // (define (string-append s1 . args) (fold-left string-cons s1 args))
        globals::string_minus_append.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() < 1 {
                        panic!("not enough args")
                    }
                    let s1 = args[0].clone();
                    let args_ = Scm::list(&args[1..]);
                    // (letrec () (fold-left string-cons s1 args))
                    {
                        // (fold-left string-cons s1 args)
                        globals::fold_minus_left.with(|value| value.get()).invoke(&[
                            imports::string_minus_cons.with(|value| value.get()),
                            s1.clone(),
                            args_.clone(),
                        ])
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
                                    globals::map_minus_1.with(|value| value.get()).invoke(&[
                                        imports::car.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ]),
                                ]);
                                // (_for-each proc (map-1 cdr seq*))
                                globals::__for_minus_each
                                    .with(|value| value.get())
                                    .invoke(&[
                                        proc.clone(),
                                        // (map-1 cdr seq*)
                                        globals::map_minus_1.with(|value| value.get()).invoke(&[
                                            imports::cdr.with(|value| value.get()),
                                            seq_star_.clone(),
                                        ]),
                                    ])
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
                                    globals::map_minus_1.with(|value| value.get()).invoke(&[
                                        imports::car.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ]),
                                ]),
                                // (_map func (map-1 cdr seq*))
                                globals::__map.with(|value| value.get()).invoke(&[
                                    func.clone(),
                                    // (map-1 cdr seq*)
                                    globals::map_minus_1.with(|value| value.get()).invoke(&[
                                        imports::cdr.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ]),
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
                        // (cond ...)
                        if (
                            // (null? seq)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            Scm::True
                        } else if (
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
                        // (cond ...)
                        if (
                            // (null? seq)
                            imports::null_p
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        )
                        .is_true()
                        {
                            Scm::False
                        } else if (
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
                                            imports::cons.with(|value| value.get()).invoke(&[
                                                // (func x)
                                                func.clone().invoke(&[x.clone()]),
                                                acc.clone(),
                                            ])
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
                                globals::fold_minus_right
                                    .with(|value| value.get())
                                    .invoke(&[
                                        op.clone(),
                                        init.clone(),
                                        // (cdr seq)
                                        imports::cdr
                                            .with(|value| value.get())
                                            .invoke(&[seq.clone()]),
                                    ]),
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
