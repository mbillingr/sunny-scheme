#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::base::exports::*;
}

pub mod exports {
    pub use super::globals::append;
    pub use super::globals::assoc;
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
    pub use super::imports::procedure_p;
    pub use super::imports::set_minus_car_i;
    pub use super::imports::set_minus_cdr_i;
    pub use super::imports::string_l__p;
    pub use super::imports::string_minus__g_list;
    pub use super::imports::symbol_minus__g_string;
    pub use super::imports::symbol_p;
}

mod globals {
    use sunny_core::{Mut, Scm};
    pub fn __for_minus_each(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let proc = args[0].clone();
            let seq_star_ = args[1].clone();
            if ({
                // (any? null? seq*)
                globals::any_p
                    .with(|value| value.get())
                    .invoke(&[imports::null_p.with(|value| value.get()), seq_star_.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else {
                {
                    {
                        // (apply proc (map-1 car seq*))
                        imports::apply
                            .with(|value| value.get())
                            .invoke(&[proc.clone(), {
                                // (map-1 car seq*)
                                globals::map_minus_1.with(|value| value.get()).invoke(&[
                                    imports::car.with(|value| value.get()),
                                    seq_star_.clone(),
                                ])
                            }])
                    };
                    {
                        // (_for-each proc (map-1 cdr seq*))
                        globals::__for_minus_each
                            .with(|value| value.get())
                            .invoke(&[proc.clone(), {
                                // (map-1 cdr seq*)
                                globals::map_minus_1.with(|value| value.get()).invoke(&[
                                    imports::cdr.with(|value| value.get()),
                                    seq_star_.clone(),
                                ])
                            }])
                    }
                }
            }
        }
    }
    pub fn __map(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let func = args[0].clone();
            let seq_star_ = args[1].clone();
            if ({
                // (any? null? seq*)
                globals::any_p
                    .with(|value| value.get())
                    .invoke(&[imports::null_p.with(|value| value.get()), seq_star_.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else {
                {
                    // (cons (apply func (map-1 car seq*)) (_map func (map-1 cdr seq*)))
                    imports::cons.with(|value| value.get()).invoke(&[
                        {
                            // (apply func (map-1 car seq*))
                            imports::apply
                                .with(|value| value.get())
                                .invoke(&[func.clone(), {
                                    // (map-1 car seq*)
                                    globals::map_minus_1.with(|value| value.get()).invoke(&[
                                        imports::car.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ])
                                }])
                        },
                        {
                            // (_map func (map-1 cdr seq*))
                            globals::__map
                                .with(|value| value.get())
                                .invoke(&[func.clone(), {
                                    // (map-1 cdr seq*)
                                    globals::map_minus_1.with(|value| value.get()).invoke(&[
                                        imports::cdr.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ])
                                }])
                        },
                    ])
                }
            }
        }
    }
    pub fn all_p(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let pred = args[0].clone();
            let seq = args[1].clone();
            {
                // (cond ...)
                if ({
                    // (null? seq)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[seq.clone()])
                })
                .is_true()
                {
                    Scm::True
                } else if ({
                    // (pred (car seq))
                    pred.clone().invoke(&[{
                        // (car seq)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    }])
                })
                .is_true()
                {
                    {
                        // (all? pred (cdr seq))
                        globals::all_p
                            .with(|value| value.get())
                            .invoke(&[pred.clone(), {
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                    }
                } else {
                    Scm::False
                }
            }
        }
    }
    pub fn any_p(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let pred = args[0].clone();
            let seq = args[1].clone();
            {
                // (cond ...)
                if ({
                    // (null? seq)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[seq.clone()])
                })
                .is_true()
                {
                    Scm::False
                } else if ({
                    // (pred (car seq))
                    pred.clone().invoke(&[{
                        // (car seq)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    }])
                })
                .is_true()
                {
                    Scm::True
                } else {
                    {
                        // (any? pred (cdr seq))
                        globals::any_p
                            .with(|value| value.get())
                            .invoke(&[pred.clone(), {
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                    }
                }
            }
        }
    }
    pub fn append(args: &[Scm]) {
        {
            if args.len() < 0 {
                panic!("not enough args")
            }
            let seq = Scm::list(&args[0..]);
            {
                // (fold-right append2 (quote ()) seq)
                globals::fold_minus_right
                    .with(|value| value.get())
                    .invoke(&[
                        globals::append2.with(|value| value.get()),
                        Scm::Nil,
                        seq.clone(),
                    ])
            }
        }
    }
    pub fn append2(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let list1 = args[0].clone();
            let list2 = args[1].clone();
            if ({
                // (null? list1)
                imports::null_p
                    .with(|value| value.get())
                    .invoke(&[list1.clone()])
            })
            .is_true()
            {
                list2.clone()
            } else {
                {
                    // (cons (car list1) (append2 (cdr list1) list2))
                    imports::cons.with(|value| value.get()).invoke(&[
                        {
                            // (car list1)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[list1.clone()])
                        },
                        {
                            // (append2 (cdr list1) list2)
                            globals::append2.with(|value| value.get()).invoke(&[
                                {
                                    // (cdr list1)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[list1.clone()])
                                },
                                list2.clone(),
                            ])
                        },
                    ])
                }
            }
        }
    }
    pub fn assoc(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let obj = args[0].clone();
            let seq = args[1].clone();
            if ({
                // (pair? seq)
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                if ({
                    // (equal? obj (caar seq))
                    imports::equal_p
                        .with(|value| value.get())
                        .invoke(&[obj.clone(), {
                            // (caar seq)
                            imports::caar
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        }])
                })
                .is_true()
                {
                    {
                        // (car seq)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    }
                } else {
                    {
                        // (assoc obj (cdr seq))
                        globals::assoc
                            .with(|value| value.get())
                            .invoke(&[obj.clone(), {
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                    }
                }
            } else {
                Scm::False
            }
        }
    }
    pub fn assq(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let obj = args[0].clone();
            let seq = args[1].clone();
            if ({
                // (pair? seq)
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                if ({
                    // (eq? obj (caar seq))
                    imports::eq_p
                        .with(|value| value.get())
                        .invoke(&[obj.clone(), {
                            // (caar seq)
                            imports::caar
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        }])
                })
                .is_true()
                {
                    {
                        // (car seq)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    }
                } else {
                    {
                        // (assq obj (cdr seq))
                        globals::assq
                            .with(|value| value.get())
                            .invoke(&[obj.clone(), {
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                    }
                }
            } else {
                Scm::False
            }
        }
    }
    pub fn fold_minus_left(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let op = args[0].clone();
            let init = args[1].clone();
            let seq = args[2].clone();
            if ({
                // (null? seq)
                imports::null_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                init.clone()
            } else {
                {
                    // (fold-left op (op init (car seq)) (cdr seq))
                    globals::fold_minus_left.with(|value| value.get()).invoke(&[
                        op.clone(),
                        {
                            // (op init (car seq))
                            op.clone().invoke(&[init.clone(), {
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                        },
                        {
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        },
                    ])
                }
            }
        }
    }
    pub fn fold_minus_right(args: &[Scm]) {
        {
            if args.len() != 3 {
                panic!("invalid arity")
            }
            let op = args[0].clone();
            let init = args[1].clone();
            let seq = args[2].clone();
            if ({
                // (null? seq)
                imports::null_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                init.clone()
            } else {
                {
                    // (op (car seq) (fold-right op init (cdr seq)))
                    op.clone().invoke(&[
                        {
                            // (car seq)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        },
                        {
                            // (fold-right op init (cdr seq))
                            globals::fold_minus_right
                                .with(|value| value.get())
                                .invoke(&[op.clone(), init.clone(), {
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                }])
                        },
                    ])
                }
            }
        }
    }
    pub fn for_minus_each(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let proc = args[0].clone();
            let seq_star_ = Scm::list(&args[1..]);
            {
                // (_for-each proc seq*)
                globals::__for_minus_each
                    .with(|value| value.get())
                    .invoke(&[proc.clone(), seq_star_.clone()])
            }
        }
    }
    pub fn length(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (fold-left (lambda (acc _) (+ acc 1)) 0 seq)
                globals::fold_minus_left.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let acc = args[0].clone();
                            let __ = args[1].clone();
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
        }
    }
    pub fn list(args: &[Scm]) {
        {
            if args.len() < 0 {
                panic!("not enough args")
            }
            let x = Scm::list(&args[0..]);
            x.clone()
        }
    }
    pub fn list_minus_copy(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
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
        }
    }
    pub fn list_p(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (cond ...)
                if ({
                    // (null? seq)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[seq.clone()])
                })
                .is_true()
                {
                    Scm::True
                } else if ({
                    // (pair? seq)
                    imports::pair_p
                        .with(|value| value.get())
                        .invoke(&[seq.clone()])
                })
                .is_true()
                {
                    {
                        // (list? (cdr seq))
                        globals::list_p.with(|value| value.get()).invoke(&[{
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        }])
                    }
                } else {
                    Scm::False
                }
            }
        }
    }
    pub fn map(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let func = args[0].clone();
            let seq_star_ = Scm::list(&args[1..]);
            {
                // (_map func seq*)
                globals::__map
                    .with(|value| value.get())
                    .invoke(&[func.clone(), seq_star_.clone()])
            }
        }
    }
    pub fn map_minus_1(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let func = args[0].clone();
            let seq = args[1].clone();
            {
                // (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq)
                globals::fold_minus_right
                    .with(|value| value.get())
                    .invoke(&[
                        {
                            // Closure
                            let func = func.clone();
                            Scm::func(move |args: &[Scm]| {
                                if args.len() != 2 {
                                    panic!("invalid arity")
                                }
                                let x = args[0].clone();
                                let acc = args[1].clone();
                                {
                                    // (cons (func x) acc)
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        {
                                            // (func x)
                                            func.clone().invoke(&[x.clone()])
                                        },
                                        acc.clone(),
                                    ])
                                }
                            })
                        },
                        Scm::Nil,
                        seq.clone(),
                    ])
            }
        }
    }
    pub fn memq(args: &[Scm]) {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let obj = args[0].clone();
            let seq = args[1].clone();
            if ({
                // (pair? seq)
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                if ({
                    // (eq? obj (car seq))
                    imports::eq_p
                        .with(|value| value.get())
                        .invoke(&[obj.clone(), {
                            // (car seq)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        }])
                })
                .is_true()
                {
                    seq.clone()
                } else {
                    {
                        // (memq obj (cdr seq))
                        globals::memq
                            .with(|value| value.get())
                            .invoke(&[obj.clone(), {
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            }])
                    }
                }
            } else {
                Scm::False
            }
        }
    }
    pub fn not(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            if (x.clone()).is_true() {
                Scm::False
            } else {
                Scm::True
            }
        }
    }
    pub fn reverse(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq)
                globals::fold_minus_left.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 2 {
                                panic!("invalid arity")
                            }
                            let acc = args[0].clone();
                            let x = args[1].clone();
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
        }
    }
    pub fn string_minus_append(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (fold-left string-cons s1 args)
                globals::fold_minus_left.with(|value| value.get()).invoke(&[
                    imports::string_minus_cons.with(|value| value.get()),
                    s1.clone(),
                    args_.clone(),
                ])
            }
        }
    }
    pub fn string_e__p(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (all? (lambda (s) (equal? s1 s)) args)
                globals::all_p.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        let s1 = s1.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let s = args[0].clone();
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
        }
    }
    pub fn symbol_e__p(args: &[Scm]) {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (all? (lambda (s) (eq? s1 s)) args)
                globals::all_p.with(|value| value.get()).invoke(&[
                    {
                        // Closure
                        let s1 = s1.clone();
                        Scm::func(move |args: &[Scm]| {
                            if args.len() != 1 {
                                panic!("invalid arity")
                            }
                            let s = args[0].clone();
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
        }
    }
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
        {
            // (define (not x) ...)
            (/*NOP*/)
        };
        {
            // (define (list . x) ...)
            (/*NOP*/)
        };
        {
            // (define (list? seq) ...)
            (/*NOP*/)
        };
        {
            // (define (length seq) ...)
            (/*NOP*/)
        };
        {
            // (define (reverse seq) ...)
            (/*NOP*/)
        };
        {
            // (define (list-copy seq) ...)
            (/*NOP*/)
        };
        {
            // (define (assq obj seq) ...)
            (/*NOP*/)
        };
        {
            // (define (assoc obj seq) ...)
            (/*NOP*/)
        };
        {
            // (define (memq obj seq) ...)
            (/*NOP*/)
        };
        {
            // (define (append . seq) ...)
            (/*NOP*/)
        };
        {
            // (define (append2 list1 list2) ...)
            (/*NOP*/)
        };
        {
            // (define (symbol=? s1 . args) ...)
            (/*NOP*/)
        };
        {
            // (define (string=? s1 . args) ...)
            (/*NOP*/)
        };
        {
            // (define (string-append s1 . args) ...)
            (/*NOP*/)
        };
        {
            // (define (for-each proc . seq*) ...)
            (/*NOP*/)
        };
        {
            // (define (map func . seq*) ...)
            (/*NOP*/)
        };
        {
            // (define (_for-each proc seq*) ...)
            (/*NOP*/)
        };
        {
            // (define (_map func seq*) ...)
            (/*NOP*/)
        };
        {
            // (define (all? pred seq) ...)
            (/*NOP*/)
        };
        {
            // (define (any? pred seq) ...)
            (/*NOP*/)
        };
        {
            // (define (map-1 func seq) ...)
            (/*NOP*/)
        };
        {
            // (define (fold-right op init seq) ...)
            (/*NOP*/)
        };
        {
            // (define (fold-left op init seq) ...)
            (/*NOP*/)
        }
    };
}
