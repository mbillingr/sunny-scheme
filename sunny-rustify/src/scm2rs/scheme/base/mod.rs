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
    pub fn __for_minus_each(args: &[Scm]) -> Scm {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let proc = args[0].clone();
            let seq_star_ = args[1].clone();
            if ({
                // (any? null? seq*)
                Scm::func(globals::any_p)
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
                                Scm::func(globals::map_minus_1).invoke(&[
                                    imports::car.with(|value| value.get()),
                                    seq_star_.clone(),
                                ])
                            }])
                    };
                    {
                        // (_for-each proc (map-1 cdr seq*))
                        Scm::func(globals::__for_minus_each).invoke(&[proc.clone(), {
                            // (map-1 cdr seq*)
                            Scm::func(globals::map_minus_1).invoke(&[
                                imports::cdr.with(|value| value.get()),
                                seq_star_.clone(),
                            ])
                        }])
                    }
                }
            }
        }
        .into()
    }
    pub fn __map(args: &[Scm]) -> Scm {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let func = args[0].clone();
            let seq_star_ = args[1].clone();
            if ({
                // (any? null? seq*)
                Scm::func(globals::any_p)
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
                                    Scm::func(globals::map_minus_1).invoke(&[
                                        imports::car.with(|value| value.get()),
                                        seq_star_.clone(),
                                    ])
                                }])
                        },
                        {
                            // (_map func (map-1 cdr seq*))
                            Scm::func(globals::__map).invoke(&[func.clone(), {
                                // (map-1 cdr seq*)
                                Scm::func(globals::map_minus_1).invoke(&[
                                    imports::cdr.with(|value| value.get()),
                                    seq_star_.clone(),
                                ])
                            }])
                        },
                    ])
                }
            }
        }
        .into()
    }
    pub fn all_p(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::all_p).invoke(&[pred.clone(), {
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
        .into()
    }
    pub fn any_p(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::any_p).invoke(&[pred.clone(), {
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        }])
                    }
                }
            }
        }
        .into()
    }
    pub fn append(args: &[Scm]) -> Scm {
        {
            if args.len() < 0 {
                panic!("not enough args")
            }
            let seq = Scm::list(&args[0..]);
            {
                // (fold-right append2 (quote ()) seq)
                Scm::func(globals::fold_minus_right).invoke(&[
                    Scm::func(globals::append2),
                    Scm::Nil,
                    seq.clone(),
                ])
            }
        }
        .into()
    }
    pub fn append2(args: &[Scm]) -> Scm {
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
                            Scm::func(globals::append2).invoke(&[
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
        .into()
    }
    pub fn assoc(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::assoc).invoke(&[obj.clone(), {
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
        .into()
    }
    pub fn assq(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::assq).invoke(&[obj.clone(), {
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
        .into()
    }
    pub fn fold_minus_left(args: &[Scm]) -> Scm {
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
                    Scm::func(globals::fold_minus_left).invoke(&[
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
        .into()
    }
    pub fn fold_minus_right(args: &[Scm]) -> Scm {
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
                            Scm::func(globals::fold_minus_right).invoke(&[
                                op.clone(),
                                init.clone(),
                                {
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                },
                            ])
                        },
                    ])
                }
            }
        }
        .into()
    }
    pub fn for_minus_each(args: &[Scm]) -> Scm {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let proc = args[0].clone();
            let seq_star_ = Scm::list(&args[1..]);
            {
                // (_for-each proc seq*)
                Scm::func(globals::__for_minus_each).invoke(&[proc.clone(), seq_star_.clone()])
            }
        }
        .into()
    }
    pub fn length(args: &[Scm]) -> Scm {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (fold-left (lambda (acc _) (+ acc 1)) 0 seq)
                Scm::func(globals::fold_minus_left).invoke(&[
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
        .into()
    }
    pub fn list(args: &[Scm]) -> Scm {
        {
            if args.len() < 0 {
                panic!("not enough args")
            }
            let x = Scm::list(&args[0..]);
            x.clone()
        }
        .into()
    }
    pub fn list_minus_copy(args: &[Scm]) -> Scm {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (fold-right cons (quote ()) seq)
                Scm::func(globals::fold_minus_right).invoke(&[
                    imports::cons.with(|value| value.get()),
                    Scm::Nil,
                    seq.clone(),
                ])
            }
        }
        .into()
    }
    pub fn list_p(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::list_p).invoke(&[{
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
        .into()
    }
    pub fn map(args: &[Scm]) -> Scm {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let func = args[0].clone();
            let seq_star_ = Scm::list(&args[1..]);
            {
                // (_map func seq*)
                Scm::func(globals::__map).invoke(&[func.clone(), seq_star_.clone()])
            }
        }
        .into()
    }
    pub fn map_minus_1(args: &[Scm]) -> Scm {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let func = args[0].clone();
            let seq = args[1].clone();
            {
                // (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq)
                Scm::func(globals::fold_minus_right).invoke(&[
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
        .into()
    }
    pub fn memq(args: &[Scm]) -> Scm {
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
                        Scm::func(globals::memq).invoke(&[obj.clone(), {
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
        .into()
    }
    pub fn not(args: &[Scm]) -> Scm {
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
        .into()
    }
    pub fn reverse(args: &[Scm]) -> Scm {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let seq = args[0].clone();
            {
                // (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq)
                Scm::func(globals::fold_minus_left).invoke(&[
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
        .into()
    }
    pub fn string_minus_append(args: &[Scm]) -> Scm {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (fold-left string-cons s1 args)
                Scm::func(globals::fold_minus_left).invoke(&[
                    imports::string_minus_cons.with(|value| value.get()),
                    s1.clone(),
                    args_.clone(),
                ])
            }
        }
        .into()
    }
    pub fn string_e__p(args: &[Scm]) -> Scm {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (all? (lambda (s) (equal? s1 s)) args)
                Scm::func(globals::all_p).invoke(&[
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
        .into()
    }
    pub fn symbol_e__p(args: &[Scm]) -> Scm {
        {
            if args.len() < 1 {
                panic!("not enough args")
            }
            let s1 = args[0].clone();
            let args_ = Scm::list(&args[1..]);
            {
                // (all? (lambda (s) (eq? s1 s)) args)
                Scm::func(globals::all_p).invoke(&[
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
        .into()
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
