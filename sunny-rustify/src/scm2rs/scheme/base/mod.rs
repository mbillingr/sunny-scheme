#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::native::base::exports::*;
}

pub mod exports {
    pub use super::append;
    pub use super::assoc;
    pub use super::assq;
    pub use super::for_minus_each;
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
    pub use super::imports::number_minus__g_string;
    pub use super::imports::pair_p;
    pub use super::imports::procedure_p;
    pub use super::imports::set_minus_car_i;
    pub use super::imports::set_minus_cdr_i;
    pub use super::imports::string_l__p;
    pub use super::imports::string_minus__g_list;
    pub use super::imports::string_minus__g_symbol;
    pub use super::imports::string_p;
    pub use super::imports::symbol_minus__g_string;
    pub use super::imports::symbol_p;
    pub use super::length;
    pub use super::list;
    pub use super::list_minus_copy;
    pub use super::list_p;
    pub use super::map;
    pub use super::memq;
    pub use super::not;
    pub use super::reverse;
    pub use super::string_e__p;
    pub use super::string_minus_append;
    pub use super::symbol_e__p;
}

pub fn __for_minus_each(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let proc__32 = args[0].clone();
        let seq_star___31 = args[1].clone();
        if ({
            // (any? null? seq*)
            Scm::func(any_p).invoke(&[Scm::func(imports::null_p), seq_star___31.clone()])
        })
        .is_true()
        {
            Scm::Nil
        } else {
            {
                {
                    // (apply proc (map-1 car seq*))
                    imports::apply(&[proc__32.clone(), {
                        // (map-1 car seq*)
                        Scm::func(map_minus_1)
                            .invoke(&[Scm::func(imports::car), seq_star___31.clone()])
                    }])
                };
                {
                    // (_for-each proc (map-1 cdr seq*))
                    Scm::func(__for_minus_each).invoke(&[proc__32.clone(), {
                        // (map-1 cdr seq*)
                        Scm::func(map_minus_1)
                            .invoke(&[Scm::func(imports::cdr), seq_star___31.clone()])
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
        let func__34 = args[0].clone();
        let seq_star___33 = args[1].clone();
        if ({
            // (any? null? seq*)
            Scm::func(any_p).invoke(&[Scm::func(imports::null_p), seq_star___33.clone()])
        })
        .is_true()
        {
            Scm::Nil
        } else {
            {
                // (cons (apply func (map-1 car seq*)) (_map func (map-1 cdr seq*)))
                imports::cons(&[
                    {
                        // (apply func (map-1 car seq*))
                        imports::apply(&[func__34.clone(), {
                            // (map-1 car seq*)
                            Scm::func(map_minus_1)
                                .invoke(&[Scm::func(imports::car), seq_star___33.clone()])
                        }])
                    },
                    {
                        // (_map func (map-1 cdr seq*))
                        Scm::func(__map).invoke(&[func__34.clone(), {
                            // (map-1 cdr seq*)
                            Scm::func(map_minus_1)
                                .invoke(&[Scm::func(imports::cdr), seq_star___33.clone()])
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
        let pred__36 = args[0].clone();
        let seq__35 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__35.clone()])
            })
            .is_true()
            {
                Scm::True
            } else if ({
                // (pred (car seq))
                pred__36.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__35.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (all? pred (cdr seq))
                    Scm::func(all_p).invoke(&[pred__36.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__35.clone()])
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
        let pred__38 = args[0].clone();
        let seq__37 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__37.clone()])
            })
            .is_true()
            {
                Scm::False
            } else if ({
                // (pred (car seq))
                pred__38.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__37.clone()])
                }])
            })
            .is_true()
            {
                Scm::True
            } else {
                {
                    // (any? pred (cdr seq))
                    Scm::func(any_p).invoke(&[pred__38.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__37.clone()])
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
        let seq__16 = Scm::list(&args[0..]);
        {
            // (fold-right append2 (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[Scm::func(append2), Scm::Nil, seq__16.clone()])
        }
    }
    .into()
}
pub fn append2(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let list1__18 = args[0].clone();
        let list2__17 = args[1].clone();
        if ({
            // (null? list1)
            imports::null_p(&[list1__18.clone()])
        })
        .is_true()
        {
            list2__17.clone()
        } else {
            {
                // (cons (car list1) (append2 (cdr list1) list2))
                imports::cons(&[
                    {
                        // (car list1)
                        imports::car(&[list1__18.clone()])
                    },
                    {
                        // (append2 (cdr list1) list2)
                        Scm::func(append2).invoke(&[
                            {
                                // (cdr list1)
                                imports::cdr(&[list1__18.clone()])
                            },
                            list2__17.clone(),
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
        let obj__13 = args[0].clone();
        let seq__12 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__12.clone()])
        })
        .is_true()
        {
            if ({
                // (equal? obj (caar seq))
                imports::equal_p(&[obj__13.clone(), {
                    // (caar seq)
                    imports::caar(&[seq__12.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (car seq)
                    imports::car(&[seq__12.clone()])
                }
            } else {
                {
                    // (assoc obj (cdr seq))
                    Scm::func(assoc).invoke(&[obj__13.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__12.clone()])
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
        let obj__11 = args[0].clone();
        let seq__10 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__10.clone()])
        })
        .is_true()
        {
            if ({
                // (eq? obj (caar seq))
                imports::eq_p(&[obj__11.clone(), {
                    // (caar seq)
                    imports::caar(&[seq__10.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (car seq)
                    imports::car(&[seq__10.clone()])
                }
            } else {
                {
                    // (assq obj (cdr seq))
                    Scm::func(assq).invoke(&[obj__11.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__10.clone()])
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
        let op__48 = args[0].clone();
        let init__47 = args[1].clone();
        let seq__46 = args[2].clone();
        if ({
            // (null? seq)
            imports::null_p(&[seq__46.clone()])
        })
        .is_true()
        {
            init__47.clone()
        } else {
            {
                // (fold-left op (op init (car seq)) (cdr seq))
                Scm::func(fold_minus_left).invoke(&[
                    op__48.clone(),
                    {
                        // (op init (car seq))
                        op__48.clone().invoke(&[init__47.clone(), {
                            // (car seq)
                            imports::car(&[seq__46.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        imports::cdr(&[seq__46.clone()])
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
        let op__45 = args[0].clone();
        let init__44 = args[1].clone();
        let seq__43 = args[2].clone();
        if ({
            // (null? seq)
            imports::null_p(&[seq__43.clone()])
        })
        .is_true()
        {
            init__44.clone()
        } else {
            {
                // (op (car seq) (fold-right op init (cdr seq)))
                op__45.clone().invoke(&[
                    {
                        // (car seq)
                        imports::car(&[seq__43.clone()])
                    },
                    {
                        // (fold-right op init (cdr seq))
                        Scm::func(fold_minus_right).invoke(&[op__45.clone(), init__44.clone(), {
                            // (cdr seq)
                            imports::cdr(&[seq__43.clone()])
                        }])
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
        let proc__28 = args[0].clone();
        let seq_star___27 = Scm::list(&args[1..]);
        {
            // (_for-each proc seq*)
            Scm::func(__for_minus_each).invoke(&[proc__28.clone(), seq_star___27.clone()])
        }
    }
    .into()
}
pub fn length(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__4 = args[0].clone();
        {
            // (fold-left (lambda (acc _) (+ acc 1)) 0 seq)
            Scm::func(fold_minus_left).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let acc__5 = args[0].clone();
                        let __ = args[1].clone();
                        {
                            // (+ acc 1)
                            imports::_plus_(&[acc__5.clone(), Scm::from(1)])
                        }
                    })
                },
                Scm::from(0),
                seq__4.clone(),
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
        let x__2 = Scm::list(&args[0..]);
        x__2.clone()
    }
    .into()
}
pub fn list_minus_copy(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__9 = args[0].clone();
        {
            // (fold-right cons (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[
                Scm::func(imports::cons),
                Scm::Nil,
                seq__9.clone(),
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
        let seq__3 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__3.clone()])
            })
            .is_true()
            {
                Scm::True
            } else if ({
                // (pair? seq)
                imports::pair_p(&[seq__3.clone()])
            })
            .is_true()
            {
                {
                    // (list? (cdr seq))
                    Scm::func(list_p).invoke(&[{
                        // (cdr seq)
                        imports::cdr(&[seq__3.clone()])
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
        let func__30 = args[0].clone();
        let seq_star___29 = Scm::list(&args[1..]);
        {
            // (_map func seq*)
            Scm::func(__map).invoke(&[func__30.clone(), seq_star___29.clone()])
        }
    }
    .into()
}
pub fn map_minus_1(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let func__42 = args[0].clone();
        let seq__39 = args[1].clone();
        {
            // (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[
                {
                    // Closure
                    let func__42 = func__42.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let x__41 = args[0].clone();
                        let acc__40 = args[1].clone();
                        {
                            // (cons (func x) acc)
                            imports::cons(&[
                                {
                                    // (func x)
                                    func__42.clone().invoke(&[x__41.clone()])
                                },
                                acc__40.clone(),
                            ])
                        }
                    })
                },
                Scm::Nil,
                seq__39.clone(),
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
        let obj__15 = args[0].clone();
        let seq__14 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__14.clone()])
        })
        .is_true()
        {
            if ({
                // (eq? obj (car seq))
                imports::eq_p(&[obj__15.clone(), {
                    // (car seq)
                    imports::car(&[seq__14.clone()])
                }])
            })
            .is_true()
            {
                seq__14.clone()
            } else {
                {
                    // (memq obj (cdr seq))
                    Scm::func(memq).invoke(&[obj__15.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__14.clone()])
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
        let x__1 = args[0].clone();
        if (x__1.clone()).is_true() {
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
        let seq__6 = args[0].clone();
        {
            // (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq)
            Scm::func(fold_minus_left).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let acc__7 = args[0].clone();
                        let x__8 = args[1].clone();
                        {
                            // (cons x acc)
                            imports::cons(&[x__8.clone(), acc__7.clone()])
                        }
                    })
                },
                Scm::Nil,
                seq__6.clone(),
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
        let s1__26 = args[0].clone();
        let args__25 = Scm::list(&args[1..]);
        {
            // (fold-left string-cons s1 args)
            Scm::func(fold_minus_left).invoke(&[
                Scm::func(imports::string_minus_cons),
                s1__26.clone(),
                args__25.clone(),
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
        let s1__24 = args[0].clone();
        let args__22 = Scm::list(&args[1..]);
        {
            // (all? (lambda (s) (equal? s1 s)) args)
            Scm::func(all_p).invoke(&[
                {
                    // Closure
                    let s1__24 = s1__24.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let s__23 = args[0].clone();
                        {
                            // (equal? s1 s)
                            imports::equal_p(&[s1__24.clone(), s__23.clone()])
                        }
                    })
                },
                args__22.clone(),
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
        let s1__21 = args[0].clone();
        let args__19 = Scm::list(&args[1..]);
        {
            // (all? (lambda (s) (eq? s1 s)) args)
            Scm::func(all_p).invoke(&[
                {
                    // Closure
                    let s1__21 = s1__21.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let s__20 = args[0].clone();
                        {
                            // (eq? s1 s)
                            imports::eq_p(&[s1__21.clone(), s__20.clone()])
                        }
                    })
                },
                args__19.clone(),
            ])
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
