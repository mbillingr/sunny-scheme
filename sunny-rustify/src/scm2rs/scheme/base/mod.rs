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
        let proc__1 = args[0].clone();
        let seq_star___2 = args[1].clone();
        if ({
            // (any? null? seq*)
            Scm::func(any_p).invoke(&[Scm::func(imports::null_p), seq_star___2.clone()])
        })
        .is_true()
        {
            Scm::Nil
        } else {
            {
                {
                    // (apply proc (map-1 car seq*))
                    imports::apply(&[proc__1.clone(), {
                        // (map-1 car seq*)
                        Scm::func(map_minus_1)
                            .invoke(&[Scm::func(imports::car), seq_star___2.clone()])
                    }])
                };
                {
                    // (_for-each proc (map-1 cdr seq*))
                    Scm::func(__for_minus_each).invoke(&[proc__1.clone(), {
                        // (map-1 cdr seq*)
                        Scm::func(map_minus_1)
                            .invoke(&[Scm::func(imports::cdr), seq_star___2.clone()])
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
        let func__1 = args[0].clone();
        let seq_star___3 = args[1].clone();
        if ({
            // (any? null? seq*)
            Scm::func(any_p).invoke(&[Scm::func(imports::null_p), seq_star___3.clone()])
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
                        imports::apply(&[func__1.clone(), {
                            // (map-1 car seq*)
                            Scm::func(map_minus_1)
                                .invoke(&[Scm::func(imports::car), seq_star___3.clone()])
                        }])
                    },
                    {
                        // (_map func (map-1 cdr seq*))
                        Scm::func(__map).invoke(&[func__1.clone(), {
                            // (map-1 cdr seq*)
                            Scm::func(map_minus_1)
                                .invoke(&[Scm::func(imports::cdr), seq_star___3.clone()])
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
        let pred__0 = args[0].clone();
        let seq__8 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__8.clone()])
            })
            .is_true()
            {
                Scm::True
            } else if ({
                // (pred (car seq))
                pred__0.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__8.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (all? pred (cdr seq))
                    Scm::func(all_p).invoke(&[pred__0.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__8.clone()])
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
        let pred__1 = args[0].clone();
        let seq__9 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__9.clone()])
            })
            .is_true()
            {
                Scm::False
            } else if ({
                // (pred (car seq))
                pred__1.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__9.clone()])
                }])
            })
            .is_true()
            {
                Scm::True
            } else {
                {
                    // (any? pred (cdr seq))
                    Scm::func(any_p).invoke(&[pred__1.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__9.clone()])
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
        let seq__7 = Scm::list(&args[0..]);
        {
            // (fold-right append2 (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[Scm::func(append2), Scm::Nil, seq__7.clone()])
        }
    }
    .into()
}
pub fn append2(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let list1__0 = args[0].clone();
        let list2__0 = args[1].clone();
        if ({
            // (null? list1)
            imports::null_p(&[list1__0.clone()])
        })
        .is_true()
        {
            list2__0.clone()
        } else {
            {
                // (cons (car list1) (append2 (cdr list1) list2))
                imports::cons(&[
                    {
                        // (car list1)
                        imports::car(&[list1__0.clone()])
                    },
                    {
                        // (append2 (cdr list1) list2)
                        Scm::func(append2).invoke(&[
                            {
                                // (cdr list1)
                                imports::cdr(&[list1__0.clone()])
                            },
                            list2__0.clone(),
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
        let obj__1 = args[0].clone();
        let seq__5 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__5.clone()])
        })
        .is_true()
        {
            if ({
                // (equal? obj (caar seq))
                imports::equal_p(&[obj__1.clone(), {
                    // (caar seq)
                    imports::caar(&[seq__5.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (car seq)
                    imports::car(&[seq__5.clone()])
                }
            } else {
                {
                    // (assoc obj (cdr seq))
                    Scm::func(assoc).invoke(&[obj__1.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__5.clone()])
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
        let obj__0 = args[0].clone();
        let seq__4 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__4.clone()])
        })
        .is_true()
        {
            if ({
                // (eq? obj (caar seq))
                imports::eq_p(&[obj__0.clone(), {
                    // (caar seq)
                    imports::caar(&[seq__4.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (car seq)
                    imports::car(&[seq__4.clone()])
                }
            } else {
                {
                    // (assq obj (cdr seq))
                    Scm::func(assq).invoke(&[obj__0.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__4.clone()])
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
        let op__1 = args[0].clone();
        let init__1 = args[1].clone();
        let seq__12 = args[2].clone();
        if ({
            // (null? seq)
            imports::null_p(&[seq__12.clone()])
        })
        .is_true()
        {
            init__1.clone()
        } else {
            {
                // (fold-left op (op init (car seq)) (cdr seq))
                Scm::func(fold_minus_left).invoke(&[
                    op__1.clone(),
                    {
                        // (op init (car seq))
                        op__1.clone().invoke(&[init__1.clone(), {
                            // (car seq)
                            imports::car(&[seq__12.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        imports::cdr(&[seq__12.clone()])
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
        let op__0 = args[0].clone();
        let init__0 = args[1].clone();
        let seq__11 = args[2].clone();
        if ({
            // (null? seq)
            imports::null_p(&[seq__11.clone()])
        })
        .is_true()
        {
            init__0.clone()
        } else {
            {
                // (op (car seq) (fold-right op init (cdr seq)))
                op__0.clone().invoke(&[
                    {
                        // (car seq)
                        imports::car(&[seq__11.clone()])
                    },
                    {
                        // (fold-right op init (cdr seq))
                        Scm::func(fold_minus_right).invoke(&[op__0.clone(), init__0.clone(), {
                            // (cdr seq)
                            imports::cdr(&[seq__11.clone()])
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
        let proc__0 = args[0].clone();
        let seq_star___0 = Scm::list(&args[1..]);
        {
            // (_for-each proc seq*)
            Scm::func(__for_minus_each).invoke(&[proc__0.clone(), seq_star___0.clone()])
        }
    }
    .into()
}
pub fn length(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__1 = args[0].clone();
        {
            // (fold-left (lambda (acc _) (+ acc 1)) 0 seq)
            Scm::func(fold_minus_left).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let acc__0 = args[0].clone();
                        let __ = args[1].clone();
                        {
                            // (+ acc 1)
                            imports::_plus_(&[acc__0.clone(), Scm::from(1)])
                        }
                    })
                },
                Scm::from(0),
                seq__1.clone(),
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
        let x__1 = Scm::list(&args[0..]);
        x__1.clone()
    }
    .into()
}
pub fn list_minus_copy(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__3 = args[0].clone();
        {
            // (fold-right cons (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[
                Scm::func(imports::cons),
                Scm::Nil,
                seq__3.clone(),
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
        let seq__0 = args[0].clone();
        {
            // (cond ...)
            if ({
                // (null? seq)
                imports::null_p(&[seq__0.clone()])
            })
            .is_true()
            {
                Scm::True
            } else if ({
                // (pair? seq)
                imports::pair_p(&[seq__0.clone()])
            })
            .is_true()
            {
                {
                    // (list? (cdr seq))
                    Scm::func(list_p).invoke(&[{
                        // (cdr seq)
                        imports::cdr(&[seq__0.clone()])
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
        let func__0 = args[0].clone();
        let seq_star___1 = Scm::list(&args[1..]);
        {
            // (_map func seq*)
            Scm::func(__map).invoke(&[func__0.clone(), seq_star___1.clone()])
        }
    }
    .into()
}
pub fn map_minus_1(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let func__2 = args[0].clone();
        let seq__10 = args[1].clone();
        {
            // (fold-right (lambda (x acc) (cons (func x) acc)) (quote ()) seq)
            Scm::func(fold_minus_right).invoke(&[
                {
                    // Closure
                    let func__2 = func__2.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let x__3 = args[0].clone();
                        let acc__2 = args[1].clone();
                        {
                            // (cons (func x) acc)
                            imports::cons(&[
                                {
                                    // (func x)
                                    func__2.clone().invoke(&[x__3.clone()])
                                },
                                acc__2.clone(),
                            ])
                        }
                    })
                },
                Scm::Nil,
                seq__10.clone(),
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
        let obj__2 = args[0].clone();
        let seq__6 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__6.clone()])
        })
        .is_true()
        {
            if ({
                // (eq? obj (car seq))
                imports::eq_p(&[obj__2.clone(), {
                    // (car seq)
                    imports::car(&[seq__6.clone()])
                }])
            })
            .is_true()
            {
                seq__6.clone()
            } else {
                {
                    // (memq obj (cdr seq))
                    Scm::func(memq).invoke(&[obj__2.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__6.clone()])
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
        let x__0 = args[0].clone();
        if (x__0.clone()).is_true() {
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
        let seq__2 = args[0].clone();
        {
            // (fold-left (lambda (acc x) (cons x acc)) (quote ()) seq)
            Scm::func(fold_minus_left).invoke(&[
                {
                    // Closure
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 2 {
                            panic!("invalid arity")
                        }
                        let acc__1 = args[0].clone();
                        let x__2 = args[1].clone();
                        {
                            // (cons x acc)
                            imports::cons(&[x__2.clone(), acc__1.clone()])
                        }
                    })
                },
                Scm::Nil,
                seq__2.clone(),
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
        let s1__2 = args[0].clone();
        let args__2 = Scm::list(&args[1..]);
        {
            // (fold-left string-cons s1 args)
            Scm::func(fold_minus_left).invoke(&[
                Scm::func(imports::string_minus_cons),
                s1__2.clone(),
                args__2.clone(),
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
        let s1__1 = args[0].clone();
        let args__1 = Scm::list(&args[1..]);
        {
            // (all? (lambda (s) (equal? s1 s)) args)
            Scm::func(all_p).invoke(&[
                {
                    // Closure
                    let s1__1 = s1__1.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let s__1 = args[0].clone();
                        {
                            // (equal? s1 s)
                            imports::equal_p(&[s1__1.clone(), s__1.clone()])
                        }
                    })
                },
                args__1.clone(),
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
        let s1__0 = args[0].clone();
        let args__0 = Scm::list(&args[1..]);
        {
            // (all? (lambda (s) (eq? s1 s)) args)
            Scm::func(all_p).invoke(&[
                {
                    // Closure
                    let s1__0 = s1__0.clone();
                    Scm::func(move |args: &[Scm]| {
                        if args.len() != 1 {
                            panic!("invalid arity")
                        }
                        let s__0 = args[0].clone();
                        {
                            // (eq? s1 s)
                            imports::eq_p(&[s1__0.clone(), s__0.clone()])
                        }
                    })
                },
                args__0.clone(),
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
