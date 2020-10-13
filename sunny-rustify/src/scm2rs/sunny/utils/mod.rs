#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::any;
    pub use super::atom_p;
    pub use super::bor;
    pub use super::dotted_minus_list_p;
    pub use super::filter;
    pub use super::last_minus_cdr;
    pub use super::proper_minus_list_minus_part;
    pub use super::reduce;
    pub use super::same_minus_name_p;
    pub use super::sort;
}

pub fn any(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let f__2 = args[0].clone();
        let seq__18 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__18.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f__2.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__18.clone()])
                }])
            })
            .is_true()
            {
                Scm::True
            } else {
                {
                    // (any f (cdr seq))
                    Scm::func(any).invoke(&[f__2.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__18.clone()])
                    }])
                }
            }
        } else {
            Scm::False
        }
    }
    .into()
}
pub fn atom_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x__28 = args[0].clone();
        if ({
            // (pair? x)
            imports::pair_p(&[x__28.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            Scm::True
        }
    }
    .into()
}
pub fn bor(args: &[Scm]) -> Scm {
    {
        if args.len() < 1 {
            panic!("not enough args")
        }
        let first__0 = args[0].clone();
        let args__3 = Scm::list(&args[1..]);
        if (first__0.clone()).is_true() {
            first__0.clone()
        } else if ({
            // (null? args)
            imports::null_p(&[args__3.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (apply bor args)
                imports::apply(&[Scm::func(bor), args__3.clone()])
            }
        }
    }
    .into()
}
pub fn dotted_minus_list_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__13 = args[0].clone();
        {
            // (not (null? (last-cdr seq)))
            imports::not(&[{
                // (null? (last-cdr seq))
                imports::null_p(&[{
                    // (last-cdr seq)
                    Scm::func(last_minus_cdr).invoke(&[seq__13.clone()])
                }])
            }])
        }
    }
    .into()
}
pub fn filter(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let f__0 = args[0].clone();
        let seq__16 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__16.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f__0.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__16.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (cons (car seq) (filter f (cdr seq)))
                    imports::cons(&[
                        {
                            // (car seq)
                            imports::car(&[seq__16.clone()])
                        },
                        {
                            // (filter f (cdr seq))
                            Scm::func(filter).invoke(&[f__0.clone(), {
                                // (cdr seq)
                                imports::cdr(&[seq__16.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (filter f (cdr seq))
                    Scm::func(filter).invoke(&[f__0.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__16.clone()])
                    }])
                }
            }
        } else {
            Scm::Nil
        }
    }
    .into()
}
pub fn last_minus_cdr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__14 = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__14.clone()])
        })
        .is_true()
        {
            {
                // (last-cdr (cdr seq))
                Scm::func(last_minus_cdr).invoke(&[{
                    // (cdr seq)
                    imports::cdr(&[seq__14.clone()])
                }])
            }
        } else {
            seq__14.clone()
        }
    }
    .into()
}
pub fn proper_minus_list_minus_part(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__15 = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__15.clone()])
        })
        .is_true()
        {
            {
                // (cons (car seq) (proper-list-part (cdr seq)))
                imports::cons(&[
                    {
                        // (car seq)
                        imports::car(&[seq__15.clone()])
                    },
                    {
                        // (proper-list-part (cdr seq))
                        Scm::func(proper_minus_list_minus_part).invoke(&[{
                            // (cdr seq)
                            imports::cdr(&[seq__15.clone()])
                        }])
                    },
                ])
            }
        } else {
            Scm::Nil
        }
    }
    .into()
}
pub fn reduce(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let f__1 = args[0].clone();
        let init__2 = args[1].clone();
        let seq__17 = args[2].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__17.clone()])
        })
        .is_true()
        {
            {
                // (reduce f (f init (car seq)) (cdr seq))
                Scm::func(reduce).invoke(&[
                    f__1.clone(),
                    {
                        // (f init (car seq))
                        f__1.clone().invoke(&[init__2.clone(), {
                            // (car seq)
                            imports::car(&[seq__17.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        imports::cdr(&[seq__17.clone()])
                    },
                ])
            }
        } else {
            init__2.clone()
        }
    }
    .into()
}
pub fn same_minus_name_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let a__0 = args[0].clone();
        let b__0 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (and (symbol? a) (symbol? b))
                if ({
                    // (symbol? a)
                    imports::symbol_p(&[a__0.clone()])
                })
                .is_true()
                {
                    {
                        // (symbol? b)
                        imports::symbol_p(&[b__0.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (eq? a b)
                    imports::eq_p(&[a__0.clone(), b__0.clone()])
                }
            } else if ({
                // (and (string? a) (string? b))
                if ({
                    // (string? a)
                    imports::string_p(&[a__0.clone()])
                })
                .is_true()
                {
                    {
                        // (string? b)
                        imports::string_p(&[b__0.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (equal? a b)
                    imports::equal_p(&[a__0.clone(), b__0.clone()])
                }
            } else if ({
                // (symbol? a)
                imports::symbol_p(&[a__0.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? (symbol->string a) b)
                    Scm::func(same_minus_name_p).invoke(&[
                        {
                            // (symbol->string a)
                            imports::symbol_minus__g_string(&[a__0.clone()])
                        },
                        b__0.clone(),
                    ])
                }
            } else if ({
                // (symbol? b)
                imports::symbol_p(&[b__0.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? a (symbol->string b))
                    Scm::func(same_minus_name_p).invoke(&[a__0.clone(), {
                        // (symbol->string b)
                        imports::symbol_minus__g_string(&[b__0.clone()])
                    }])
                }
            } else {
                {
                    // (error "invalid names" a b)
                    imports::error(&[Scm::from("invalid names"), a__0.clone(), b__0.clone()])
                }
            }
        }
    }
    .into()
}
pub fn sort(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let cmp__0 = args[0].clone();
        let ass__0 = args[1].clone();
        if ({
            // (pair? ass)
            imports::pair_p(&[ass__0.clone()])
        })
        .is_true()
        {
            {
                // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                {
                    let pivot__0 = {
                        // (car ass)
                        imports::car(&[ass__0.clone()])
                    };
                    {
                        // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                        imports::append(&[
                            {
                                // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                Scm::func(sort).invoke(&[cmp__0.clone(), {
                                    // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                    filter(&[
                                        {
                                            // Closure
                                            let cmp__0 = cmp__0.clone();
                                            let pivot__0 = pivot__0.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let x__29 = args[0].clone();
                                                {
                                                    // (cmp x pivot)
                                                    cmp__0
                                                        .clone()
                                                        .invoke(&[x__29.clone(), pivot__0.clone()])
                                                }
                                            })
                                        },
                                        {
                                            // (cdr ass)
                                            imports::cdr(&[ass__0.clone()])
                                        },
                                    ])
                                }])
                            },
                            {
                                // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                imports::cons(&[pivot__0.clone(), {
                                    // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                    Scm::func(sort).invoke(&[cmp__0.clone(), {
                                        // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                        filter(&[
                                            {
                                                // Closure
                                                let cmp__0 = cmp__0.clone();
                                                let pivot__0 = pivot__0.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x__30 = args[0].clone();
                                                    {
                                                        // (not (cmp x pivot))
                                                        imports::not(&[{
                                                            // (cmp x pivot)
                                                            cmp__0.clone().invoke(&[
                                                                x__30.clone(),
                                                                pivot__0.clone(),
                                                            ])
                                                        }])
                                                    }
                                                })
                                            },
                                            {
                                                // (cdr ass)
                                                imports::cdr(&[ass__0.clone()])
                                            },
                                        ])
                                    }])
                                }])
                            },
                        ])
                    }
                }
            }
        } else {
            Scm::Nil
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
    {
        (/*NOP*/);
        {
            // (define (atom? x) ...)
            (/*NOP*/)
        };
        {
            // (define (dotted-list? seq) ...)
            (/*NOP*/)
        };
        {
            // (define (last-cdr seq) ...)
            (/*NOP*/)
        };
        {
            // (define (proper-list-part seq) ...)
            (/*NOP*/)
        };
        {
            // (define (filter f seq) ...)
            (/*NOP*/)
        };
        {
            // (define (reduce f init seq) ...)
            (/*NOP*/)
        };
        {
            // (define (any f seq) ...)
            (/*NOP*/)
        };
        {
            // (define (bor first . args) ...)
            (/*NOP*/)
        };
        {
            // (define (sort cmp ass) ...)
            (/*NOP*/)
        };
        {
            // (define (same-name? a b) ...)
            (/*NOP*/)
        }
    };
}
