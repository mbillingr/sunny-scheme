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
        let f__83 = args[0].clone();
        let seq__82 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__82.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f__83.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__82.clone()])
                }])
            })
            .is_true()
            {
                Scm::True
            } else {
                {
                    // (any f (cdr seq))
                    Scm::func(any).invoke(&[f__83.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__82.clone()])
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
        let x__73 = args[0].clone();
        if ({
            // (pair? x)
            imports::pair_p(&[x__73.clone()])
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
        let first__84 = args[0].clone();
        let args__85 = Scm::list(&args[1..]);
        if (first__84.clone()).is_true() {
            first__84.clone()
        } else if ({
            // (null? args)
            imports::null_p(&[args__85.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (apply bor args)
                imports::apply(&[Scm::func(bor), args__85.clone()])
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
        let seq__74 = args[0].clone();
        {
            // (not (null? (last-cdr seq)))
            imports::not(&[{
                // (null? (last-cdr seq))
                imports::null_p(&[{
                    // (last-cdr seq)
                    Scm::func(last_minus_cdr).invoke(&[seq__74.clone()])
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
        let f__78 = args[0].clone();
        let seq__77 = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__77.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f__78.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq__77.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (cons (car seq) (filter f (cdr seq)))
                    imports::cons(&[
                        {
                            // (car seq)
                            imports::car(&[seq__77.clone()])
                        },
                        {
                            // (filter f (cdr seq))
                            Scm::func(filter).invoke(&[f__78.clone(), {
                                // (cdr seq)
                                imports::cdr(&[seq__77.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (filter f (cdr seq))
                    Scm::func(filter).invoke(&[f__78.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq__77.clone()])
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
        let seq__75 = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__75.clone()])
        })
        .is_true()
        {
            {
                // (last-cdr (cdr seq))
                Scm::func(last_minus_cdr).invoke(&[{
                    // (cdr seq)
                    imports::cdr(&[seq__75.clone()])
                }])
            }
        } else {
            seq__75.clone()
        }
    }
    .into()
}
pub fn proper_minus_list_minus_part(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq__76 = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__76.clone()])
        })
        .is_true()
        {
            {
                // (cons (car seq) (proper-list-part (cdr seq)))
                imports::cons(&[
                    {
                        // (car seq)
                        imports::car(&[seq__76.clone()])
                    },
                    {
                        // (proper-list-part (cdr seq))
                        Scm::func(proper_minus_list_minus_part).invoke(&[{
                            // (cdr seq)
                            imports::cdr(&[seq__76.clone()])
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
        let f__80 = args[0].clone();
        let init__81 = args[1].clone();
        let seq__79 = args[2].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq__79.clone()])
        })
        .is_true()
        {
            {
                // (reduce f (f init (car seq)) (cdr seq))
                Scm::func(reduce).invoke(&[
                    f__80.clone(),
                    {
                        // (f init (car seq))
                        f__80.clone().invoke(&[init__81.clone(), {
                            // (car seq)
                            imports::car(&[seq__79.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        imports::cdr(&[seq__79.clone()])
                    },
                ])
            }
        } else {
            init__81.clone()
        }
    }
    .into()
}
pub fn same_minus_name_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let a__91 = args[0].clone();
        let b__92 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (and (symbol? a) (symbol? b))
                if ({
                    // (symbol? a)
                    imports::symbol_p(&[a__91.clone()])
                })
                .is_true()
                {
                    {
                        // (symbol? b)
                        imports::symbol_p(&[b__92.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (eq? a b)
                    imports::eq_p(&[a__91.clone(), b__92.clone()])
                }
            } else if ({
                // (and (string? a) (string? b))
                if ({
                    // (string? a)
                    imports::string_p(&[a__91.clone()])
                })
                .is_true()
                {
                    {
                        // (string? b)
                        imports::string_p(&[b__92.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (equal? a b)
                    imports::equal_p(&[a__91.clone(), b__92.clone()])
                }
            } else if ({
                // (symbol? a)
                imports::symbol_p(&[a__91.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? (symbol->string a) b)
                    Scm::func(same_minus_name_p).invoke(&[
                        {
                            // (symbol->string a)
                            imports::symbol_minus__g_string(&[a__91.clone()])
                        },
                        b__92.clone(),
                    ])
                }
            } else if ({
                // (symbol? b)
                imports::symbol_p(&[b__92.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? a (symbol->string b))
                    Scm::func(same_minus_name_p).invoke(&[a__91.clone(), {
                        // (symbol->string b)
                        imports::symbol_minus__g_string(&[b__92.clone()])
                    }])
                }
            } else {
                {
                    // (error "invalid names" a b)
                    imports::error(&[Scm::from("invalid names"), a__91.clone(), b__92.clone()])
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
        let cmp__87 = args[0].clone();
        let ass__86 = args[1].clone();
        if ({
            // (pair? ass)
            imports::pair_p(&[ass__86.clone()])
        })
        .is_true()
        {
            {
                // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                {
                    let pivot__89 = {
                        // (car ass)
                        imports::car(&[ass__86.clone()])
                    };
                    {
                        // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                        imports::append(&[
                            {
                                // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                Scm::func(sort).invoke(&[cmp__87.clone(), {
                                    // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                    filter(&[
                                        {
                                            // Closure
                                            let cmp__87 = cmp__87.clone();
                                            let pivot__89 = pivot__89.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let x__88 = args[0].clone();
                                                {
                                                    // (cmp x pivot)
                                                    cmp__87
                                                        .clone()
                                                        .invoke(&[x__88.clone(), pivot__89.clone()])
                                                }
                                            })
                                        },
                                        {
                                            // (cdr ass)
                                            imports::cdr(&[ass__86.clone()])
                                        },
                                    ])
                                }])
                            },
                            {
                                // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                imports::cons(&[pivot__89.clone(), {
                                    // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                    Scm::func(sort).invoke(&[cmp__87.clone(), {
                                        // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                        filter(&[
                                            {
                                                // Closure
                                                let cmp__87 = cmp__87.clone();
                                                let pivot__89 = pivot__89.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x__90 = args[0].clone();
                                                    {
                                                        // (not (cmp x pivot))
                                                        imports::not(&[{
                                                            // (cmp x pivot)
                                                            cmp__87.clone().invoke(&[
                                                                x__90.clone(),
                                                                pivot__89.clone(),
                                                            ])
                                                        }])
                                                    }
                                                })
                                            },
                                            {
                                                // (cdr ass)
                                                imports::cdr(&[ass__86.clone()])
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
