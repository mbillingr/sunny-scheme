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
        let f = args[0].clone();
        let seq = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq.clone()])
                }])
            })
            .is_true()
            {
                Scm::True
            } else {
                {
                    // (any f (cdr seq))
                    Scm::func(any).invoke(&[f.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq.clone()])
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
        let x = args[0].clone();
        if ({
            // (pair? x)
            imports::pair_p(&[x.clone()])
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
        let first = args[0].clone();
        let args_ = Scm::list(&args[1..]);
        if (first.clone()).is_true() {
            first.clone()
        } else if ({
            // (null? args)
            imports::null_p(&[args_.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (apply bor args)
                imports::apply(&[Scm::func(bor), args_.clone()])
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
        let seq = args[0].clone();
        {
            // (not (null? (last-cdr seq)))
            imports::not(&[{
                // (null? (last-cdr seq))
                imports::null_p(&[{
                    // (last-cdr seq)
                    Scm::func(last_minus_cdr).invoke(&[seq.clone()])
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
        let f = args[0].clone();
        let seq = args[1].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f.clone().invoke(&[{
                    // (car seq)
                    imports::car(&[seq.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (cons (car seq) (filter f (cdr seq)))
                    imports::cons(&[
                        {
                            // (car seq)
                            imports::car(&[seq.clone()])
                        },
                        {
                            // (filter f (cdr seq))
                            Scm::func(filter).invoke(&[f.clone(), {
                                // (cdr seq)
                                imports::cdr(&[seq.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (filter f (cdr seq))
                    Scm::func(filter).invoke(&[f.clone(), {
                        // (cdr seq)
                        imports::cdr(&[seq.clone()])
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
        let seq = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq.clone()])
        })
        .is_true()
        {
            {
                // (last-cdr (cdr seq))
                Scm::func(last_minus_cdr).invoke(&[{
                    // (cdr seq)
                    imports::cdr(&[seq.clone()])
                }])
            }
        } else {
            seq.clone()
        }
    }
    .into()
}
pub fn proper_minus_list_minus_part(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let seq = args[0].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq.clone()])
        })
        .is_true()
        {
            {
                // (cons (car seq) (proper-list-part (cdr seq)))
                imports::cons(&[
                    {
                        // (car seq)
                        imports::car(&[seq.clone()])
                    },
                    {
                        // (proper-list-part (cdr seq))
                        Scm::func(proper_minus_list_minus_part).invoke(&[{
                            // (cdr seq)
                            imports::cdr(&[seq.clone()])
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
        let f = args[0].clone();
        let init = args[1].clone();
        let seq = args[2].clone();
        if ({
            // (pair? seq)
            imports::pair_p(&[seq.clone()])
        })
        .is_true()
        {
            {
                // (reduce f (f init (car seq)) (cdr seq))
                Scm::func(reduce).invoke(&[
                    f.clone(),
                    {
                        // (f init (car seq))
                        f.clone().invoke(&[init.clone(), {
                            // (car seq)
                            imports::car(&[seq.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        imports::cdr(&[seq.clone()])
                    },
                ])
            }
        } else {
            init.clone()
        }
    }
    .into()
}
pub fn same_minus_name_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let a = args[0].clone();
        let b = args[1].clone();
        {
            // (cond ...)
            if ({
                // (and (symbol? a) (symbol? b))
                if ({
                    // (symbol? a)
                    imports::symbol_p(&[a.clone()])
                })
                .is_true()
                {
                    {
                        // (symbol? b)
                        imports::symbol_p(&[b.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (eq? a b)
                    imports::eq_p(&[a.clone(), b.clone()])
                }
            } else if ({
                // (and (string? a) (string? b))
                if ({
                    // (string? a)
                    imports::string_p(&[a.clone()])
                })
                .is_true()
                {
                    {
                        // (string? b)
                        imports::string_p(&[b.clone()])
                    }
                } else {
                    Scm::False
                }
            })
            .is_true()
            {
                {
                    // (equal? a b)
                    imports::equal_p(&[a.clone(), b.clone()])
                }
            } else if ({
                // (symbol? a)
                imports::symbol_p(&[a.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? (symbol->string a) b)
                    Scm::func(same_minus_name_p).invoke(&[
                        {
                            // (symbol->string a)
                            imports::symbol_minus__g_string(&[a.clone()])
                        },
                        b.clone(),
                    ])
                }
            } else if ({
                // (symbol? b)
                imports::symbol_p(&[b.clone()])
            })
            .is_true()
            {
                {
                    // (same-name? a (symbol->string b))
                    Scm::func(same_minus_name_p).invoke(&[a.clone(), {
                        // (symbol->string b)
                        imports::symbol_minus__g_string(&[b.clone()])
                    }])
                }
            } else {
                {
                    // (error "invalid names" a b)
                    imports::error(&[Scm::from("invalid names"), a.clone(), b.clone()])
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
        let cmp = args[0].clone();
        let ass = args[1].clone();
        if ({
            // (pair? ass)
            imports::pair_p(&[ass.clone()])
        })
        .is_true()
        {
            {
                // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                {
                    let pivot = {
                        // (car ass)
                        imports::car(&[ass.clone()])
                    };
                    {
                        // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                        imports::append(&[
                            {
                                // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                Scm::func(sort).invoke(&[cmp.clone(), {
                                    // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                    filter(&[
                                        {
                                            // Closure
                                            let cmp = cmp.clone();
                                            let pivot = pivot.clone();
                                            Scm::func(move |args: &[Scm]| {
                                                if args.len() != 1 {
                                                    panic!("invalid arity")
                                                }
                                                let x = args[0].clone();
                                                {
                                                    // (cmp x pivot)
                                                    cmp.clone().invoke(&[x.clone(), pivot.clone()])
                                                }
                                            })
                                        },
                                        {
                                            // (cdr ass)
                                            imports::cdr(&[ass.clone()])
                                        },
                                    ])
                                }])
                            },
                            {
                                // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                imports::cons(&[pivot.clone(), {
                                    // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                    Scm::func(sort).invoke(&[cmp.clone(), {
                                        // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                        filter(&[
                                            {
                                                // Closure
                                                let cmp = cmp.clone();
                                                let pivot = pivot.clone();
                                                Scm::func(move |args: &[Scm]| {
                                                    if args.len() != 1 {
                                                        panic!("invalid arity")
                                                    }
                                                    let x = args[0].clone();
                                                    {
                                                        // (not (cmp x pivot))
                                                        imports::not(&[{
                                                            // (cmp x pivot)
                                                            cmp.clone()
                                                                .invoke(&[x.clone(), pivot.clone()])
                                                        }])
                                                    }
                                                })
                                            },
                                            {
                                                // (cdr ass)
                                                imports::cdr(&[ass.clone()])
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
