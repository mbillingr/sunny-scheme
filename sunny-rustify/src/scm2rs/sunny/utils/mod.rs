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
            Scm::func(imports::pair_p).invoke(&[seq.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f.clone().invoke(&[{
                    // (car seq)
                    Scm::func(imports::car).invoke(&[seq.clone()])
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
                        Scm::func(imports::cdr).invoke(&[seq.clone()])
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
            Scm::func(imports::pair_p).invoke(&[x.clone()])
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
            Scm::func(imports::null_p).invoke(&[args_.clone()])
        })
        .is_true()
        {
            Scm::False
        } else {
            {
                // (apply bor args)
                Scm::func(imports::apply).invoke(&[Scm::func(bor), args_.clone()])
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
            Scm::func(imports::not).invoke(&[{
                // (null? (last-cdr seq))
                Scm::func(imports::null_p).invoke(&[{
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
            Scm::func(imports::pair_p).invoke(&[seq.clone()])
        })
        .is_true()
        {
            if ({
                // (f (car seq))
                f.clone().invoke(&[{
                    // (car seq)
                    Scm::func(imports::car).invoke(&[seq.clone()])
                }])
            })
            .is_true()
            {
                {
                    // (cons (car seq) (filter f (cdr seq)))
                    Scm::func(imports::cons).invoke(&[
                        {
                            // (car seq)
                            Scm::func(imports::car).invoke(&[seq.clone()])
                        },
                        {
                            // (filter f (cdr seq))
                            Scm::func(filter).invoke(&[f.clone(), {
                                // (cdr seq)
                                Scm::func(imports::cdr).invoke(&[seq.clone()])
                            }])
                        },
                    ])
                }
            } else {
                {
                    // (filter f (cdr seq))
                    Scm::func(filter).invoke(&[f.clone(), {
                        // (cdr seq)
                        Scm::func(imports::cdr).invoke(&[seq.clone()])
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
            Scm::func(imports::pair_p).invoke(&[seq.clone()])
        })
        .is_true()
        {
            {
                // (last-cdr (cdr seq))
                Scm::func(last_minus_cdr).invoke(&[{
                    // (cdr seq)
                    Scm::func(imports::cdr).invoke(&[seq.clone()])
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
            Scm::func(imports::pair_p).invoke(&[seq.clone()])
        })
        .is_true()
        {
            {
                // (cons (car seq) (proper-list-part (cdr seq)))
                Scm::func(imports::cons).invoke(&[
                    {
                        // (car seq)
                        Scm::func(imports::car).invoke(&[seq.clone()])
                    },
                    {
                        // (proper-list-part (cdr seq))
                        Scm::func(proper_minus_list_minus_part).invoke(&[{
                            // (cdr seq)
                            Scm::func(imports::cdr).invoke(&[seq.clone()])
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
            Scm::func(imports::pair_p).invoke(&[seq.clone()])
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
                            Scm::func(imports::car).invoke(&[seq.clone()])
                        }])
                    },
                    {
                        // (cdr seq)
                        Scm::func(imports::cdr).invoke(&[seq.clone()])
                    },
                ])
            }
        } else {
            init.clone()
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
            Scm::func(imports::pair_p).invoke(&[ass.clone()])
        })
        .is_true()
        {
            {
                // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                {
                    let pivot = {
                        // (car ass)
                        Scm::func(imports::car).invoke(&[ass.clone()])
                    };
                    {
                        // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                        Scm::func(imports::append).invoke(&[
                            {
                                // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                Scm::func(sort).invoke(&[cmp.clone(), {
                                    // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                    Scm::func(filter).invoke(&[
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
                                            Scm::func(imports::cdr).invoke(&[ass.clone()])
                                        },
                                    ])
                                }])
                            },
                            {
                                // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                Scm::func(imports::cons).invoke(&[pivot.clone(), {
                                    // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                    Scm::func(sort).invoke(&[cmp.clone(), {
                                        // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                        Scm::func(filter).invoke(&[
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
                                                        Scm::func(imports::not).invoke(&[{
                                                            // (cmp x pivot)
                                                            cmp.clone()
                                                                .invoke(&[x.clone(), pivot.clone()])
                                                        }])
                                                    }
                                                })
                                            },
                                            {
                                                // (cdr ass)
                                                Scm::func(imports::cdr).invoke(&[ass.clone()])
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
        }
    };
}
