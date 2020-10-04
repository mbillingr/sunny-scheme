#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::any;
    pub use super::globals::atom_p;
    pub use super::globals::bor;
    pub use super::globals::dotted_minus_list_p;
    pub use super::globals::filter;
    pub use super::globals::last_minus_cdr;
    pub use super::globals::proper_minus_list_minus_part;
    pub use super::globals::reduce;
    pub use super::globals::sort;
}

mod globals {
    use sunny_core::{Mut, Scm};
    pub fn any(args: &[Scm]) -> Scm {
        {
            if args.len() != 2 {
                panic!("invalid arity")
            }
            let f = args[0].clone();
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
                    // (f (car seq))
                    f.clone().invoke(&[{
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
                        // (any f (cdr seq))
                        Scm::func(globals::any).invoke(&[f.clone(), {
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
    pub fn atom_p(args: &[Scm]) -> Scm {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            if ({
                // (pair? x)
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[x.clone()])
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
                imports::null_p
                    .with(|value| value.get())
                    .invoke(&[args_.clone()])
            })
            .is_true()
            {
                Scm::False
            } else {
                {
                    // (apply bor args)
                    imports::apply
                        .with(|value| value.get())
                        .invoke(&[Scm::func(globals::bor), args_.clone()])
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
                imports::not.with(|value| value.get()).invoke(&[{
                    // (null? (last-cdr seq))
                    imports::null_p.with(|value| value.get()).invoke(&[{
                        // (last-cdr seq)
                        Scm::func(globals::last_minus_cdr).invoke(&[seq.clone()])
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
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                if ({
                    // (f (car seq))
                    f.clone().invoke(&[{
                        // (car seq)
                        imports::car
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
                    }])
                })
                .is_true()
                {
                    {
                        // (cons (car seq) (filter f (cdr seq)))
                        imports::cons.with(|value| value.get()).invoke(&[
                            {
                                // (car seq)
                                imports::car
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
                            },
                            {
                                // (filter f (cdr seq))
                                Scm::func(globals::filter).invoke(&[f.clone(), {
                                    // (cdr seq)
                                    imports::cdr
                                        .with(|value| value.get())
                                        .invoke(&[seq.clone()])
                                }])
                            },
                        ])
                    }
                } else {
                    {
                        // (filter f (cdr seq))
                        Scm::func(globals::filter).invoke(&[f.clone(), {
                            // (cdr seq)
                            imports::cdr
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
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
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                {
                    // (last-cdr (cdr seq))
                    Scm::func(globals::last_minus_cdr).invoke(&[{
                        // (cdr seq)
                        imports::cdr
                            .with(|value| value.get())
                            .invoke(&[seq.clone()])
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
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                {
                    // (cons (car seq) (proper-list-part (cdr seq)))
                    imports::cons.with(|value| value.get()).invoke(&[
                        {
                            // (car seq)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[seq.clone()])
                        },
                        {
                            // (proper-list-part (cdr seq))
                            Scm::func(globals::proper_minus_list_minus_part).invoke(&[{
                                // (cdr seq)
                                imports::cdr
                                    .with(|value| value.get())
                                    .invoke(&[seq.clone()])
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
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[seq.clone()])
            })
            .is_true()
            {
                {
                    // (reduce f (f init (car seq)) (cdr seq))
                    Scm::func(globals::reduce).invoke(&[
                        f.clone(),
                        {
                            // (f init (car seq))
                            f.clone().invoke(&[init.clone(), {
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
                imports::pair_p
                    .with(|value| value.get())
                    .invoke(&[ass.clone()])
            })
            .is_true()
            {
                {
                    // (let ((pivot (car ass))) (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))))
                    {
                        let pivot = {
                            // (car ass)
                            imports::car
                                .with(|value| value.get())
                                .invoke(&[ass.clone()])
                        };
                        {
                            // (append (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass))) (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))))
                            imports::append.with(|value| value.get()).invoke(&[
                                {
                                    // (sort cmp (filter (lambda (x) (cmp x pivot)) (cdr ass)))
                                    Scm::func(globals::sort).invoke(&[cmp.clone(), {
                                        // (filter (lambda (x) (cmp x pivot)) (cdr ass))
                                        Scm::func(globals::filter).invoke(&[
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
                                                        cmp.clone()
                                                            .invoke(&[x.clone(), pivot.clone()])
                                                    }
                                                })
                                            },
                                            {
                                                // (cdr ass)
                                                imports::cdr
                                                    .with(|value| value.get())
                                                    .invoke(&[ass.clone()])
                                            },
                                        ])
                                    }])
                                },
                                {
                                    // (cons pivot (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass))))
                                    imports::cons.with(|value| value.get()).invoke(&[
                                        pivot.clone(),
                                        {
                                            // (sort cmp (filter (lambda (x) (not (cmp x pivot))) (cdr ass)))
                                            Scm::func(globals::sort).invoke(&[cmp.clone(), {
                                                // (filter (lambda (x) (not (cmp x pivot))) (cdr ass))
                                                Scm::func(globals::filter).invoke(&[
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
                                                                imports::not
                                                                    .with(|value| value.get())
                                                                    .invoke(&[{
                                                                        // (cmp x pivot)
                                                                        cmp.clone().invoke(&[
                                                                            x.clone(),
                                                                            pivot.clone(),
                                                                        ])
                                                                    }])
                                                            }
                                                        })
                                                    },
                                                    {
                                                        // (cdr ass)
                                                        imports::cdr
                                                            .with(|value| value.get())
                                                            .invoke(&[ass.clone()])
                                                    },
                                                ])
                                            }])
                                        },
                                    ])
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
