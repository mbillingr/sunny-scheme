#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::make_minus_set;
    pub use super::set_minus_add;
    pub use super::set_minus_add_star_;
    pub use super::set_minus_do_star_;
    pub use super::set_minus_empty_p;
    pub use super::set_minus_remove;
    pub use super::set_minus_remove_star_;
    pub use super::set_minus_union;
}

pub fn make_minus_set(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        Scm::Nil
    }
    .into()
}
pub fn set_minus_add(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set_1 = args[0].clone();
        let item_0 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set)
                imports::null_p(&[set_1.clone()])
            })
            .is_true()
            {
                {
                    // (cons item (quote ()))
                    imports::cons(&[item_0.clone(), Scm::Nil])
                }
            } else if ({
                // (equal? (car set) item)
                imports::equal_p(&[
                    {
                        // (car set)
                        imports::car(&[set_1.clone()])
                    },
                    item_0.clone(),
                ])
            })
            .is_true()
            {
                set_1.clone()
            } else {
                {
                    // (cons (car set) (set-add (cdr set) item))
                    imports::cons(&[
                        {
                            // (car set)
                            imports::car(&[set_1.clone()])
                        },
                        {
                            // (set-add (cdr set) item)
                            Scm::func(set_minus_add).invoke(&[
                                {
                                    // (cdr set)
                                    imports::cdr(&[set_1.clone()])
                                },
                                item_0.clone(),
                            ])
                        },
                    ])
                }
            }
        }
    }
    .into()
}
pub fn set_minus_add_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set_3 = args[0].clone();
        let item_star__0 = args[1].clone();
        {
            // (set-do* set-add set item*)
            Scm::func(set_minus_do_star_).invoke(&[
                Scm::func(set_minus_add),
                set_3.clone(),
                item_star__0.clone(),
            ])
        }
    }
    .into()
}
pub fn set_minus_do_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 3 {
            panic!("invalid arity")
        }
        let func_5 = args[0].clone();
        let set_5 = args[1].clone();
        let item_star__2 = args[2].clone();
        if ({
            // (null? item*)
            imports::null_p(&[item_star__2.clone()])
        })
        .is_true()
        {
            set_5.clone()
        } else {
            {
                // (set-do* func (func set (car item*)) (cdr item*))
                Scm::func(set_minus_do_star_).invoke(&[
                    func_5.clone(),
                    {
                        // (func set (car item*))
                        func_5.clone().invoke(&[set_5.clone(), {
                            // (car item*)
                            imports::car(&[item_star__2.clone()])
                        }])
                    },
                    {
                        // (cdr item*)
                        imports::cdr(&[item_star__2.clone()])
                    },
                ])
            }
        }
    }
    .into()
}
pub fn set_minus_empty_p(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let set_0 = args[0].clone();
        {
            // (null? set)
            imports::null_p(&[set_0.clone()])
        }
    }
    .into()
}
pub fn set_minus_remove(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set_2 = args[0].clone();
        let item_1 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set)
                imports::null_p(&[set_2.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else if ({
                // (equal? (car set) item)
                imports::equal_p(&[
                    {
                        // (car set)
                        imports::car(&[set_2.clone()])
                    },
                    item_1.clone(),
                ])
            })
            .is_true()
            {
                {
                    // (cdr set)
                    imports::cdr(&[set_2.clone()])
                }
            } else {
                {
                    // (cons (car set) (set-remove (cdr set) item))
                    imports::cons(&[
                        {
                            // (car set)
                            imports::car(&[set_2.clone()])
                        },
                        {
                            // (set-remove (cdr set) item)
                            Scm::func(set_minus_remove).invoke(&[
                                {
                                    // (cdr set)
                                    imports::cdr(&[set_2.clone()])
                                },
                                item_1.clone(),
                            ])
                        },
                    ])
                }
            }
        }
    }
    .into()
}
pub fn set_minus_remove_star_(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set_4 = args[0].clone();
        let item_star__1 = args[1].clone();
        {
            // (set-do* set-remove set item*)
            Scm::func(set_minus_do_star_).invoke(&[
                Scm::func(set_minus_remove),
                set_4.clone(),
                item_star__1.clone(),
            ])
        }
    }
    .into()
}
pub fn set_minus_union(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set1_0 = args[0].clone();
        let set2_0 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set1)
                imports::null_p(&[set1_0.clone()])
            })
            .is_true()
            {
                set2_0.clone()
            } else if ({
                // (null? set2)
                imports::null_p(&[set2_0.clone()])
            })
            .is_true()
            {
                set1_0.clone()
            } else {
                {
                    // (set-add* set1 set2)
                    set_minus_add_star_(&[set1_0.clone(), set2_0.clone()])
                }
            }
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
            // (define (make-set) ...)
            (/*NOP*/)
        };
        {
            // (define (set-empty? set) ...)
            (/*NOP*/)
        };
        {
            // (define (set-add set item) ...)
            (/*NOP*/)
        };
        {
            // (define (set-remove set item) ...)
            (/*NOP*/)
        };
        {
            // (define (set-add* set item*) ...)
            (/*NOP*/)
        };
        {
            // (define (set-remove* set item*) ...)
            (/*NOP*/)
        };
        {
            // (define (set-do* func set item*) ...)
            (/*NOP*/)
        };
        {
            // (define (set-union set1 set2) ...)
            (/*NOP*/)
        }
    };
}
