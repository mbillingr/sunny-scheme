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
        let set = args[0].clone();
        let item = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set)
                imports::null_p(&[set.clone()])
            })
            .is_true()
            {
                {
                    // (cons item (quote ()))
                    imports::cons(&[item.clone(), Scm::Nil])
                }
            } else if ({
                // (equal? (car set) item)
                imports::equal_p(&[
                    {
                        // (car set)
                        imports::car(&[set.clone()])
                    },
                    item.clone(),
                ])
            })
            .is_true()
            {
                set.clone()
            } else {
                {
                    // (cons (car set) (set-add (cdr set) item))
                    imports::cons(&[
                        {
                            // (car set)
                            imports::car(&[set.clone()])
                        },
                        {
                            // (set-add (cdr set) item)
                            Scm::func(set_minus_add).invoke(&[
                                {
                                    // (cdr set)
                                    imports::cdr(&[set.clone()])
                                },
                                item.clone(),
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
        let set = args[0].clone();
        let item_star_ = args[1].clone();
        {
            // (set-do* set-add set item*)
            Scm::func(set_minus_do_star_).invoke(&[
                Scm::func(set_minus_add),
                set.clone(),
                item_star_.clone(),
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
        let func = args[0].clone();
        let set = args[1].clone();
        let item_star_ = args[2].clone();
        if ({
            // (null? item*)
            imports::null_p(&[item_star_.clone()])
        })
        .is_true()
        {
            set.clone()
        } else {
            {
                // (set-do* func (func set (car item*)) (cdr item*))
                Scm::func(set_minus_do_star_).invoke(&[
                    func.clone(),
                    {
                        // (func set (car item*))
                        func.clone().invoke(&[set.clone(), {
                            // (car item*)
                            imports::car(&[item_star_.clone()])
                        }])
                    },
                    {
                        // (cdr item*)
                        imports::cdr(&[item_star_.clone()])
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
        let set = args[0].clone();
        {
            // (null? set)
            imports::null_p(&[set.clone()])
        }
    }
    .into()
}
pub fn set_minus_remove(args: &[Scm]) -> Scm {
    {
        if args.len() != 2 {
            panic!("invalid arity")
        }
        let set = args[0].clone();
        let item = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set)
                imports::null_p(&[set.clone()])
            })
            .is_true()
            {
                Scm::Nil
            } else if ({
                // (equal? (car set) item)
                imports::equal_p(&[
                    {
                        // (car set)
                        imports::car(&[set.clone()])
                    },
                    item.clone(),
                ])
            })
            .is_true()
            {
                {
                    // (cdr set)
                    imports::cdr(&[set.clone()])
                }
            } else {
                {
                    // (cons (car set) (set-remove (cdr set) item))
                    imports::cons(&[
                        {
                            // (car set)
                            imports::car(&[set.clone()])
                        },
                        {
                            // (set-remove (cdr set) item)
                            Scm::func(set_minus_remove).invoke(&[
                                {
                                    // (cdr set)
                                    imports::cdr(&[set.clone()])
                                },
                                item.clone(),
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
        let set = args[0].clone();
        let item_star_ = args[1].clone();
        {
            // (set-do* set-remove set item*)
            Scm::func(set_minus_do_star_).invoke(&[
                Scm::func(set_minus_remove),
                set.clone(),
                item_star_.clone(),
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
        let set1 = args[0].clone();
        let set2 = args[1].clone();
        {
            // (cond ...)
            if ({
                // (null? set1)
                imports::null_p(&[set1.clone()])
            })
            .is_true()
            {
                set2.clone()
            } else if ({
                // (null? set2)
                imports::null_p(&[set2.clone()])
            })
            .is_true()
            {
                set1.clone()
            } else {
                {
                    // (set-add* set1 set2)
                    set_minus_add_star_(&[set1.clone(), set2.clone()])
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
