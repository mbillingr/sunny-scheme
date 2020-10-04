#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::{caar, cadr, car, cdar, cddr, cdr};
}

pub mod exports {
    pub use super::globals::caaaar;
    pub use super::globals::caaadr;
    pub use super::globals::caaar;
    pub use super::globals::caadar;
    pub use super::globals::caaddr;
    pub use super::globals::caadr;
    pub use super::globals::cadaar;
    pub use super::globals::cadadr;
    pub use super::globals::cadar;
    pub use super::globals::caddar;
    pub use super::globals::cadddr;
    pub use super::globals::caddr;
    pub use super::globals::cdaaar;
    pub use super::globals::cdaadr;
    pub use super::globals::cdaar;
    pub use super::globals::cdadar;
    pub use super::globals::cdaddr;
    pub use super::globals::cdadr;
    pub use super::globals::cddaar;
    pub use super::globals::cddadr;
    pub use super::globals::cddar;
    pub use super::globals::cdddar;
    pub use super::globals::cddddr;
    pub use super::globals::cdddr;
}

mod globals {
    use sunny_core::{Mut, Scm};
    pub fn caaaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (caar (caar x))
                imports::caar.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caaadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (caar (cadr x))
                imports::caar.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (car (caar x))
                imports::car.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caadar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (caar (cdar x))
                imports::caar.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caaddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (caar (cddr x))
                imports::caar.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (car (cadr x))
                imports::car.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cadaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cadr (caar x))
                imports::cadr.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cadadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cadr (cadr x))
                imports::cadr.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cadar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (car (cdar x))
                imports::car.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caddar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cadr (cdar x))
                imports::cadr.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cadddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cadr (cddr x))
                imports::cadr.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn caddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (car (cddr x))
                imports::car.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdaaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdar (caar x))
                imports::cdar.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdaadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdar (cadr x))
                imports::cdar.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdr (caar x))
                imports::cdr.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdadar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdar (cdar x))
                imports::cdar.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdaddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdar (cddr x))
                imports::cdar.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdr (cadr x))
                imports::cdr.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cddaar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cddr (caar x))
                imports::cddr.with(|value| value.get()).invoke(&[{
                    // (caar x)
                    imports::caar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cddadr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cddr (cadr x))
                imports::cddr.with(|value| value.get()).invoke(&[{
                    // (cadr x)
                    imports::cadr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cddar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdr (cdar x))
                imports::cdr.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdddar(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cddr (cdar x))
                imports::cddr.with(|value| value.get()).invoke(&[{
                    // (cdar x)
                    imports::cdar.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cddddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cddr (cddr x))
                imports::cddr.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
    }
    pub fn cdddr(args: &[Scm]) {
        {
            if args.len() != 1 {
                panic!("invalid arity")
            }
            let x = args[0].clone();
            {
                // (cdr (cddr x))
                imports::cdr.with(|value| value.get()).invoke(&[{
                    // (cddr x)
                    imports::cddr.with(|value| value.get()).invoke(&[x.clone()])
                }])
            }
        }
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
            // (define (caaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (caadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cadar x) ...)
            (/*NOP*/)
        };
        {
            // (define (caddr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cddar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdddr x) ...)
            (/*NOP*/)
        };
        {
            // (define (caaaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (caaadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (caadar x) ...)
            (/*NOP*/)
        };
        {
            // (define (caaddr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cadaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cadadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (caddar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cadddr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdaaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdaadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdadar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdaddr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cddaar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cddadr x) ...)
            (/*NOP*/)
        };
        {
            // (define (cdddar x) ...)
            (/*NOP*/)
        };
        {
            // (define (cddddr x) ...)
            (/*NOP*/)
        }
    };
}
