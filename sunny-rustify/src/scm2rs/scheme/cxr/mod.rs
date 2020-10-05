#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::{caar, cadr, car, cdar, cddr, cdr};
}

pub mod exports {
    pub use super::caaaar;
    pub use super::caaadr;
    pub use super::caaar;
    pub use super::caadar;
    pub use super::caaddr;
    pub use super::caadr;
    pub use super::cadaar;
    pub use super::cadadr;
    pub use super::cadar;
    pub use super::caddar;
    pub use super::cadddr;
    pub use super::caddr;
    pub use super::cdaaar;
    pub use super::cdaadr;
    pub use super::cdaar;
    pub use super::cdadar;
    pub use super::cdaddr;
    pub use super::cdadr;
    pub use super::cddaar;
    pub use super::cddadr;
    pub use super::cddar;
    pub use super::cdddar;
    pub use super::cddddr;
    pub use super::cdddr;
}

pub fn caaaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (caar (caar x))
            Scm::func(imports::caar).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caaadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (caar (cadr x))
            Scm::func(imports::caar).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (car (caar x))
            Scm::func(imports::car).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caadar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (caar (cdar x))
            Scm::func(imports::caar).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caaddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (caar (cddr x))
            Scm::func(imports::caar).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (car (cadr x))
            Scm::func(imports::car).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cadaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cadr (caar x))
            Scm::func(imports::cadr).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cadadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cadr (cadr x))
            Scm::func(imports::cadr).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cadar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (car (cdar x))
            Scm::func(imports::car).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caddar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cadr (cdar x))
            Scm::func(imports::cadr).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cadddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cadr (cddr x))
            Scm::func(imports::cadr).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn caddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (car (cddr x))
            Scm::func(imports::car).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdaaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdar (caar x))
            Scm::func(imports::cdar).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdaadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdar (cadr x))
            Scm::func(imports::cdar).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdr (caar x))
            Scm::func(imports::cdr).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdadar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdar (cdar x))
            Scm::func(imports::cdar).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdaddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdar (cddr x))
            Scm::func(imports::cdar).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdr (cadr x))
            Scm::func(imports::cdr).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cddaar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cddr (caar x))
            Scm::func(imports::cddr).invoke(&[{
                // (caar x)
                Scm::func(imports::caar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cddadr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cddr (cadr x))
            Scm::func(imports::cddr).invoke(&[{
                // (cadr x)
                Scm::func(imports::cadr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cddar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdr (cdar x))
            Scm::func(imports::cdr).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdddar(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cddr (cdar x))
            Scm::func(imports::cddr).invoke(&[{
                // (cdar x)
                Scm::func(imports::cdar).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cddddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cddr (cddr x))
            Scm::func(imports::cddr).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
        }
    }
    .into()
}
pub fn cdddr(args: &[Scm]) -> Scm {
    {
        if args.len() != 1 {
            panic!("invalid arity")
        }
        let x = args[0].clone();
        {
            // (cdr (cddr x))
            Scm::func(imports::cdr).invoke(&[{
                // (cddr x)
                Scm::func(imports::cddr).invoke(&[x.clone()])
            }])
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
