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
        let x__57 = args[0].clone();
        {
            // (caar (caar x))
            imports::caar(&[{
                // (caar x)
                imports::caar(&[x__57.clone()])
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
        let x__58 = args[0].clone();
        {
            // (caar (cadr x))
            imports::caar(&[{
                // (cadr x)
                imports::cadr(&[x__58.clone()])
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
        let x__49 = args[0].clone();
        {
            // (car (caar x))
            imports::car(&[{
                // (caar x)
                imports::caar(&[x__49.clone()])
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
        let x__59 = args[0].clone();
        {
            // (caar (cdar x))
            imports::caar(&[{
                // (cdar x)
                imports::cdar(&[x__59.clone()])
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
        let x__60 = args[0].clone();
        {
            // (caar (cddr x))
            imports::caar(&[{
                // (cddr x)
                imports::cddr(&[x__60.clone()])
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
        let x__50 = args[0].clone();
        {
            // (car (cadr x))
            imports::car(&[{
                // (cadr x)
                imports::cadr(&[x__50.clone()])
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
        let x__61 = args[0].clone();
        {
            // (cadr (caar x))
            imports::cadr(&[{
                // (caar x)
                imports::caar(&[x__61.clone()])
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
        let x__62 = args[0].clone();
        {
            // (cadr (cadr x))
            imports::cadr(&[{
                // (cadr x)
                imports::cadr(&[x__62.clone()])
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
        let x__51 = args[0].clone();
        {
            // (car (cdar x))
            imports::car(&[{
                // (cdar x)
                imports::cdar(&[x__51.clone()])
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
        let x__63 = args[0].clone();
        {
            // (cadr (cdar x))
            imports::cadr(&[{
                // (cdar x)
                imports::cdar(&[x__63.clone()])
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
        let x__64 = args[0].clone();
        {
            // (cadr (cddr x))
            imports::cadr(&[{
                // (cddr x)
                imports::cddr(&[x__64.clone()])
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
        let x__52 = args[0].clone();
        {
            // (car (cddr x))
            imports::car(&[{
                // (cddr x)
                imports::cddr(&[x__52.clone()])
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
        let x__65 = args[0].clone();
        {
            // (cdar (caar x))
            imports::cdar(&[{
                // (caar x)
                imports::caar(&[x__65.clone()])
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
        let x__66 = args[0].clone();
        {
            // (cdar (cadr x))
            imports::cdar(&[{
                // (cadr x)
                imports::cadr(&[x__66.clone()])
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
        let x__53 = args[0].clone();
        {
            // (cdr (caar x))
            imports::cdr(&[{
                // (caar x)
                imports::caar(&[x__53.clone()])
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
        let x__67 = args[0].clone();
        {
            // (cdar (cdar x))
            imports::cdar(&[{
                // (cdar x)
                imports::cdar(&[x__67.clone()])
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
        let x__68 = args[0].clone();
        {
            // (cdar (cddr x))
            imports::cdar(&[{
                // (cddr x)
                imports::cddr(&[x__68.clone()])
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
        let x__54 = args[0].clone();
        {
            // (cdr (cadr x))
            imports::cdr(&[{
                // (cadr x)
                imports::cadr(&[x__54.clone()])
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
        let x__69 = args[0].clone();
        {
            // (cddr (caar x))
            imports::cddr(&[{
                // (caar x)
                imports::caar(&[x__69.clone()])
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
        let x__70 = args[0].clone();
        {
            // (cddr (cadr x))
            imports::cddr(&[{
                // (cadr x)
                imports::cadr(&[x__70.clone()])
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
        let x__55 = args[0].clone();
        {
            // (cdr (cdar x))
            imports::cdr(&[{
                // (cdar x)
                imports::cdar(&[x__55.clone()])
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
        let x__71 = args[0].clone();
        {
            // (cddr (cdar x))
            imports::cddr(&[{
                // (cdar x)
                imports::cdar(&[x__71.clone()])
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
        let x__72 = args[0].clone();
        {
            // (cddr (cddr x))
            imports::cddr(&[{
                // (cddr x)
                imports::cddr(&[x__72.clone()])
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
        let x__56 = args[0].clone();
        {
            // (cdr (cddr x))
            imports::cdr(&[{
                // (cddr x)
                imports::cddr(&[x__56.clone()])
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
