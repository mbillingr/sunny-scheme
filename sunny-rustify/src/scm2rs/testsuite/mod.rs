#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::run_minus_tests;
}

pub fn run_minus_tests(args: &[Scm]) -> Scm {
    {
        if args.len() != 0 {
            panic!("invalid arity")
        }
        Scm::symbol("*UNSPECIFIED*")
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
            // (define (run-tests) ...)
            (/*NOP*/)
        }
    };
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn the_empty_list() {
        super::initialize();
        {
            // (let* ((x (quote ()))) (begin (assert (null? x))))
            {
                // (let ((x (quote ()))) (begin (begin (assert (null? x)))))
                {
                    let x_32 = Scm::Nil;
                    assert!({
                        // (null? x)
                        imports::null_p(&[x_32.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
    #[test]
    fn integers() {
        super::initialize();
        {
            // (let* ((x 1) (y (quote 1))) (begin (assert (= x y))))
            {
                // (let ((x 1)) (let ((y (quote 1))) (begin (begin (assert (= x y))))))
                {
                    let x_33 = Scm::from(1);
                    // (let ((y (quote 1))) (begin (begin (assert (= x y)))))
                    let y_0 = Scm::from(1);
                    assert!({
                        // (= x y)
                        imports::_e_(&[x_33.clone(), y_0.clone()])
                    }
                    .is_true());
                }
            }
        }
    }
}
