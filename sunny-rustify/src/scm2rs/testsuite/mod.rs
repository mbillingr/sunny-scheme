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
                            let x = Scm::Nil;
                            assert!({
                                // (null? x)
                                Scm::func(imports::null_p).invoke(&[x.clone()])
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
                            let x = Scm::from(1);
                            // (let ((y (quote 1))) (begin (begin (assert (= x y)))))
                            let y = Scm::from(1);
                            assert!({
                                // (= x y)
                                Scm::func(imports::_e_).invoke(&[x.clone(), y.clone()])
                            }
                            .is_true());
                        }
                    }
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
            // (define (run-tests) ...)
            (/*NOP*/)
        }
    };
}
