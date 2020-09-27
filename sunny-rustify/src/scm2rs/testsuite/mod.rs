#[allow(unused_imports)]
use sunny_core::{Mut, Scm};
mod imports {
    pub use crate::scheme::base::exports::*;
}

pub mod exports {
    pub use super::globals::run_minus_tests;
}

mod globals {
    use sunny_core::{Mut, Scm};
    thread_local! {#[allow(non_upper_case_globals)] pub static run_minus_tests: Mut<Scm> = Mut::new(Scm::symbol("UNINITIALIZED GLOBAL run-tests"))}
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
        // (define (run-tests) (testsuite "Scheme Tests" (testcase "the empty list" (given (x <- (quote ()))) (then (null? x))) (testcase "integers" (given (x <- 1) (y <- (quote 1))) (then (= x y)))))
        globals::run_minus_tests.with(|value| {
            value.set({
                Scm::func(move |args: &[Scm]| {
                    if args.len() != 0 {
                        panic!("invalid arity")
                    }
                    // (letrec () (testsuite "Scheme Tests" (testcase "the empty list" (given (x <- (quote ()))) (then (null? x))) (testcase "integers" (given (x <- 1) (y <- (quote 1))) (then (= x y)))))
                    {
                        Scm::symbol("*UNSPECIFIED*")
                    }
                })
            })
        })
    };
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn the_empty_list() {
        super::initialize();

        // (let* ((x (quote ()))) (begin (assert (null? x))))
        {
            let [x] = [Scm::Nil];
            // (letrec () (let* () (begin (assert (null? x)))))
            {
                // (let* () (begin (assert (null? x))))
                assert!(
                    // (null? x)
                    imports::null_p
                        .with(|value| value.get())
                        .invoke(&[x.clone(),])
                        .is_true()
                );
            }
        }
    }
    #[test]
    fn integers() {
        super::initialize();

        // (let* ((x 1) (y (quote 1))) (begin (assert (= x y))))
        {
            let [x] = [Scm::from(1)];
            // (letrec () (let* ((y (quote 1))) (begin (assert (= x y)))))
            {
                // (let* ((y (quote 1))) (begin (assert (= x y))))
                {
                    let [y] = [Scm::from(1)];
                    // (letrec () (let* () (begin (assert (= x y)))))
                    {
                        // (let* () (begin (assert (= x y))))
                        assert!(
                            // (= x y)
                            imports::_e_
                                .with(|value| value.get())
                                .invoke(&[x.clone(), y.clone(),])
                                .is_true()
                        );
                    }
                }
            }
        }
    }
}
