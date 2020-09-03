#![allow(non_upper_case_globals)]

pub mod base;
pub mod file;
pub mod filesystem;
pub mod process_context;
pub mod read;
pub mod write;

pub mod sunny_helpers {
    use sunny_core::{self, Mut, Scm};

    thread_local! {pub static assert_minus_eq: Mut<Scm> = Mut::new(Scm::func(_assert_eq))}
    thread_local! {pub static assert_minus_equal: Mut<Scm> = Mut::new(Scm::func(_assert_equal))}

    fn _assert_eq(args: &[Scm]) -> Scm {
        assert!(sunny_core::is_ptreq(args).is_true());
        Scm::True
    }

    fn _assert_equal(args: &[Scm]) -> Scm {
        assert!(args[0] == args[1]);
        Scm::True
    }
}
