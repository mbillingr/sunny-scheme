#![allow(non_upper_case_globals)]

pub mod base;
pub mod file;
pub mod filesystem;
pub mod process_context;
pub mod read;
pub mod write;

pub mod sunny_helpers {
    use sunny_core::{self, Mut, Scm};

    fn assert_eq(args: &[Scm]) -> Scm {
        assert!(sunny_core::is_ptreq(args).is_true());
        Scm::True
    }

    fn assert_equal(args: &[Scm]) -> Scm {
        assert!(args[0] == args[1]);
        Scm::True
    }
}
