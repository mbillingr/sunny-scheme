use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[allow(non_upper_case_globals)]
mod scheme {
    pub mod base {
        use sunny_core::{self, Mut, Scm};

        thread_local! {pub static eq_p: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_ptreq))}
        thread_local! {pub static _e_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numeq))}
        thread_local! {pub static _minus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::sub))}
        thread_local! {pub static _plus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::add))}
    }

    pub mod write {
        use sunny_core::{self, Mut, Scm};

        thread_local! {pub static display: Mut<Scm> = Mut::new(Scm::func(_display))}
        thread_local! {pub static newline: Mut<Scm> = Mut::new(Scm::func(_newline))}

        fn _display(args: &[Scm]) -> Scm {
            print!("{:?}", args[0]);
            Scm::Nil
        }

        fn _newline(_: &[Scm]) -> Scm {
            println!();
            Scm::Nil
        }
    }

    pub mod sunny_helpers {
        use sunny_core::{self, Mut, Scm};

        thread_local! {pub static assert_minus_eq: Mut<Scm> = Mut::new(Scm::func(_assert_eq))}

        fn _assert_eq(args: &[Scm]) -> Scm {
            assert!(sunny_core::is_ptreq(args).is_true());
            Scm::True
        }
    }
}
