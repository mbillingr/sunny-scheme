use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[allow(non_upper_case_globals)]
mod scheme {
    pub mod base {
        use sunny_core::{self, Mut, Scm};

        thread_local! {pub static _e_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numeq))}
        thread_local! {pub static _g_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numgt))}
        thread_local! {pub static _l_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numlt))}
        thread_local! {pub static _minus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::sub))}
        thread_local! {pub static _plus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::add))}
        thread_local! {pub static car: Mut<Scm> = Mut::new(Scm::func(sunny_core::car))}
        thread_local! {pub static cdr: Mut<Scm> = Mut::new(Scm::func(sunny_core::cdr))}
        thread_local! {pub static cons: Mut<Scm> = Mut::new(Scm::func(sunny_core::cons))}
        thread_local! {pub static eq_p: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_ptreq))}
        thread_local! {pub static null_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_null))}
        thread_local! {pub static pair_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_pair))}
    }

    pub mod write {
        use sunny_core::{self, Mut, Scm};

        thread_local! {pub static display: Mut<Scm> = Mut::new(Scm::func(_display))}
        thread_local! {pub static newline: Mut<Scm> = Mut::new(Scm::func(_newline))}
        thread_local! {pub static write: Mut<Scm> = Mut::new(Scm::func(_write))}

        fn _display(args: &[Scm]) -> Scm {
            print!("{}", args[0]);
            Scm::Nil
        }

        fn _newline(_: &[Scm]) -> Scm {
            println!();
            Scm::Nil
        }

        fn _write(args: &[Scm]) -> Scm {
            print!("{:?}", args[0]);
            Scm::Nil
        }
    }

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
}
