use std::cell::Cell;
use sunny_core::{Scm, Mut};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[allow(non_upper_case_globals)]
mod scheme {
    pub mod write {
        use std::cell::Cell;
        use sunny_core::{self, Scm, Mut};

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
        use std::cell::Cell;
        use sunny_core::{self, Scm, Mut};

        thread_local! {pub static _e_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numeq))}
        thread_local! {pub static _minus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::sub))}
        thread_local! {pub static _plus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::add))}
    }
}
