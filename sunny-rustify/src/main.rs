use sunny_core::Scm;
use std::cell::Cell;

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[allow(non_upper_case_globals)]
mod scheme {
    pub mod write {
        use std::cell::Cell;
        use sunny_core::{self, Scm};

        thread_local! {pub static display: Cell<Scm> = Cell::new(Scm::func(_display))}
        thread_local! {pub static newline: Cell<Scm> = Cell::new(Scm::func(_newline))}
        thread_local! {pub static _plus_: Cell<Scm> = Cell::new(Scm::func(sunny_core::add))}

        fn _display(args: &[Scm]) -> Scm {
            print!("{:?}", args[0]);
            Scm::Nil
        }

        fn _newline(_: &[Scm]) -> Scm {
            println!();
            Scm::Nil
        }
    }
}
