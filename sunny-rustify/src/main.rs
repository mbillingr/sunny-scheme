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

    pub mod cxr {
        use sunny_core::{Mut, Scm};

        thread_local! {pub static caaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().car().unwrap()))}
        thread_local! {pub static caadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().car().unwrap()))}
        thread_local! {pub static cadar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().car().unwrap()))}
        thread_local! {pub static caddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().car().unwrap()))}
        thread_local! {pub static cdaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().cdr().unwrap()))}
        thread_local! {pub static cdadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().cdr().unwrap()))}
        thread_local! {pub static cddar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().cdr().unwrap()))}
        thread_local! {pub static cdddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().cdr().unwrap()))}

        thread_local! {pub static caaaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().caar().unwrap()))}
        thread_local! {pub static caaadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().caar().unwrap()))}
        thread_local! {pub static caadar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().caar().unwrap()))}
        thread_local! {pub static caaddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().caar().unwrap()))}
        thread_local! {pub static cadaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().cdar().unwrap()))}
        thread_local! {pub static cadadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().cdar().unwrap()))}
        thread_local! {pub static caddar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().cdar().unwrap()))}
        thread_local! {pub static cadddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().cdar().unwrap()))}
        thread_local! {pub static cdaaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().cadr().unwrap()))}
        thread_local! {pub static cdaadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().cadr().unwrap()))}
        thread_local! {pub static cdadar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().cadr().unwrap()))}
        thread_local! {pub static cdaddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().cadr().unwrap()))}
        thread_local! {pub static cddaar: Mut<Scm> = Mut::new(Scm::func1(|x| x.caar().unwrap().cddr().unwrap()))}
        thread_local! {pub static cddadr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cdar().unwrap().cddr().unwrap()))}
        thread_local! {pub static cdddar: Mut<Scm> = Mut::new(Scm::func1(|x| x.cadr().unwrap().cddr().unwrap()))}
        thread_local! {pub static cddddr: Mut<Scm> = Mut::new(Scm::func1(|x| x.cddr().unwrap().cddr().unwrap()))}
    }

    pub mod read {
        use sunny_core::{self, Mut, Scm};
        use sunny_parse::from_reader;
        use std::io::{stdin, Read};

        thread_local! {pub static read: Mut<Scm> = Mut::new(Scm::func(_read))}

        fn _read(args: &[Scm]) -> Scm {
            from_reader(stdin())
        }
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
