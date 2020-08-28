use sunny_core::{Mut, Scm, MEMORY_MODEL_KIND};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

macro_rules! pipe {
    ($f:tt) =>  { $f };
    ($f:tt $g:ident) => { |x| $g(&[$f(x)]) };
    ($f:tt $g:ident $($rest:ident)*) => { pipe![ (|x| $g(&[$f(x)])) $($rest)*] };
}

#[allow(non_upper_case_globals)]
mod scheme {
    pub mod base {
        use sunny_core::{self, car as _car, cdr as _cdr, Mut, Scm};

        thread_local! {pub static _e_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numeq))}
        thread_local! {pub static _g_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numgt))}
        thread_local! {pub static _l_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numlt))}
        thread_local! {pub static _minus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::sub))}
        thread_local! {pub static _plus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::add))}
        thread_local! {pub static car: Mut<Scm> = Mut::new(Scm::func(sunny_core::car))}
        thread_local! {pub static cdr: Mut<Scm> = Mut::new(Scm::func(sunny_core::cdr))}
        thread_local! {pub static caar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car]))}
        thread_local! {pub static cadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car]))}
        thread_local! {pub static cdar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr]))}
        thread_local! {pub static cddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr]))}
        thread_local! {pub static cons: Mut<Scm> = Mut::new(Scm::func(sunny_core::cons))}
        thread_local! {pub static eof_minus_object_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_eof))}
        thread_local! {pub static eq_p: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_ptreq))}
        thread_local! {pub static null_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_null))}
        thread_local! {pub static pair_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_pair))}
        thread_local! {pub static set_minus_car_i: Mut<Scm> = Mut::new(Scm::func(_set_car))}
        thread_local! {pub static set_minus_cdr_i: Mut<Scm> = Mut::new(Scm::func(_set_cdr))}
        thread_local! {pub static symbol_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_symbol))}
        thread_local! {pub static char_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_char))}
        thread_local! {pub static symbol_minus__g_string: Mut<Scm> = Mut::new(Scm::func1(_symbol_to_string))}
        thread_local! {pub static string_minus__g_list: Mut<Scm> = Mut::new(Scm::func1(_string_to_list))}
        thread_local! {pub static string_minus_append: Mut<Scm> = Mut::new(Scm::func2(_string_append))}
        thread_local! {pub static list_minus__g_string: Mut<Scm> = Mut::new(Scm::func1(_list_to_string))}

        fn _set_car(args: &[Scm]) -> Scm {
            match args {
                [Scm::Pair(p), x] => {
                    p.0.set(x.clone());
                    x.clone()
                }
                [_, _] => panic!("Not a pair: set-car! {:?}", args),
                _ => panic!("Incorrect arity: set-car! {:?}", args),
            }
        }

        fn _set_cdr(args: &[Scm]) -> Scm {
            match args {
                [Scm::Pair(p), x] => {
                    p.1.set(x.clone());
                    x.clone()
                }
                [_, _] => panic!("Not a pair: set-cdr! {:?}", args),
                _ => panic!("Incorrect arity: set-cdr! {:?}", args),
            }
        }

        fn _symbol_to_string(s: &Scm) -> Scm {
            Scm::str(s.as_symbol().unwrap().name())
        }

        fn _string_to_list(s: &Scm) -> Scm {
            let s = s.as_string().unwrap();

            let mut seq = Scm::nil();
            for ch in s.as_str().chars().rev() {
                seq = Scm::pair(ch, seq);
            }

            return seq;
        }

        fn _string_append(a: &Scm, b: &Scm) -> Scm {
            let a = a.as_string().unwrap();
            let b = b.as_string().unwrap();
            Scm::string(a.as_str().to_owned() + b.as_str())
        }

        fn _list_to_string(seq: &Scm) -> Scm {
            let mut seq = seq.clone();
            let mut s = String::new();
            while !seq.is_null() {
                s.push(seq.car().unwrap().as_char().unwrap());
                seq = seq.cdr().unwrap()
            }
            s.into()
        }
    }

    pub mod cxr {
        use sunny_core::{car as _car, cdr as _cdr, Mut, Scm};

        thread_local! {pub static caaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _car]))}
        thread_local! {pub static caadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _car]))}
        thread_local! {pub static cadar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _car]))}
        thread_local! {pub static caddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _car]))}
        thread_local! {pub static cdaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _cdr]))}
        thread_local! {pub static cdadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _cdr]))}
        thread_local! {pub static cddar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _cdr]))}
        thread_local! {pub static cdddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _cdr]))}

        thread_local! {pub static caaaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _car _car]))}
        thread_local! {pub static caaadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _car _car]))}
        thread_local! {pub static caadar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _car _car]))}
        thread_local! {pub static caaddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _car _car]))}
        thread_local! {pub static cadaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _cdr _car]))}
        thread_local! {pub static cadadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _cdr _car]))}
        thread_local! {pub static caddar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _cdr _car]))}
        thread_local! {pub static cadddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _cdr _car]))}
        thread_local! {pub static cdaaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _car _cdr]))}
        thread_local! {pub static cdaadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _car _cdr]))}
        thread_local! {pub static cdadar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _car _cdr]))}
        thread_local! {pub static cdaddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _car _cdr]))}
        thread_local! {pub static cddaar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car _cdr _cdr]))}
        thread_local! {pub static cddadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car _cdr _cdr]))}
        thread_local! {pub static cdddar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr _cdr _cdr]))}
        thread_local! {pub static cddddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr _cdr _cdr]))}
    }

    pub mod read {
        use std::io::stdin;
        use sunny_core::{self, Mut, Scm};
        use sunny_parse::from_reader;

        thread_local! {pub static read: Mut<Scm> = Mut::new(Scm::func(_read))}

        fn _read(_args: &[Scm]) -> Scm {
            match from_reader(stdin()) {
                Ok(x) => x,
                Err(e) if e.is_eof() => Scm::eof(),
                Err(e) => panic!("{:?}", e),
            }
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
