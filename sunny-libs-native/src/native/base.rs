pub fn initialize() {}
pub mod exports {
    use sunny_core::{self, car as _car, cdr as _cdr, Mut, Scm};
    thread_local! {pub static _e_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numeq))}
    thread_local! {pub static _g_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numgt))}
    thread_local! {pub static _l_: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_numlt))}
    thread_local! {pub static _minus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::sub))}
    thread_local! {pub static _plus_: Mut<Scm> = Mut::new(Scm::func(sunny_core::add))}
    thread_local! {pub static apply: Mut<Scm> = Mut::new(Scm::func(_apply))}
    thread_local! {pub static car: Mut<Scm> = Mut::new(Scm::func(sunny_core::car))}
    thread_local! {pub static cdr: Mut<Scm> = Mut::new(Scm::func(sunny_core::cdr))}
    thread_local! {pub static caar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _car]))}
    thread_local! {pub static cadr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _car]))}
    thread_local! {pub static cdar: Mut<Scm> = Mut::new(Scm::func(pipe![_car _cdr]))}
    thread_local! {pub static cddr: Mut<Scm> = Mut::new(Scm::func(pipe![_cdr _cdr]))}
    thread_local! {pub static cons: Mut<Scm> = Mut::new(Scm::func(sunny_core::cons))}
    thread_local! {pub static eof_minus_object_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_eof))}
    thread_local! {pub static eq_p: Mut<Scm> = Mut::new(Scm::func(sunny_core::is_ptreq))}
    thread_local! {pub static equal_p: Mut<Scm> = Mut::new(Scm::func2(Scm::eq))}
    thread_local! {pub static error: Mut<Scm> = Mut::new(Scm::func(_error))}
    thread_local! {pub static null_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_null))}
    thread_local! {pub static pair_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_pair))}
    thread_local! {pub static set_minus_car_i: Mut<Scm> = Mut::new(Scm::func(_set_car))}
    thread_local! {pub static set_minus_cdr_i: Mut<Scm> = Mut::new(Scm::func(_set_cdr))}
    thread_local! {pub static symbol_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_symbol))}
    thread_local! {pub static char_p: Mut<Scm> = Mut::new(Scm::func1(Scm::is_char))}
    thread_local! {pub static symbol_minus__g_string: Mut<Scm> = Mut::new(Scm::func1(_symbol_to_string))}
    thread_local! {pub static string_minus__g_list: Mut<Scm> = Mut::new(Scm::func1(_string_to_list))}
    thread_local! {pub static string_minus_cons: Mut<Scm> = Mut::new(Scm::func2(_string_cons))}
    thread_local! {pub static string_l__p: Mut<Scm> = Mut::new(Scm::func2(_string_cmp))}
    thread_local! {pub static list_minus__g_string: Mut<Scm> = Mut::new(Scm::func1(_list_to_string))}
    thread_local! {pub static close_minus_port: Mut<Scm> = Mut::new(Scm::func1(Scm::close_port))}

    fn _error(args: &[Scm]) -> Scm {
        panic!("{}, {:?}", args[0], &args[1..])
    }

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

    fn _string_cons(a: &Scm, b: &Scm) -> Scm {
        let a = a.as_string().unwrap();
        let b = b.as_string().unwrap();
        Scm::string(a.as_str().to_owned() + b.as_str())
    }

    fn _string_cmp(a: &Scm, b: &Scm) -> Scm {
        let a = a.as_string().unwrap();
        let b = b.as_string().unwrap();
        Scm::bool(a.as_str() < b.as_str())
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

    fn _apply(args: &[Scm]) -> Scm {
        match args {
            [proc, args @ .., listargs] => {
                let mut args = args.to_vec();
                let mut listargs = listargs.clone();
                while let Some((a, d)) = listargs.as_pair() {
                    args.push(a);
                    listargs = d;
                }
                proc.invoke(&args)
            }
            _ => panic!("Incorrect arity: apply {:?}", args),
        }
    }
}
