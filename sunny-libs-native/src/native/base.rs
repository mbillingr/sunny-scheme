pub fn initialize() {}
pub mod exports {
    use sunny_core::{self, Mut, Scm};

    pub use sunny_core::{
        is_numeq as _e_,
        is_numgt as _g_,
        is_numlt as _l_,
        sub as _minus_,
        add as _plus_,
        car, cdr, cons,
        is_ptreq as eq_p,
    };

    pipe_fn!{caar(&[Scm]) -> Scm = car car}
    pipe_fn!{cadr(&[Scm]) -> Scm = cdr car}
    pipe_fn!{cdar(&[Scm]) -> Scm = car cdr}
    pipe_fn!{cddr(&[Scm]) -> Scm = cdr cdr}

    wrap_fn1!{"eof-object?", eof_minus_object_p = Scm::is_eof}
    wrap_fn2!{"equal?", equal_p = Scm::eq}
    wrap_fn1!{"null?", null_p = Scm::is_null}
    wrap_fn1!{"pair?", pair_p = Scm::is_pair}
    wrap_fn1!{"procedure?", procedure_p = Scm::is_procedure}
    wrap_fn1!{"string?", string_p = Scm::is_string}
    wrap_fn1!{"symbol?", symbol_p = Scm::is_symbol}
    wrap_fn1!{"char?", char_p = Scm::is_char}

    wrap_fn1!{"close-port", close_minus_port = Scm::close_port}

    pub fn error(args: &[Scm]) -> Scm {
        panic!("{}, {:?}", args[0], &args[1..])
    }

    pub fn set_minus_car_i(args: &[Scm]) -> Scm {
        match args {
            [Scm::Pair(p), x] => {
                p.0.set(x.clone());
                x.clone()
            }
            [_, _] => panic!("Not a pair: set-car! {:?}", args),
            _ => panic!("Incorrect arity: set-car! {:?}", args),
        }
    }

    pub fn set_minus_cdr_i(args: &[Scm]) -> Scm {
        match args {
            [Scm::Pair(p), x] => {
                p.1.set(x.clone());
                x.clone()
            }
            [_, _] => panic!("Not a pair: set-cdr! {:?}", args),
            _ => panic!("Incorrect arity: set-cdr! {:?}", args),
        }
    }

    wrap_fn!{"symbol->string",
        symbol_minus__g_string(s) {
            Scm::str(s.as_symbol().unwrap().name())
        }
    }

    wrap_fn!{"string->list",
        string_minus__g_list(s) {
            let s = s.as_string().unwrap();

            let mut seq = Scm::nil();
            for ch in s.as_str().chars().rev() {
                seq = Scm::pair(ch, seq);
            }

            return seq;
        }
    }

    wrap_fn!{"string-cons",
        string_minus_cons(a, b) {
            let a = a.as_string().unwrap();
            let b = b.as_string().unwrap();
            Scm::string(a.as_str().to_owned() + b.as_str())
        }
    }

    wrap_fn!{"string<?",
        string_l__p(a, b) {
            let a = a.as_string().unwrap();
            let b = b.as_string().unwrap();
            Scm::bool(a.as_str() < b.as_str())
        }
    }

    wrap_fn!{"list->string",
        list_minus__g_string(seq) {
            let mut seq = seq.clone();
            let mut s = String::new();
            while !seq.is_null() {
                s.push(seq.car().unwrap().as_char().unwrap());
                seq = seq.cdr().unwrap()
            }
            s.into()
        }
    }

    pub fn apply(args: &[Scm]) -> Scm {
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
