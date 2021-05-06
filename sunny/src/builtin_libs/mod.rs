#[macro_use]
mod macros;
mod hash_table;
use crate::context::Context;
use crate::frontend::syntax_forms::{
    Assignment, Begin, Branch, Definition, Import, Lambda, Let, LibraryDefinition, Quotation,
    SyntaxDefinition,
};
use hash_table::define_lib_sunny_hash_table;
use lazy_static::lazy_static;
use sexpr_generics::prelude::*;
use sexpr_generics::{lists, numbers};
use sunny_scm::Scm;
use sunny_vm::scm_extension::ScmExt;
use sunny_vm::{ErrorKind, Result, Vm};

/*  ===== R7RS compliant apply using our intrinsic function =====
(define (full-apply f x . args) (if (null? args) (apply f x) (apply f (cons x (build-list args)))))
(define (build-list lst) (if (null? (cdr lst)) (car lst) (cons (car lst) (build-list (cdr lst)))))
*/

pub fn define_standard_libraries(ctx: &mut Context) {
    ctx.define_library("(sunny core)")
        .define_syntax("begin", Begin)
        .define_syntax("define", Definition)
        .define_syntax("define-library", LibraryDefinition)
        .define_syntax("define-syntax", SyntaxDefinition)
        .define_syntax("if", Branch)
        .define_syntax("import", Import)
        .define_syntax("lambda", Lambda)
        .define_syntax("let", Let)
        .define_syntax("quote", Quotation)
        .define_syntax("set!", Assignment)
        .define_primitive_fixed_arity("append", 2, list_append)
        .define_intrinsic("call/cc", 1)
        .define_intrinsic("call-with-values", 2)
        .define_primitive_vararg("+", 0, add)
        .define_primitive_vararg("-", 0, sub)
        .define_primitive_vararg("*", 0, mul)
        .define_primitive_vararg("/", 0, div)
        .define_primitive_fixed_arity("<", 2, lt)
        .define_primitive_fixed_arity(">", 2, gt)
        .define_primitive_fixed_arity("<=", 2, le)
        .define_primitive_fixed_arity(">=", 2, ge)
        .define_primitive_vararg("=", 2, neq)
        .define_intrinsic("apply", 2)
        .define_primitive_fixed_arity("boolean?", 1, is_boolean)
        .define_primitive_fixed_arity("bytevector?", 1, is_bytevector)
        .define_primitive_vararg("bytevector", 0, bytevector)
        .define_primitive_fixed_arity("char?", 1, is_char)
        .define_intrinsic("cons", 2)
        .define_intrinsic("car", 1)
        .define_intrinsic("cdr", 1)
        .define_primitive_fixed_arity("display", 1, display)
        .define_primitive_fixed_arity("dec", 1, dec)
        .define_primitive_fixed_arity("eof-object?", 1, is_eof_object)
        .define_primitive_fixed_arity("eof-object", 0, eof_object)
        .define_intrinsic("eq?", 2)
        .define_primitive_fixed_arity("equal?", 2, is_equal)
        .define_primitive_fixed_arity("eqv?", 2, is_eqv)
        .define_primitive_fixed_arity("error", 1, error)
        .define_primitive_fixed_arity("memv", 2, memv)
        .define_primitive_fixed_arity("newline", 0, newline)
        .define_primitive_fixed_arity("null?", 1, is_null)
        .define_primitive_fixed_arity("number?", 1, is_number)
        .define_primitive_fixed_arity("pair?", 1, is_pair)
        .define_primitive_fixed_arity("procedure?", 1, is_procedure)
        .define_primitive_fixed_arity("procedure-arity", 1, proc_arity)
        .define_primitive_fixed_arity("reverse", 1, list_reverse)
        .define_primitive_fixed_arity("set-car!", 2, set_car)
        .define_primitive_fixed_arity("set-cdr!", 2, set_cdr)
        .define_primitive_fixed_arity("string?", 1, is_string)
        .define_primitive_vararg("string", 0, string)
        .define_primitive_fixed_arity("string->list", 1, string_to_list)
        .define_primitive_vararg("string-append", 0, string_append)
        .define_primitive_fixed_arity("string-length", 1, string_length)
        .define_primitive_fixed_arity("symbol?", 1, is_symbol)
        .define_primitive_vararg("values", 0, values)
        .define_primitive_fixed_arity("vector?", 1, is_vector)
        .define_primitive_vararg("vector", 0, vector)
        .define_value("foo", Scm::cons(1, 2))
        .build();

    ctx.define_library("(sunny extra)")
        .define_primitive_fixed_arity("now", 0, now)
        .build();

    define_lib_sunny_hash_table(ctx);
}

primitive! {
    fn dec(num: Scm) -> Result<Scm> {
        let x = num.as_int().ok_or(ErrorKind::TypeError("number", num))?;
        Ok(Scm::number(x - 1))
    }

    varfn add([args]) -> Result<Scm> {
        numbers::sum(args.iter()).map_err(|x|ErrorKind::TypeError("numbers", x.clone()))
    }

    varfn sub([args]) -> Result<Scm> {
        numbers::diff(args.iter()).map_err(|x|ErrorKind::TypeError("numbers", x.clone()))
    }

    varfn mul([args]) -> Result<Scm> {
        numbers::prod(args.iter()).map_err(|x|ErrorKind::TypeError("numbers", x.clone()))
    }

    varfn div([args]) -> Result<Scm> {
        numbers::quot(args.iter()).map_err(|x|ErrorKind::TypeError("numbers", x.clone()))
    }

    fn is_equal(a: Scm, b: Scm) -> Result<Scm> {
        Ok(Scm::bool(a.is_equal(&b)))
    }

    fn is_eqv(a: Scm, b: Scm) -> Result<Scm> {
        Ok(Scm::bool(a.is_eqv(&b)))
    }

    varfn neq([args]) -> Result<Scm> {
        let cmp = match args.as_slice() {
            [a, b, extra@..] => numbers::all_equal(a, b, extra.iter())
                                    .map_err(|x|ErrorKind::TypeError("numbers", x.clone())),
            _ => Err(ErrorKind::TooFewArgs),
        }?;
        Ok(Scm::bool(cmp))
    }

    varfn lt([args]) -> Result<Scm> {
        let cmp = match args.as_slice() {
            [a, b, extra@..] => numbers::all_strictly_increasing(a, b, extra.iter())
                                    .map_err(|x|ErrorKind::TypeError("numbers", x.clone())),
            _ => Err(ErrorKind::TooFewArgs),
        }?;
        Ok(Scm::bool(cmp))
    }

    varfn gt([args]) -> Result<Scm> {
        let cmp = match args.as_slice() {
            [a, b, extra@..] => numbers::all_strictly_decreasing(a, b, extra.iter())
                                    .map_err(|x|ErrorKind::TypeError("numbers", x.clone())),
            _ => Err(ErrorKind::TooFewArgs),
        }?;
        Ok(Scm::bool(cmp))
    }

    varfn le([args]) -> Result<Scm> {
        let cmp = match args.as_slice() {
            [a, b, extra@..] => numbers::all_increasing(a, b, extra.iter())
                                    .map_err(|x|ErrorKind::TypeError("numbers", x.clone())),
            _ => Err(ErrorKind::TooFewArgs),
        }?;
        Ok(Scm::bool(cmp))
    }

    varfn ge([args]) -> Result<Scm> {
        let cmp = match args.as_slice() {
            [a, b, extra@..] => numbers::all_decreasing(a, b, extra.iter())
                                    .map_err(|x|ErrorKind::TypeError("numbers", x.clone())),
            _ => Err(ErrorKind::TooFewArgs),
        }?;
        Ok(Scm::bool(cmp))
    }

    fn is_boolean(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_bool()))
    }

    fn is_bytevector(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_bytevector()))
    }

    varfn bytevector([args]) -> Result<Scm> {
        let mut byte_data = Vec::with_capacity(args.len());
        for x in args {
            let x = x.as_int().filter(|&x| x >= 0 && x <= 255).ok_or(ErrorKind::TypeError("byte-valued number", x))?;
            byte_data.push(x as u8);
        }
        Ok(Scm::bytevector(byte_data))
    }

    fn is_char(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_char()))
    }

    fn is_eof_object(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_eof()))
    }

    fn eof_object() -> Result<Scm> {
        Ok(Scm::eof())
    }

    fn is_null(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_null()))
    }

    fn is_number(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_number()))
    }

    fn is_pair(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_pair()))
    }

    fn is_procedure(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_procedure()))
    }

    fn is_string(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_str()))
    }

    varfn string([args]) -> Result<Scm> {
        let mut string_data = String::with_capacity(args.len());
        for x in args {
            let ch = x.to_char().ok_or_else(||ErrorKind::TypeError("character", x))?;
            string_data.push(ch);
        }
        Ok(Scm::string(string_data))
    }

    varfn string_append([args]) -> Result<Scm> {
        let mut string_data = String::with_capacity(args.len());
        for x in args {
            let s = x.to_str().ok_or_else(||ErrorKind::TypeError("string", x.clone()))?;
            string_data.push_str(s);
        }
        Ok(Scm::string(string_data))
    }

    fn string_length(obj: Scm) -> Result<Scm> {
        let string = obj.to_str().ok_or_else(||ErrorKind::TypeError("string", obj.clone()))?;
        Ok(Scm::number(string.len()))
    }

    fn string_to_list(obj: Scm) -> Result<Scm> {
        let string = obj.to_str().ok_or_else(||ErrorKind::TypeError("string", obj.clone()))?;
        let mut list = Scm::null();
        for ch in string.chars().rev() {
            list = Scm::cons(ch, list);
        }
        Ok(list)
    }

    fn is_symbol(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_symbol()))
    }

    fn is_vector(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_vector()))
    }

    varfn vector([args]) -> Result<Scm> {
        Ok(Scm::vector(args))
    }

    fn set_car(pair: Scm, car: Scm) -> Result<Scm> {
        pair.set_car(car).ok_or_else(||ErrorKind::TypeError("mutable pair", pair))?;
        Ok(Scm::void())
    }

    fn set_cdr(pair: Scm, cdr: Scm) -> Result<Scm> {
        pair.set_cdr(cdr).ok_or_else(||ErrorKind::TypeError("mutable pair", pair))?;
        Ok(Scm::void())
    }

    fn now() -> Result<Scm> {
        let begin = *BEGINNING_OF_TIME;
        let duration = std::time::Instant::now() - begin;
        let micros = duration.as_micros() as i64;
        Ok(micros.into())
    }

    fn display(x: Scm) -> Result<Scm> {
        print!("{}", x);
        Ok(Scm::void())
    }

    fn newline() -> Result<Scm> {
        println!();
        Ok(Scm::void())
    }

    fn error(msg: Scm) -> Result<Scm> {
        Err(ErrorKind::Generic(msg))
    }

    fn list_reverse(list: Scm) -> Result<Scm> {
        Ok(lists::reverse(&list))
    }

    fn list_append(left: Scm, right: Scm) -> Result<Scm> {
        Ok(lists::append(&left, &right))
    }

    fn memv(obj: Scm, list: Scm) -> Result<Scm> {
        match lists::memv(&list, &obj) {
            Some(m) => Ok(m.clone()),
            None => Ok(Scm::bool(false))
        }
    }
}

fn values(n_args: usize, vm: &mut Vm) -> Result<()> {
    vm.push_value(Scm::values(n_args));
    Ok(())
}

fn proc_arity(n_args: usize, vm: &mut Vm) -> Result<()> {
    match n_args {
        0 => return Err(ErrorKind::TooFewArgs),
        1 => {}
        _ => return Err(ErrorKind::TooManyArgs),
    }
    let proc = vm.pop_value()?;
    let (min, max) = if let Some(pri) = proc.as_primitive() {
        (pri.min_arity, pri.max_arity)
    } else if let Some(cls) = proc.as_closure() {
        cls.arity()
    } else if let Some(cnt) = proc.as_continuation() {
        cnt.arity()
    } else {
        return Err(ErrorKind::TypeError("callable", proc));
    };
    let max = max.map(Scm::from).unwrap_or_else(|| Scm::bool(false));
    let arity = Scm::cons(min, max);
    vm.push_value(arity);
    Ok(())
}

lazy_static! {
    static ref BEGINNING_OF_TIME: std::time::Instant = std::time::Instant::now();
}
