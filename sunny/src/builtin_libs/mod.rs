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
use sunny_sexpr_parser::Scm;
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
        .define_primitive_fixed_arity("=", 2, ne)
        .define_primitive_fixed_arity("null?", 1, is_null)
        .define_intrinsic("apply", 2)
        .define_intrinsic("cons", 2)
        .define_intrinsic("car", 1)
        .define_intrinsic("cdr", 1)
        .define_primitive_fixed_arity("display", 1, display)
        .define_primitive_fixed_arity("dec", 1, dec)
        .define_intrinsic("eq?", 2)
        .define_primitive_fixed_arity("error", 1, error)
        .define_primitive_fixed_arity("newline", 0, newline)
        .define_primitive_fixed_arity("procedure-arity", 1, proc_arity)
        .define_primitive_vararg("values", 0, values)
        .define_value("foo", |_| Scm::cons(1, 2))
        .build();

    ctx.define_library("(sunny extra)")
        .define_primitive_fixed_arity("now", 0, now)
        .build();

    define_lib_sunny_hash_table(ctx);
}

primitive! {
    fn dec(num: Scm) -> Result<Scm> {
        let x = num.as_int().ok_or(ErrorKind::TypeError)?;
        Ok(Scm::number(x - 1))
    }

    varfn add([args]) -> Result<Scm> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Scm::number(0)),
            [x, ..] => x.as_int().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_int().ok_or(ErrorKind::TypeError)?;
            acc = &acc + x;
        }

        Ok(Scm::number(acc))
    }

    varfn sub([args]) -> Result<Scm> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Scm::number(0)),
            [x] => return Ok(Scm::number(-x.as_int().ok_or(ErrorKind::TypeError)?)),
            [x, ..] => x.as_int().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_int().ok_or(ErrorKind::TypeError)?;
            acc = &acc - x;
        }

        Ok(Scm::number(acc))
    }

    varfn mul([args]) -> Result<Scm> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Scm::number(1)),
            [x, ..] => x.as_int().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_int().ok_or(ErrorKind::TypeError)?;
            acc = &acc * x;
        }

        Ok(Scm::number(acc))
    }

    varfn div([args]) -> Result<Scm> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Scm::number(0)),
            [x] => return Ok(Scm::number(x.as_int().map(|i|1/i).ok_or(ErrorKind::TypeError)?)),
            [x, ..] => x.as_int().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_int().ok_or(ErrorKind::TypeError)?;
            acc = &acc / x;
        }

        Ok(Scm::number(acc))
    }

    fn lt(a: Scm, b: Scm) -> Result<Scm> {
        Scm::try_is_less(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn gt(a: Scm, b: Scm) -> Result<Scm> {
        Scm::try_is_greater(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn le(a: Scm, b: Scm) -> Result<Scm> {
        Scm::try_is_less_or_equal(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn ge(a: Scm, b: Scm) -> Result<Scm> {
        Scm::try_is_greater_or_equal(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn ne(a: Scm, b: Scm) -> Result<Scm> {
        Scm::try_is_numeq(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn is_null(obj: Scm) -> Result<Scm> {
        Ok(Scm::bool(obj.is_null()))
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
        return Err(ErrorKind::TypeError);
    };
    let max = max.map(Scm::from).unwrap_or_else(|| Scm::bool(false));
    let arity = Scm::cons(min, max);
    vm.push_value(arity);
    Ok(())
}

lazy_static! {
    static ref BEGINNING_OF_TIME: std::time::Instant = std::time::Instant::now();
}
