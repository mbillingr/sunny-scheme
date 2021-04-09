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
use sunny_vm::{ErrorKind, Result, Value, Vm};

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
        .define_primitive_fixed_arity("newline", 0, newline)
        .define_primitive_vararg("values", 0, values)
        .define_value("foo", |storage| storage.cons(1, 2))
        .build();

    ctx.define_library("(sunny extra)")
        .define_primitive_fixed_arity("now", 0, now)
        .build();

    define_lib_sunny_hash_table(ctx);
}

primitive! {
    fn dec(num: Value) -> Result<Value> {
        let x = num.as_int().ok_or(ErrorKind::TypeError)?;
        Ok(Value::number(x - 1))
    }

    varfn add([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::number(0)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc = &acc + x;
        }

        Ok(Value::number(acc))
    }

    varfn sub([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::number(0)),
            [x] => return Ok(Value::number(-x.as_number().ok_or(ErrorKind::TypeError)?)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc = &acc - x;
        }

        Ok(Value::number(acc))
    }

    varfn mul([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::number(1)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc = &acc * x;
        }

        Ok(Value::number(acc))
    }

    varfn div([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::number(0)),
            [x] => return Ok(Value::number(x.as_number().ok_or(ErrorKind::TypeError)?.inv())),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?.clone(),
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc = &acc / x;
        }

        Ok(Value::number(acc))
    }

    fn lt(a: Value, b: Value) -> Result<Value> {
        Value::try_is_less(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn gt(a: Value, b: Value) -> Result<Value> {
        Value::try_is_greater(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn le(a: Value, b: Value) -> Result<Value> {
        Value::try_is_less_or_equal(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn ge(a: Value, b: Value) -> Result<Value> {
        Value::try_is_greater_or_equal(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn ne(a: Value, b: Value) -> Result<Value> {
        Value::try_is_numeq(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn is_null(obj: Value) -> Result<Value> {
        Ok(Value::bool(obj.is_nil()))
    }

    fn now() -> Result<Value> {
        let begin = *BEGINNING_OF_TIME;
        let duration = std::time::Instant::now() - begin;
        let micros = duration.as_micros() as i64;
        Ok(micros.into())
    }

    fn display(x: Value) -> Result<Value> {
        print!("{}", x);
        Ok(Value::Void)
    }

    fn newline() -> Result<Value> {
        println!();
        Ok(Value::Void)
    }
}

fn values(n_args: usize, vm: &mut Vm) -> Result<()> {
    vm.push_value(Value::Values(n_args));
    Ok(())
}

lazy_static! {
    static ref BEGINNING_OF_TIME: std::time::Instant = std::time::Instant::now();
}
