use crate::context::Context;
use crate::frontend::syntax_forms::{
    Assignment, Begin, Branch, Definition, Import, Lambda, LibraryDefinition, Quotation,
    SyntaxDefinition,
};
use sunny_vm::{ErrorKind, Result, Value};

/*  ===== R7RS compliant apply using our intrinsic function =====
(define (full-apply f x . args) (if (null? args) (apply f x) (apply f (cons x (build-list args)))))
(define (build-list lst) (if (null? (cdr lst)) (car lst) (cons (car lst) (build-list (cdr lst)))))
*/

pub fn define_standard_libraries(ctx: &mut Context) {
    ctx.define_library("(scheme base)")
        .define_syntax("begin", Begin)
        .define_syntax("define", Definition)
        .define_syntax("define-library", LibraryDefinition)
        .define_syntax("define-syntax", SyntaxDefinition)
        .define_syntax("if", Branch)
        .define_syntax("import", Import)
        .define_syntax("lambda", Lambda)
        .define_syntax("quote", Quotation)
        .define_syntax("set!", Assignment)
        .define_intrinsic("call/cc", 1)
        .define_primitive("+", add)
        .define_primitive("-", sub)
        .define_primitive("*", mul)
        .define_primitive("/", div)
        .define_primitive("<", lt)
        .define_primitive(">", gt)
        .define_primitive("null?", is_null)
        .define_intrinsic("apply", 2)
        .define_intrinsic("cons", 2)
        .define_intrinsic("car", 1)
        .define_intrinsic("cdr", 1)
        .define_intrinsic("eq?", 2)
        .define_primitive("dec", dec)
        .define_value("foo", |storage| {
            storage.ensure(1);
            storage.cons(1, 2).unwrap()
        })
        .build();
}

macro_rules! primitive {
    ($($kind:tt $name:ident($($args:tt)*) -> Result<Value> $body:block)*) => {
        $(
            primitive!{def $kind $name($($args)*) -> Result<Value> $body}
        )*
    };

    (def fn $name:ident($($a:ident: Value),*) -> Result<Value> $body:block) => {
        fn $name(n_args: usize, vm: &mut sunny_vm::Vm) -> Result<()> {
            let n_expect = 0;
            $(let $a; let n_expect = n_expect + 1;)*

            match n_args.cmp(&n_expect) {
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::TooFewArgs),
                std::cmp::Ordering::Greater => return Err(sunny_vm::ErrorKind::TooManyArgs),
                std::cmp::Ordering::Equal => {}
            }

            $($a = vm.pop_value().unwrap();)*
            // wrap body in closure, so `return`ing from the body works as expected
            let ret = (||$body)();
            vm.push_value(ret?);
            Ok(())
        }
    };

    (def varfn $name:ident($($a:ident: Value,)* [$vararg:ident]) -> Result<Value> $body:block) => {
        fn $name(n_args: usize, vm: &mut sunny_vm::Vm) -> Result<()> {
            let n_expect = 0;
            $(let $a; let n_expect = n_expect + 1;)*

            match n_args.cmp(&n_expect) {
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::TooFewArgs),
                _ => {}
            }

            $($a = vm.pop_value().unwrap();)*

            let n_varargs = n_args - n_expect;
            let mut $vararg = vec![];
            for _ in 0..n_varargs {
                $vararg.push(vm.pop_value().unwrap());
            }

            // wrap body in closure, so `return`ing from the body works as expected
            let ret = (||$body)();
            vm.push_value(ret?);
            Ok(())
        }
    };
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

    fn is_null(obj: Value) -> Result<Value> {
        Ok(Value::bool(obj.is_nil()))
    }
}
