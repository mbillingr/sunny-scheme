use crate::context::Context;
use crate::frontend::syntax_forms::{
    Assignment, Begin, Branch, Definition, Import, Lambda, LibraryDefinition, Quotation,
    SyntaxDefinition,
};
use sunny_vm::{ErrorKind, Result, Value};

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
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::NotEnoughArgs),
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
                std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::NotEnoughArgs),
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
        Ok(Value::Number(x - 1))
    }

    varfn add([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::Number(0)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?,
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc += x;
        }

        Ok(Value::Number(acc))
    }

    varfn sub([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::Number(0)),
            [x] => return Ok(Value::Number(-x.as_number().ok_or(ErrorKind::TypeError)?)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?,
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc -= x;
        }

        Ok(Value::Number(acc))
    }

    varfn mul([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::Number(1)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?,
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc *= x;
        }

        Ok(Value::Number(acc))
    }

    varfn div([args]) -> Result<Value> {
        let mut acc = match args.as_slice() {
            [] => return Ok(Value::Number(0)),
            [x] => return Ok(Value::Number(1/x.as_number().ok_or(ErrorKind::TypeError)?)),
            [x, ..] => x.as_number().ok_or(ErrorKind::TypeError)?,
        };

        for x in &args[1..] {
            let x = x.as_number().ok_or(ErrorKind::TypeError)?;
            acc /= x;
        }

        Ok(Value::Number(acc))
    }

    fn lt(a: Value, b: Value) -> Result<Value> {
        Value::try_is_less(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn gt(a: Value, b: Value) -> Result<Value> {
        Value::try_is_greater(&a, &b).ok_or(ErrorKind::TypeError)
    }
}
