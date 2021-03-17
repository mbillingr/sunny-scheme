use crate::context::Context;
use crate::frontend::syntax_forms::{
    Assignment, Begin, Branch, Definition, Import, Lambda, LibraryDefinition, Quotation,
    SyntaxDefinition,
};
use sunny_vm::{ErrorKind, Result, Value, ValueStorage};

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
        .define_primitive("+", add)
        .define_primitive("-", sub)
        .define_primitive("*", mul)
        .define_primitive("/", div)
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
    ($(fn $name:ident($($a:ident: Value),*) -> Result<Value> $body:block)*) => {
        $(
            fn $name(n_args: usize, _stack: &mut Vec<Value>, _storage: &mut ValueStorage) -> Result<()> {
                let n_expect = 0;
                $(let $a; let n_expect = n_expect + 1;)*

                match n_args.cmp(&n_expect) {
                    std::cmp::Ordering::Less => return Err(sunny_vm::ErrorKind::NotEnoughArgs),
                    std::cmp::Ordering::Greater => return Err(sunny_vm::ErrorKind::TooManyArgs),
                    std::cmp::Ordering::Equal => {}
                }

                $($a = _stack.pop().unwrap();)*
                let ret = $body;
                _stack.push(ret?);
                Ok(())
            }
        )*
    }
}

primitive! {
    fn dec(num: Value) -> Result<Value> {
        let x = num.as_int().ok_or(ErrorKind::TypeError)?;
        Ok(Value::Number(x - 1))
    }

    fn add(a: Value, b: Value) -> Result<Value> {
        Value::try_add(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn sub(a: Value, b: Value) -> Result<Value> {
        Value::try_sub(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn mul(a: Value, b: Value) -> Result<Value> {
        Value::try_mul(&a, &b).ok_or(ErrorKind::TypeError)
    }

    fn div(a: Value, b: Value) -> Result<Value> {
        Value::try_div(&a, &b).ok_or(ErrorKind::TypeError)
    }
}
