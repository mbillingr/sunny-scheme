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
        .define_intrinsic("cons", 2)
        .define_intrinsic("car", 1)
        .define_intrinsic("cdr", 1)
        .define_primitive("dec", dec)
        .define_value("foo", |storage| {
            storage.ensure(1);
            storage.cons(1, 2).unwrap()
        })
        .build();
}

macro_rules! primitive {
    ($(fn $name:ident($a:ident: Value) -> Result<Value> $body:block)*) => {
        $(
            fn $name(_stack: &mut Vec<Value>, _storage: &mut ValueStorage) -> Result<()> {
                let $a = _stack.pop().unwrap();
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
}
