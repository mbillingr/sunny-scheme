use crate::context::Context;
use crate::frontend::syntax_forms::Begin;
use sunny_vm::{ErrorKind, Result, Value, ValueStorage};

pub fn define_standard_libraries(ctx: &mut Context) {
    ctx.define_library("(scheme base)")
        .define_syntax("begin", Begin)
        .define_primitive("car", car)
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
    fn car(pair: Value) -> Result<Value> {
        let x = pair.car().cloned().ok_or(ErrorKind::TypeError)?;
        Ok(x)
    }
}
