use crate::context::Context;
use crate::frontend::syntax_forms::Begin;

pub fn define_standard_libraries(ctx: &mut Context) {
    ctx.define_library("(scheme base)")
        .define_syntax("begin", Begin)
        .define_primitive("car", |stack, _storage| {
            let pair = stack.pop().unwrap();
            let x = pair.car().unwrap().clone();
            stack.push(x);
            Ok(())
        })
        .define_value("foo", |storage| {
            storage.ensure(1);
            storage.cons(1, 2).unwrap()
        });
}
