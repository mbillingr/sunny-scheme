use sunny_sexpr_parser::Sexpr;
use sunny_vm::bytecode::Op;
use sunny_vm::{Value, ValueStorage};

pub trait Backend {
    type Output;

    fn constant(&mut self, c: &Sexpr) -> Self::Output;

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output;
}

struct ByteCodeBackend<'s> {
    storage: &'s mut ValueStorage,
    constants: Vec<Value>,
}

impl<'s> ByteCodeBackend<'s> {
    pub fn new(storage: &'s mut ValueStorage) -> Self {
        ByteCodeBackend {
            storage,
            constants: vec![],
        }
    }
}

impl Backend for ByteCodeBackend<'_> {
    type Output = Vec<Op>; // todo: use code segments or basic blocks here...

    fn constant(&mut self, c: &Sexpr) -> Self::Output {
        self.storage.ensure(1);
        let value = self.storage.sexpr_to_value(c).unwrap();
        let idx = self.constants.len();
        self.constants.push(value);
        vec![Op::Const(idx as u8)]
    }

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output {
        let mut code = first;
        code.extend(second);
        code.push(Op::Cons);
        code
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_bytecode_constant() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let code = bcb.constant(&Sexpr::nil());
        assert_eq!(code, vec![Op::Const(0)])
    }

    #[test]
    fn build_bytecode_cons() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(&Sexpr::nil());
        let b = bcb.constant(&Sexpr::nil());
        let c = bcb.cons(a, b);
        assert_eq!(c, vec![Op::Const(0), Op::Const(1), Op::Cons])
    }

    #[test]
    fn build_bytecode_multiple_cons() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(&Sexpr::nil());
        let b = bcb.constant(&Sexpr::nil());
        let c = bcb.constant(&Sexpr::nil());
        let d = bcb.cons(b, a);
        let e = bcb.cons(c, d);
        assert_eq!(
            e,
            vec![Op::Const(2), Op::Const(1), Op::Const(0), Op::Cons, Op::Cons]
        )
    }
}
