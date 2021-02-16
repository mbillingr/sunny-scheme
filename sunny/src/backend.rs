use sunny_sexpr_parser::Sexpr;
use sunny_vm::bytecode::Op;
use sunny_vm::{BasicBlock, BlockChain, Value, ValueStorage};

pub trait Backend {
    type Output;

    fn constant(&mut self, c: &Sexpr) -> Self::Output;

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output;
}

struct ByteCodeBackend<'s> {
    storage: &'s mut ValueStorage,
}

impl<'s> ByteCodeBackend<'s> {
    pub fn new(storage: &'s mut ValueStorage) -> Self {
        ByteCodeBackend { storage }
    }
}

impl Backend for ByteCodeBackend<'_> {
    type Output = BlockChain;

    fn constant(&mut self, c: &Sexpr) -> Self::Output {
        self.storage.ensure(1);
        let value = self.storage.sexpr_to_value(c).unwrap();
        let block = BasicBlock::new(vec![Op::Const(0)], vec![value]);
        BlockChain::singleton(block)
    }

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output {
        second.append_op(Op::Cons);
        first.append(second)
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

        let cs = code.build_segment();
        assert_eq!(cs.code_slice(), &[Op::Const(0), Op::Halt]);
        assert_eq!(cs.constant_slice(), &[Value::Nil]);
    }

    #[test]
    fn build_bytecode_cons() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(&Sexpr::int(1));
        let b = bcb.constant(&Sexpr::int(2));

        let c = bcb.cons(a, b);

        let cs = c.build_segment();
        assert_eq!(
            cs.code_slice(),
            &[Op::Const(0), Op::Const(1), Op::Cons, Op::Halt]
        );
        assert_eq!(cs.constant_slice(), &[Value::Int(1), Value::Int(2)]);
    }

    #[test]
    fn build_bytecode_multiple_cons() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(&Sexpr::int(1));
        let b = bcb.constant(&Sexpr::int(2));
        let c = bcb.constant(&Sexpr::int(3));
        let d = bcb.cons(b, a);
        let e = bcb.cons(c, d);

        let cs = e.build_segment();
        assert_eq!(
            cs.code_slice(),
            &[
                Op::Const(0),
                Op::Const(1),
                Op::Const(2),
                Op::Cons,
                Op::Cons,
                Op::Halt
            ]
        );
        assert_eq!(
            cs.constant_slice(),
            &[Value::Int(3), Value::Int(2), Value::Int(1)]
        );
    }
}
