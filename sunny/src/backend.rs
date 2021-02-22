use sunny_sexpr_parser::Sexpr;
use sunny_sexpr_parser::SourceLocation;
use sunny_vm::bytecode::Op;
use sunny_vm::{BasicBlock, BlockChain, Value, ValueStorage};

pub trait Backend {
    type Output;

    fn add_global(&mut self, idx: usize);

    fn constant(&mut self, context: SourceLocation<()>, c: &Sexpr) -> Self::Output;

    fn fetch(&mut self, context: SourceLocation<()>, depth: usize, idx: usize) -> Self::Output;
    fn store(&mut self, depth: usize, idx: usize, val: Self::Output) -> Self::Output;

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output;

    fn ifexpr(
        &mut self,
        condition: Self::Output,
        consequent: Self::Output,
        alternative: Self::Output,
    ) -> Self::Output;

    fn sequence(&mut self, first: Self::Output, next: Self::Output) -> Self::Output;

    fn lambda(&mut self, n_args: usize, body: Self::Output) -> Self::Output;

    fn invoke(&mut self, func: Self::Output, args: Vec<Self::Output>) -> Self::Output;
}

pub struct ByteCodeBackend<'s> {
    storage: &'s mut ValueStorage,
    prelude: BlockChain,
}

impl<'s> ByteCodeBackend<'s> {
    pub fn new(storage: &'s mut ValueStorage) -> Self {
        ByteCodeBackend {
            storage,
            prelude: BlockChain::empty(),
        }
    }
}

impl ByteCodeBackend<'_> {
    pub fn generate_prelude(&self) -> BlockChain {
        self.prelude.clone()
    }
}

impl Backend for ByteCodeBackend<'_> {
    type Output = BlockChain;

    fn add_global(&mut self, _: usize) {
        let block = BasicBlock::new(vec![Op::Const(0), Op::PushLocal], vec![Value::Void]);
        self.prelude.append(BlockChain::singleton(block));
    }

    fn constant(&mut self, context: SourceLocation<()>, c: &Sexpr) -> Self::Output {
        let allocs = self.storage.count_allocations(c);
        self.storage.ensure(allocs);
        let value = self.storage.sexpr_to_value(c).unwrap();
        let mut block = BasicBlock::new(vec![Op::Const(0)], vec![value]);
        block.map_source(0, context);
        BlockChain::singleton(block)
    }

    fn fetch(&mut self, context: SourceLocation<()>, depth: usize, idx: usize) -> Self::Output {
        let mut ops = vec![];
        ops.extend(Op::extended(Op::Integer, depth));
        ops.extend(Op::extended(Op::Fetch, idx));
        let mut block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        BlockChain::singleton(block)
    }

    fn store(&mut self, depth: usize, idx: usize, val: Self::Output) -> Self::Output {
        let mut ops = vec![];
        ops.extend(Op::extended(Op::Integer, depth));
        ops.extend(Op::extended(Op::Store, idx));
        let block = BasicBlock::new(ops, vec![]);
        val.chain(BlockChain::singleton(block))
    }

    fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output {
        second.append_op(Op::Cons);
        first.chain(second)
    }

    fn ifexpr(
        &mut self,
        condition: Self::Output,
        consequent: Self::Output,
        alternative: Self::Output,
    ) -> Self::Output {
        let exit = BlockChain::empty();
        condition.branch_bool(consequent, alternative, exit)
    }

    fn sequence(&mut self, _first: Self::Output, _next: Self::Output) -> Self::Output {
        unimplemented!()
    }

    fn lambda(&mut self, n_args: usize, body: Self::Output) -> Self::Output {
        let start = BlockChain::empty();
        let exit = BlockChain::empty();
        body.return_from();
        start.make_closure(body, exit)
    }

    fn invoke(&mut self, func: Self::Output, args: Vec<Self::Output>) -> Self::Output {
        let mut blocks = BlockChain::empty();
        let n_args = args.len();
        for a in args.into_iter().rev() {
            blocks.append(a)
        }
        blocks.append(func);
        blocks.append_ops(Op::extended(|n_args| Op::Call { n_args }, n_args));
        blocks
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_bytecode_constant() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);

        let code = bcb.constant(SourceLocation::new(()), &Sexpr::nil());

        let cs = code.build_segment();
        assert_eq!(cs.code_slice(), &[Op::Const(0), Op::Halt]);
        assert_eq!(cs.constant_slice(), &[Value::Nil]);
    }

    #[test]
    fn build_bytecode_cons() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(SourceLocation::new(()), &Sexpr::int(1));
        let b = bcb.constant(SourceLocation::new(()), &Sexpr::int(2));

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
        let a = bcb.constant(SourceLocation::new(()), &Sexpr::int(1));
        let b = bcb.constant(SourceLocation::new(()), &Sexpr::int(2));
        let c = bcb.constant(SourceLocation::new(()), &Sexpr::int(3));
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

    #[test]
    fn build_bytecode_if() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let a = bcb.constant(SourceLocation::new(()), &Sexpr::int(1));
        let b = bcb.constant(SourceLocation::new(()), &Sexpr::int(2));
        let c = bcb.constant(SourceLocation::new(()), &Sexpr::int(3));

        let cs = bcb.ifexpr(a, b, c).build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::Const(0),
                Op::JumpIfTrue { forward: 2 },
                Op::Const(1),
                Op::Jump { forward: 1 },
                Op::Const(2),
                Op::Halt
            ]
        );
        assert_eq!(
            cs.constant_slice(),
            &[Value::Int(1), Value::Int(3), Value::Int(2)]
        );
    }

    #[test]
    fn build_bytecode_fetch() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let expr = bcb.fetch(SourceLocation::new(()), 0, 0);

        let cs = expr.build_segment();

        assert_eq!(cs.code_slice(), &[Op::Integer(0), Op::Fetch(0), Op::Halt]);
    }

    #[test]
    fn build_bytecode_store() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let val = bcb.constant(SourceLocation::new(()), &Sexpr::int(42));
        let expr = bcb.store(0, 0, val);

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[Op::Const(0), Op::Integer(0), Op::Store(0), Op::Halt]
        );
    }

    #[test]
    fn build_bytecode_trivial_lambda() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let val = bcb.constant(SourceLocation::new(()), &Sexpr::int(42));
        let expr = bcb.lambda(0, val);

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 2 },
                Op::Const(0),
                Op::Return,
                Op::Halt
            ]
        );
    }

    #[test]
    fn build_bytecode_can_return_from_lambda_definition() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let val = bcb.constant(SourceLocation::new(()), &Sexpr::int(42));
        let expr = bcb.lambda(0, val);
        expr.return_from();

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 2 },
                Op::Const(0),
                Op::Return,
                Op::Return
            ]
        );
    }

    #[test]
    fn build_bytecode_invoke_lambda() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let val = bcb.constant(SourceLocation::new(()), &Sexpr::int(42));
        let expr = bcb.lambda(0, val);
        let invoke = bcb.invoke(expr, vec![]);

        let cs = invoke.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 2 },
                Op::Const(0),
                Op::Return,
                Op::Call { n_args: 0 },
                Op::Halt
            ]
        );
    }

    #[test]
    fn build_bytecode_invoke_pushes_args_in_reverse_order() {
        let mut storage = ValueStorage::new(100);
        let mut bcb = ByteCodeBackend::new(&mut storage);
        let func = bcb.fetch(SourceLocation::new(()), 0, 0);
        let args = vec![
            bcb.constant(SourceLocation::new(()), &Sexpr::int(1)),
            bcb.constant(SourceLocation::new(()), &Sexpr::int(2)),
            bcb.constant(SourceLocation::new(()), &Sexpr::int(3)),
        ];
        let invoke = bcb.invoke(func, args);

        let cs = invoke.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::Const(0),
                Op::Const(1),
                Op::Const(2),
                Op::Integer(0),
                Op::Fetch(0),
                Op::Call { n_args: 3 },
                Op::Halt
            ]
        );

        assert_eq!(
            cs.constant_slice(),
            &[Value::Int(3), Value::Int(2), Value::Int(1)]
        )
    }
}
