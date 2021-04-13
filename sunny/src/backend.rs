use crate::frontend::library::Export;
use std::collections::HashMap;
use sunny_sexpr_parser::Scm;
use sunny_sexpr_parser::SourceLocation;
use sunny_vm::bytecode::Op;
use sunny_vm::{BasicBlock, BlockChain, ValueStorage};

pub trait Backend {
    type Ir;

    fn void(&mut self) -> Self::Ir;

    fn constant(&mut self, context: SourceLocation<()>, c: Scm) -> Self::Ir;

    fn fetch(&mut self, context: SourceLocation<()>, idx: usize) -> Self::Ir;
    fn store(&mut self, context: SourceLocation<()>, idx: usize, val: Self::Ir) -> Self::Ir;

    fn fetch_global(&mut self, context: SourceLocation<()>, name: &str) -> Self::Ir;
    fn store_global(&mut self, context: SourceLocation<()>, name: &str, val: Self::Ir) -> Self::Ir;

    fn ifexpr(
        &mut self,
        context: SourceLocation<()>,
        condition: Self::Ir,
        consequence: Self::Ir,
        alternative: Self::Ir,
    ) -> Self::Ir;

    fn sequence(&mut self, first: Self::Ir, next: Self::Ir) -> Self::Ir;

    fn lambda(
        &mut self,
        context: SourceLocation<()>,
        n_args: usize,
        accept_vararg: bool,
        body: Self::Ir,
    ) -> Self::Ir;

    fn invoke(&mut self, context: SourceLocation<()>, args: Vec<Self::Ir>) -> Self::Ir;

    fn intrinsic(
        &mut self,
        context: SourceLocation<()>,
        name: &'static str,
        args: Vec<Self::Ir>,
    ) -> Self::Ir;

    fn module(&mut self, name: &str, body: Self::Ir, exports: &[Export]) -> Self::Ir;
}

pub struct ByteCodeBackend<'s> {
    storage: &'s mut ValueStorage,
    global_table: &'s mut GlobalTable,
}

impl<'s> ByteCodeBackend<'s> {
    pub fn new(storage: &'s mut ValueStorage, global_table: &'s mut GlobalTable) -> Self {
        ByteCodeBackend {
            storage,
            global_table,
        }
    }
}

impl Backend for ByteCodeBackend<'_> {
    type Ir = BlockChain;

    fn void(&mut self) -> Self::Ir {
        BlockChain::singleton(BasicBlock::new(vec![Op::Void], vec![]))
    }

    fn constant(&mut self, context: SourceLocation<()>, c: Scm) -> Self::Ir {
        let block = BasicBlock::new(vec![Op::Const(0)], vec![c]);
        block.map_source(0, context);
        BlockChain::singleton(block)
    }

    fn fetch(&mut self, context: SourceLocation<()>, idx: usize) -> Self::Ir {
        let mut ops = vec![];
        ops.extend(Op::extended(Op::Fetch, idx));
        let block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        BlockChain::singleton(block)
    }

    fn store(&mut self, context: SourceLocation<()>, idx: usize, val: Self::Ir) -> Self::Ir {
        let mut ops = vec![];
        ops.extend(Op::extended(Op::Store, idx));
        ops.push(Op::Void);
        let block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        val.chain(BlockChain::singleton(block))
    }

    fn fetch_global(&mut self, context: SourceLocation<()>, name: &str) -> Self::Ir {
        let idx = self.global_table.determine_index(name);

        let mut ops = vec![];
        ops.extend(Op::extended(Op::FetchGlobal, idx));
        let block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        BlockChain::singleton(block)
    }

    fn store_global(&mut self, context: SourceLocation<()>, name: &str, val: Self::Ir) -> Self::Ir {
        let idx = self.global_table.determine_index(name);

        let mut ops = vec![];
        ops.extend(Op::extended(Op::StoreGlobal, idx));
        ops.push(Op::Void);
        let block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        val.chain(BlockChain::singleton(block))
    }

    fn ifexpr(
        &mut self,
        context: SourceLocation<()>,
        condition: Self::Ir,
        consequent: Self::Ir,
        alternative: Self::Ir,
    ) -> Self::Ir {
        let exit = BlockChain::empty();
        condition.branch_bool_with_context(consequent, alternative, exit, context)
    }

    fn sequence(&mut self, first: Self::Ir, next: Self::Ir) -> Self::Ir {
        first.append_op(Op::Drop);
        first.chain(next)
    }

    fn lambda(
        &mut self,
        context: SourceLocation<()>,
        n_args: usize,
        accept_vararg: bool,
        body: Self::Ir,
    ) -> Self::Ir {
        body.return_from_with_context(context.clone());
        let mut preamble = BlockChain::empty();

        let argop = if accept_vararg {
            Op::PrepareVarArgs
        } else {
            Op::PrepareArgs
        };

        preamble.append_ops(Op::extended(argop, n_args));
        preamble.append(body);

        let prologue = BlockChain::empty();
        let epilogue = BlockChain::empty();
        prologue.make_closure(context, preamble, epilogue)
    }

    fn invoke(&mut self, context: SourceLocation<()>, args: Vec<Self::Ir>) -> Self::Ir {
        let mut blocks = BlockChain::empty();
        let n_args = args.len() - 1;
        for a in args.into_iter().rev() {
            blocks.append(a)
        }
        blocks.append_ops_with_context(Op::extended(|n_args| Op::Call { n_args }, n_args), context);
        blocks
    }

    fn intrinsic(
        &mut self,
        context: SourceLocation<()>,
        name: &'static str,
        args: Vec<Self::Ir>,
    ) -> Self::Ir {
        let mut blocks = BlockChain::empty();
        for a in args.into_iter() {
            blocks.append(a)
        }

        match name {
            "apply" => blocks.append_op_with_context(Op::Apply, context),
            "call/cc" => blocks.append_ops(vec![
                Op::CaptureContinuation { offset: 2 },
                Op::Swap,
                Op::Call { n_args: 1 },
            ]),
            "call-with-values" => {
                blocks.append_ops(vec![Op::Swap, Op::Call { n_args: 0 }, Op::CallDynamic])
            }
            "car" => blocks.append_op_with_context(Op::Car, context),
            "cdr" => blocks.append_op_with_context(Op::Cdr, context),
            "cons" => blocks.append_op_with_context(Op::Cons, context),
            "eq?" => blocks.append_op_with_context(Op::Eq, context),
            _ => unimplemented!("intrinsic {}", name),
        }

        blocks
    }

    fn module(&mut self, _name: &str, body: Self::Ir, _exports: &[Export]) -> Self::Ir {
        body
    }
}

pub struct GlobalTable {
    mappings: HashMap<String, usize>,
}

impl Default for GlobalTable {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTable {
    pub fn new() -> Self {
        GlobalTable {
            mappings: HashMap::new(),
        }
    }

    pub fn determine_index(&mut self, name: &str) -> usize {
        if let Some(idx) = self.mappings.get(name) {
            *idx
        } else {
            self.add_variable(name)
        }
    }

    pub fn add_variable(&mut self, name: impl ToString) -> usize {
        let idx = self.mappings.len();
        self.mappings.insert(name.to_string(), idx);
        idx
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sunny_vm::Value;

    #[test]
    fn build_bytecode_constant() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);

        let code = bcb.constant(SourceLocation::new(()), Scm::null());

        let cs = code.build_segment();
        assert_eq!(cs.code_slice(), &[Op::Const(0), Op::Halt]);
        assert_eq!(cs.constant_slice(), &[Scm::null()]);
    }

    #[test]
    fn build_bytecode_if() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let a = bcb.constant(SourceLocation::new(()), Scm::int(1));
        let b = bcb.constant(SourceLocation::new(()), Scm::int(2));
        let c = bcb.constant(SourceLocation::new(()), Scm::int(3));

        let cs = bcb.ifexpr(SourceLocation::new(()), a, b, c).build_segment();

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
            &[Scm::number(1), Scm::number(3), Scm::number(2)]
        );
    }

    #[test]
    fn build_bytecode_fetch() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let expr = bcb.fetch(SourceLocation::new(()), 0);

        let cs = expr.build_segment();

        assert_eq!(cs.code_slice(), &[Op::Fetch(0), Op::Halt]);
    }

    #[test]
    fn build_bytecode_store() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let val = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let expr = bcb.store(SourceLocation::new(()), 0, val);

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[Op::Const(0), Op::Store(0), Op::Void, Op::Halt]
        );
    }

    #[test]
    fn build_bytecode_trivial_lambda() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let val = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let expr = bcb.lambda(SourceLocation::new(()), 0, false, val);

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 3 },
                Op::PrepareArgs(0),
                Op::Const(0),
                Op::Return,
                Op::Halt
            ]
        );
    }

    #[test]
    fn build_bytecode_vararg_lambda() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let val = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let expr = bcb.lambda(SourceLocation::new(()), 0, true, val);

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 3 },
                Op::PrepareVarArgs(0),
                Op::Const(0),
                Op::Return,
                Op::Halt
            ]
        );
    }

    #[test]
    fn build_bytecode_can_return_from_lambda_definition() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let val = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let expr = bcb.lambda(SourceLocation::new(()), 0, false, val);
        expr.return_from();

        let cs = expr.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 3 },
                Op::PrepareArgs(0),
                Op::Const(0),
                Op::Return,
                Op::Return
            ]
        );
    }

    #[test]
    fn build_bytecode_invoke_lambda() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let val = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let expr = bcb.lambda(SourceLocation::new(()), 0, false, val);
        let invoke = bcb.invoke(SourceLocation::new(()), vec![expr]);

        let cs = invoke.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::MakeClosure { offset: 1 },
                Op::Jump { forward: 3 },
                Op::PrepareArgs(0),
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
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);
        let context = SourceLocation::new(());
        let idx = 0;
        let mut ops = vec![];
        ops.extend(Op::extended(Op::Fetch, idx));
        let block = BasicBlock::new(ops, vec![]);
        block.map_source(0, context.clone());
        block.map_source(1, context);
        let args = vec![
            BlockChain::singleton(block),
            bcb.constant(SourceLocation::new(()), Scm::int(1)),
            bcb.constant(SourceLocation::new(()), Scm::int(2)),
            bcb.constant(SourceLocation::new(()), Scm::int(3)),
        ];
        let invoke = bcb.invoke(SourceLocation::new(()), args);

        let cs = invoke.build_segment();

        assert_eq!(
            cs.code_slice(),
            &[
                Op::Const(0),
                Op::Const(1),
                Op::Const(2),
                Op::Fetch(0),
                Op::Call { n_args: 3 },
                Op::Halt
            ]
        );

        assert_eq!(
            cs.constant_slice(),
            &[Scm::number(3), Scm::number(2), Scm::number(1)]
        )
    }

    #[test]
    fn build_bytecode_sequence() {
        let mut storage = ValueStorage::new(100);
        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);

        let first = BlockChain::singleton(Op::Integer(1));
        let next = BlockChain::singleton(Op::Integer(2));

        let code = bcb.sequence(first, next);

        let cs = code.build_segment();
        assert_eq!(
            cs.code_slice(),
            &[Op::Integer(1), Op::Drop, Op::Integer(2), Op::Halt]
        );
    }

    #[test]
    fn build_module() {
        let mut storage = ValueStorage::new(100);

        let mut global_table = GlobalTable::new();
        let mut bcb = ByteCodeBackend::new(&mut storage, &mut global_table);

        let body = bcb.constant(SourceLocation::new(()), Scm::int(42));
        let code = bcb.module("mod", body, &[]);

        let cs = code.build_segment();
        assert_eq!(cs.code_slice(), &[Op::Const(0), Op::Halt]);
        assert_eq!(cs.constant_slice(), &[Scm::number(42)]);
    }
}
