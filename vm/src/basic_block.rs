use crate::bytecode::{CodeSegment, Op};
use crate::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// A chain of blocks with a single entry and exit point.
/// Control flow may branch in a chain, but all branches
/// must eventually lead to the same last block.
pub struct BlockChain {
    first: Rc<BasicBlock>,
    last: Rc<BasicBlock>,
}

impl BlockChain {
    pub fn singleton(block: impl Into<Rc<BasicBlock>>) -> Self {
        let block = block.into();
        BlockChain {
            first: block.clone(),
            last: block,
        }
    }

    pub fn append_op(&self, op: Op) {
        self.last.append_op(op);
    }

    pub fn append(self, other: Self) -> Self {
        self.last.jump_to(other.first);
        BlockChain {
            first: self.first,
            last: other.last,
        }
    }

    pub fn build_segment(&self) -> CodeSegment {
        self.first.build_segment()
    }

    pub fn return_from(&self) {
        self.last.return_from()
    }
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    code: RefCell<Vec<Op>>,
    constants: RefCell<Vec<Value>>,
    exit: RefCell<Exit>,
}

impl BasicBlock {
    pub fn new(code: Vec<Op>, constants: Vec<Value>) -> Self {
        BasicBlock {
            code: RefCell::new(code),
            constants: RefCell::new(constants),
            exit: Default::default(),
        }
    }

    pub fn is_valid(&self) -> bool {
        self.code.borrow().iter().all(|op| match op {
            Op::Jump { .. } | Op::JumpIfTrue { .. } | Op::JumpIfVoid { .. } => false,
            Op::RJump { .. } | Op::RJumpIfTrue { .. } | Op::RJumpIfVoid { .. } => false,
            Op::Halt | Op::Return => false,
            _ => true,
        })
    }

    pub fn append_op(&self, op: Op) {
        self.code.borrow_mut().push(op);
    }

    pub fn return_from(&self) {
        *self.exit.borrow_mut() = Exit::Return
    }

    pub fn jump_to(&self, target: impl Into<Rc<BasicBlock>>) {
        *self.exit.borrow_mut() = Exit::Jump(target.into())
    }

    pub fn branch_true(
        &self,
        true_target: impl Into<Rc<BasicBlock>>,
        false_target: impl Into<Rc<BasicBlock>>,
    ) {
        *self.exit.borrow_mut() = Exit::BranchTrue(true_target.into(), false_target.into())
    }

    pub fn branch_void(
        &self,
        void_target: impl Into<Rc<BasicBlock>>,
        else_target: impl Into<Rc<BasicBlock>>,
    ) {
        *self.exit.borrow_mut() = Exit::BranchVoid(void_target.into(), else_target.into())
    }

    pub fn loop_to(&self, target: impl Into<Weak<BasicBlock>>) {
        *self.exit.borrow_mut() = Exit::Loop(target.into())
    }

    pub fn build_segment(&self) -> CodeSegment {
        let mut builder = CodeBuilder::new();
        builder.build_code(self);

        let mut constants = vec![Value::Void; builder.constant_map.len()];
        for (val, idx) in builder.constant_map {
            constants[idx] = val;
        }

        CodeSegment::new(builder.code, constants)
    }
}

struct CodeBuilder {
    code: Vec<Op>,
    constant_map: HashMap<Value, usize>,
    block_offsets: HashMap<*const BasicBlock, usize>,
}

impl CodeBuilder {
    fn new() -> Self {
        CodeBuilder {
            code: vec![],
            constant_map: HashMap::new(),
            block_offsets: HashMap::new(),
        }
    }

    fn build_code(&mut self, block: &BasicBlock) {
        self.block_offsets.insert(block, self.code.len());

        for &op in &*block.code.borrow() {
            match op {
                Op::Const(c) => {
                    let mut const_idx = c as usize;
                    let mut multiplier = 256;
                    while let Some(Op::ExtArg(x)) = self.code.last() {
                        const_idx += multiplier * *x as usize;
                        multiplier <<= 8;
                        self.code.pop();
                    }

                    let value = block.constants.borrow()[const_idx].clone();

                    let n = self.constant_map.len();
                    let new_idx = *self.constant_map.entry(value).or_insert(n);

                    let ops = Op::extended(Op::Const, new_idx);
                    self.code.extend(ops);
                }
                _ => self.code.push(op),
            }
        }

        match &*block.exit.borrow() {
            Exit::Halt => self.code.push(Op::Halt),
            Exit::Return => self.code.push(Op::Return),
            Exit::Jump(target) => {
                if let Some(&offset) = self.block_offsets.get(&(&**target as *const _)) {
                    self.build_jump(offset)
                } else {
                    self.build_code(target);
                }
            }
            Exit::BranchTrue(true_target, false_target) => {
                self.build_branch(true_target, false_target, |forward| Op::JumpIfTrue {
                    forward,
                });
            }
            Exit::BranchVoid(true_target, false_target) => {
                self.build_branch(true_target, false_target, |forward| Op::JumpIfVoid {
                    forward,
                });
            }
            _ => unimplemented!(),
        }
    }

    fn build_jump(&mut self, offset: usize) {
        if offset >= self.code.len() {
            self.code.extend(Op::extended(
                |forward| Op::Jump { forward },
                offset - self.code.len(),
            ));
        } else {
            self.code.extend(Op::extended(
                |backward| Op::RJump { backward },
                self.code.len() - offset,
            ));
        }
    }

    fn build_branch(
        &mut self,
        true_target: &Rc<BasicBlock>,
        false_target: &Rc<BasicBlock>,
        op_maker: impl FnOnce(u8) -> Op,
    ) {
        if let Some(&offset) = self.block_offsets.get(&(&**false_target as *const _)) {
            self.code.push(op_maker(1));
            self.build_jump(offset);
        } else {
            let mut false_build = Self::new();
            false_build.constant_map = self.constant_map.clone();
            false_build.block_offsets = self.block_offsets.clone();
            false_build.build_code(false_target);

            self.code
                .extend(Op::extended(op_maker, false_build.code.len()));

            self.build_code(false_target);
        }

        if let Some(&offset) = self.block_offsets.get(&(&**true_target as *const _)) {
            self.build_jump(offset);
        } else {
            self.build_code(true_target);
        }
    }
}

#[derive(Debug, Clone)]
enum Exit {
    Halt,
    Return,
    Jump(Rc<BasicBlock>),
    BranchTrue(Rc<BasicBlock>, Rc<BasicBlock>),
    BranchVoid(Rc<BasicBlock>, Rc<BasicBlock>),
    Loop(Weak<BasicBlock>),
}

impl Default for Exit {
    fn default() -> Self {
        Self::Halt
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_block_is_valid() {
        assert!(BasicBlock::default().is_valid())
    }

    #[test]
    fn blocks_with_jumps_are_invalid() {
        assert!(!BasicBlock::new(vec![Op::Jump { forward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::JumpIfTrue { forward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::JumpIfVoid { forward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::RJump { backward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::RJumpIfTrue { backward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::RJumpIfVoid { backward: 0 }], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::Halt], vec![]).is_valid());
        assert!(!BasicBlock::new(vec![Op::Return], vec![]).is_valid());
    }

    #[test]
    fn connect_blocks_with_jump() {
        let block1 = BasicBlock::default();
        let block2 = BasicBlock::default();
        block1.jump_to(block2); // should compile
    }

    #[test]
    fn jump_to_block_from_different_parents() {
        let block1 = BasicBlock::default();
        let block2 = BasicBlock::default();
        let common_target = BasicBlock::default();

        let common_target = Rc::new(common_target);
        block1.jump_to(common_target.clone()); // should compile
        block2.jump_to(common_target); // should compile
    }

    #[test]
    fn construct_loop() {
        let block1 = Rc::new(BasicBlock::default());
        block1.jump_to(block1.clone()); // should compile
    }

    #[test]
    fn convert_single_basic_block_to_code_segment_copies_ops() {
        let block = BasicBlock::new(vec![Op::Nop], vec![]);

        let segment = block.build_segment();

        assert_eq!(segment.code_slice(), &[Op::Nop, Op::Halt]);
    }

    #[test]
    fn convert_basic_block_to_code_segment_copies_used_constants() {
        let block = BasicBlock::new(vec![Op::Const(0)], vec![Value::Int(1), Value::Int(2)]);

        let segment = block.build_segment();

        assert_eq!(segment.constant_slice(), &[Value::Int(1)]);
    }

    #[test]
    fn convert_basic_block_to_code_segment_adjusts_constant_indices() {
        let block = BasicBlock::new(vec![Op::Const(1)], vec![Value::Int(1), Value::Int(2)]);

        let segment = block.build_segment();

        assert_eq!(segment.code_slice()[0], Op::Const(0));
        assert_eq!(segment.constant_slice(), &[Value::Int(2)]);
    }

    #[test]
    fn convert_to_code_segment_appends_jump_target() {
        let block1 = BasicBlock::new(vec![Op::Integer(1)], vec![]);
        let block2 = BasicBlock::new(vec![Op::Integer(2)], vec![]);
        block1.jump_to(block2);

        let segment = block1.build_segment();

        assert_eq!(
            segment.code_slice(),
            &[Op::Integer(1), Op::Integer(2), Op::Halt]
        );
    }

    #[test]
    fn convert_to_code_segment_shares_constants_between_blocks() {
        let block1 = BasicBlock::new(vec![Op::Const(0)], vec![Value::Int(42)]);
        let block2 = BasicBlock::new(vec![Op::Const(1)], vec![Value::Nil, Value::Int(42)]);
        block1.jump_to(block2);

        let segment = block1.build_segment();

        assert_eq!(
            segment.code_slice(),
            &[Op::Const(0), Op::Const(0), Op::Halt]
        );
        assert_eq!(segment.constant_slice(), &[Value::Int(42)]);
    }

    #[test]
    fn convert_to_code_segment_produces_branches() {
        let block1 = BasicBlock::new(vec![], vec![]);
        let block2 = BasicBlock::new(vec![], vec![]);
        let block3 = BasicBlock::new(vec![], vec![]);
        block2.return_from();
        block1.branch_true(block2, block3);

        let segment = block1.build_segment();
        assert_eq!(
            segment.code_slice(),
            &[Op::JumpIfTrue { forward: 1 }, Op::Halt, Op::Return]
        );
    }

    #[test]
    fn convert_to_code_segment_inserts_blocks_only_once() {
        let block1 = Rc::new(BasicBlock::new(vec![Op::Integer(1)], vec![]));
        let block2 = Rc::new(BasicBlock::new(vec![Op::Integer(2)], vec![]));
        block1.jump_to(block2.clone());
        block2.jump_to(block1.clone());

        let segment = block1.build_segment();

        assert_eq!(
            segment.code_slice(),
            &[Op::Integer(1), Op::Integer(2), Op::RJump { backward: 2 }]
        );
    }

    #[test]
    fn convert_to_code_segment_inserts_branches_only_once() {
        let block1 = Rc::new(BasicBlock::new(vec![Op::Nop], vec![]));
        let block2 = Rc::new(BasicBlock::new(vec![Op::Nop], vec![]));
        block1.branch_true(block2.clone(), block2);

        let segment = block1.build_segment();

        assert_eq!(
            segment.code_slice(),
            &[
                Op::Nop,
                Op::JumpIfTrue { forward: 2 },
                Op::Nop,
                Op::Halt,
                Op::RJump { backward: 2 }
            ]
        );
    }

    #[test]
    fn convert_to_code_segment_correctly_inserts_extargs_for_constants() {
        let mut ops = vec![];
        let mut consts = vec![];
        for i in 0..=255 {
            ops.push(Op::Const(i));
            consts.push(Value::Int(i as i64));
        }
        let block1 = BasicBlock::new(ops, consts);
        let block2 = BasicBlock::new(vec![Op::Const(0)], vec![Value::Nil]);
        block1.jump_to(block2);

        let segment = block1.build_segment();

        assert_eq!(
            &segment.code_slice()[255..],
            &[Op::Const(255), Op::ExtArg(1), Op::Const(0), Op::Halt]
        );
    }

    #[test]
    fn convert_to_code_segment_correctly_removes_extargs_for_constants() {
        let block = BasicBlock::new(vec![Op::ExtArg(0), Op::Const(0)], vec![Value::Int(1)]);

        let segment = block.build_segment();

        assert_eq!(segment.code_slice(), &[Op::Const(0), Op::Halt]);
    }
}
