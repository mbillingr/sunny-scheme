use crate::bytecode::{CodeSegment, Op};
use crate::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

#[derive(Debug, Default)]
pub struct BasicBlock {
    code: Vec<Op>,
    constants: Vec<Value>,
    exit: RefCell<Exit>,
}

impl BasicBlock {
    pub fn new(code: Vec<Op>, constants: Vec<Value>) -> Self {
        BasicBlock {
            code,
            constants,
            exit: Default::default(),
        }
    }

    pub fn is_valid(&self) -> bool {
        self.code.iter().all(|op| match op {
            Op::Jump { .. } | Op::JumpIfTrue { .. } | Op::JumpIfVoid { .. } => false,
            Op::RJump { .. } | Op::RJumpIfTrue { .. } | Op::RJumpIfVoid { .. } => false,
            Op::Halt | Op::Return => false,
            _ => true,
        })
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
        let mut constant_map = HashMap::new();

        let mut code = vec![];
        for &op in &self.code {
            match op {
                Op::Const(c) => {
                    let mut const_idx = c as usize;
                    let mut multiplier = 256;
                    while let Some(Op::ExtArg(x)) = code.last() {
                        const_idx += multiplier * *x as usize;
                        multiplier <<= 8;
                        code.pop();
                    }

                    let value = self.constants[const_idx].clone();

                    let n = constant_map.len();
                    let new_idx = *constant_map.entry(value).or_insert(n);

                    let ops = Op::extended(Op::Const, new_idx);
                    code.extend(ops);
                }
                _ => code.push(op),
            }
        }
        code.push(Op::Halt);

        let mut constants = vec![Value::Void; constant_map.len()];
        for (val, idx) in constant_map {
            constants[idx] = val;
        }

        CodeSegment::new(code, constants)
    }

    fn build_constants(&self, constants: &mut HashMap<Value, usize>) {
        let mut const_idx = 0;
        for &op in &self.code {
            match op {
                Op::ExtArg(x) => const_idx = Op::extend_arg(x, const_idx),
                Op::Const(x) => {
                    const_idx = Op::extend_arg(x, const_idx);
                    let value = &self.constants[const_idx];

                    if !constants.contains_key(value) {
                        let idx = constants.len();
                        constants.insert(value.clone(), idx);
                    }

                    const_idx = 0;
                }
                _ => const_idx = 0,
            }
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
        let mut block1 = BasicBlock::default();
        let block2 = BasicBlock::default();
        block1.jump_to(block2); // should compile
    }

    #[test]
    fn jump_to_block_from_different_parents() {
        let mut block1 = BasicBlock::default();
        let mut block2 = BasicBlock::default();
        let common_target = BasicBlock::default();

        let common_target = Rc::new(common_target);
        block1.jump_to(common_target.clone()); // should compile
        block2.jump_to(common_target); // should compile
    }

    #[test]
    fn construct_loop() {
        let mut block1 = Rc::new(BasicBlock::default());
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
        unimplemented!()
    }

    #[test]
    fn convert_to_code_segment_produces_branches() {
        unimplemented!()
    }

    #[test]
    fn convert_to_code_segment_inserts_a_multijump_target_only_once() {
        unimplemented!()
    }

    #[test]
    fn convert_to_code_segment_correctly_inserts_extargs_for_constants() {
        unimplemented!()
    }

    #[test]
    fn convert_to_code_segment_correctly_removes_extargs_for_constants() {
        unimplemented!()
    }
}
