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
        let mut block_offsets = HashMap::new();

        self.build_code(&mut code, &mut constant_map, &mut block_offsets);

        let mut constants = vec![Value::Void; constant_map.len()];
        for (val, idx) in constant_map {
            constants[idx] = val;
        }

        CodeSegment::new(code, constants)
    }

    fn build_code(
        &self,
        code: &mut Vec<Op>,
        constant_map: &mut HashMap<Value, usize>,
        block_offsets: &mut HashMap<*const Self, usize>,
    ) {
        block_offsets.insert(self, code.len());

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

        match &*self.exit.borrow() {
            Exit::Halt => code.push(Op::Halt),
            Exit::Return => code.push(Op::Return),
            Exit::Jump(target) => {
                if let Some(&offset) = block_offsets.get(&(&**target as *const _)) {
                    BasicBlock::build_jump(code, offset)
                } else {
                    target.build_code(code, constant_map, block_offsets);
                }
            }
            Exit::BranchTrue(true_target, false_target) => {
                BasicBlock::build_branch(
                    code,
                    constant_map,
                    block_offsets,
                    true_target,
                    false_target,
                    |forward| Op::JumpIfTrue { forward },
                );
            }
            Exit::BranchVoid(true_target, false_target) => {
                BasicBlock::build_branch(
                    code,
                    constant_map,
                    block_offsets,
                    true_target,
                    false_target,
                    |forward| Op::JumpIfVoid { forward },
                );
            }
            _ => unimplemented!(),
        }
    }

    fn build_jump(code: &mut Vec<Op>, offset: usize) {
        if offset >= code.len() {
            code.extend(Op::extended(
                |forward| Op::Jump { forward },
                offset - code.len(),
            ));
        } else {
            code.extend(Op::extended(
                |backward| Op::RJump { backward },
                code.len() - offset,
            ));
        }
    }

    fn build_branch(
        code: &mut Vec<Op>,
        constant_map: &mut HashMap<Value, usize>,
        block_offsets: &mut HashMap<*const Self, usize>,
        true_target: &Rc<BasicBlock>,
        false_target: &Rc<BasicBlock>,
        op_maker: impl FnOnce(u8) -> Op,
    ) {
        if let Some(&offset) = block_offsets.get(&(&**false_target as *const _)) {
            code.push(op_maker(1));
            Self::build_jump(code, offset);
        } else {
            let mut false_code = vec![];
            false_target.build_code(&mut false_code, constant_map, &mut block_offsets.clone());

            code.extend(Op::extended(op_maker, false_code.len()));

            false_target.build_code(code, constant_map, block_offsets);
        }

        if let Some(&offset) = block_offsets.get(&(&**true_target as *const _)) {
            Self::build_jump(code, offset);
        } else {
            true_target.build_code(code, constant_map, block_offsets);
        }
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
