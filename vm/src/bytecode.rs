use crate::mem::{Ref, Traceable, Tracer};
use crate::Value;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u16)]
pub enum Op {
    Nop,
    ExtArg(u8),

    Halt,
    Jump { forward: u8 },
    JumpIfTrue { forward: u8 },
    JumpIfVoid { forward: u8 },
    Return,
    Call { n_args: u8 },

    Integer(u8),
    Const(u8),
    GetArg(u8),

    Cons,
    Car,
    Cdr,

    Table,
    TableSet,
    TableGet,

    MakeClosure { n_free: u8 },
}

impl Traceable for Op {
    fn trace(&self, _: &mut Tracer) {}
}

#[derive(Debug, Clone)]
pub struct CodeSegment {
    code: Box<[Op]>,
    constants: Box<[Value]>,
}

impl Traceable for CodeSegment {
    fn trace(&self, gc: &mut Tracer) {
        self.constants.trace(gc);
    }
}

impl CodeSegment {
    pub fn new(code: Box<[Op]>, constants: Box<[Value]>) -> Self {
        CodeSegment { code, constants }
    }

    pub fn get_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    pub fn get_op(&self, position: usize) -> Op {
        self.code[position]
    }

    pub fn code_slice(&self) -> &[Op] {
        &*self.code
    }

    pub fn constant_slice(&self) -> &[Value] {
        &*self.constants
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodePointer {
    segment: Ref<CodeSegment>,
    position: usize,
}

impl Traceable for CodePointer {
    fn trace(&self, gc: &mut Tracer) {
        self.segment.trace(gc);
    }
}

impl CodePointer {
    pub fn new(segment: Ref<CodeSegment>) -> Self {
        CodePointer {
            segment,
            position: 0,
        }
    }

    pub fn at(self, position: usize) -> Self {
        CodePointer { position, ..self }
    }

    pub fn offset(&self, offset: isize) -> Self {
        CodePointer {
            segment: self.segment.clone(),
            position: (self.position as isize + offset) as usize,
        }
    }

    pub fn get_constant(&self, index: usize) -> &Value {
        self.segment.get_constant(index)
    }

    pub fn fetch(&mut self) -> Op {
        let op = self.segment.get_op(self.position);
        self.position += 1;
        op
    }

    pub fn step_forward(&mut self, amount: usize) {
        self.position += amount;
    }

    pub fn step_back(&mut self) {
        self.position -= 1;
    }

    pub fn code_slice(&self) -> &[Op] {
        &self.segment.code_slice()[self.position..]
    }

    pub fn constant_slice(&self) -> &[Value] {
        self.segment.constant_slice()
    }
}

#[derive(Debug)]
pub struct CodeBuilder {
    code: Vec<BuildOp>,
    constants: Vec<Value>,
    labels: HashMap<String, usize>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        CodeBuilder {
            code: vec![],
            constants: vec![],
            labels: HashMap::new(),
        }
    }

    pub fn build(self) -> Result<CodeSegment, Self> {
        let bytecode = Self::build_code(self.code, self.labels);
        let constants = self.constants.into_boxed_slice();
        let segment = CodeSegment::new(bytecode, constants);
        Ok(segment)
    }

    fn build_code(code: Vec<BuildOp>, labels: HashMap<String, usize>) -> Box<[Op]> {
        let mut bytecode = vec![];
        for op_builder in code {
            match op_builder {
                BuildOp::Op(op) => bytecode.push(op),
                BuildOp::LabelRef(label, builder) => bytecode.push(builder(labels[&label])),
            }
        }
        bytecode.into_boxed_slice()
    }

    pub fn label(mut self, name: impl ToString) -> Self {
        if self
            .labels
            .insert(name.to_string(), self.code.len())
            .is_some()
        {
            panic!("Duplicate label: {:?}", name.to_string())
        }
        self
    }

    pub fn constant(mut self, value: Value) -> Self {
        let idx = self.add_constant(value);
        self.with(Op::Const, idx)
    }

    pub fn jump_to(mut self, label: impl ToString) -> Self {
        let position_reference = self.code.len() + 1;
        self.code.push(BuildOp::LabelRef(
            label.to_string(),
            Box::new(move |label_idx| {
                let offset = label_idx - position_reference;
                if offset > u8::max_value() as usize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                Op::Jump {
                    forward: offset as u8,
                }
            }),
        ));

        self
    }

    pub fn branch_if(mut self, label: impl ToString) -> Self {
        let position_reference = self.code.len() + 1;
        self.code.push(BuildOp::LabelRef(
            label.to_string(),
            Box::new(move |label_idx| {
                let offset = label_idx - position_reference;
                if offset > u8::max_value() as usize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                Op::JumpIfTrue {
                    forward: offset as u8,
                }
            }),
        ));

        self
    }

    pub fn branch_void(mut self, label: impl ToString) -> Self {
        let position_reference = self.code.len() + 1;
        self.code.push(BuildOp::LabelRef(
            label.to_string(),
            Box::new(move |label_idx| {
                let offset = label_idx - position_reference;
                if offset > u8::max_value() as usize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                Op::JumpIfVoid {
                    forward: offset as u8,
                }
            }),
        ));

        self
    }

    pub fn make_closure(mut self, label: impl ToString, n_free: usize) -> Self {
        let placeholder_pos = self.code.len();
        self = self.op(Op::Nop); // placeholder
        self = self.with(|n_free| Op::MakeClosure { n_free }, n_free);
        let position_reference = self.code.len();

        self.code[placeholder_pos] = BuildOp::LabelRef(
            label.to_string(),
            Box::new(move |label_idx| {
                let offset = label_idx - position_reference;
                if offset > u8::max_value() as usize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                Op::Integer(offset as u8)
            }),
        );

        self
    }

    pub fn with(mut self, op: impl Fn(u8) -> Op, mut arg: usize) -> Self {
        if arg <= u8::max_value() as usize {
            return self.op(op(arg as u8));
        }

        let mut reverse_args = vec![];

        while arg > 0 {
            reverse_args.push((arg & 0xff) as u8);
            arg = arg >> 8;
        }

        while reverse_args.len() > 1 {
            let ext = reverse_args.pop().unwrap();
            self = self.op(Op::ExtArg(ext as u8));
        }

        let arg = reverse_args.pop().unwrap();
        self.op(op(arg as u8))
    }

    pub fn op(mut self, op: Op) -> Self {
        self.code.push(BuildOp::Op(op));
        self
    }

    fn add_constant(&mut self, value: Value) -> usize {
        if let Some(idx) = self.constants.iter().position(|c| c == &value) {
            return idx;
        }

        self.constants.push(value);
        self.constants.len() - 1
    }
}

enum BuildOp {
    Op(Op),
    LabelRef(String, Box<dyn Fn(usize) -> Op>),
}

impl std::fmt::Debug for BuildOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BuildOp::Op(op) => op.fmt(f),
            BuildOp::LabelRef(name, _) => write!(f, "?@{}", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_empty_closure() {
        let segment = CodeBuilder::new().build().unwrap();
        assert_eq!(segment.code_slice(), &[]);
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_append_opcode() {
        let segment = CodeBuilder::new().op(Op::Halt).build().unwrap();
        assert_eq!(segment.code_slice(), &[Op::Halt]);
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_with_zero_arg() {
        let segment = CodeBuilder::new().with(Op::Const, 0).build().unwrap();
        assert_eq!(segment.code_slice(), &[Op::Const(0)]);
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_with_max_arg_value() {
        let segment = CodeBuilder::new().with(Op::Const, 255).build().unwrap();
        assert_eq!(segment.code_slice(), &[Op::Const(255)]);
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_with_smallest_extension_value() {
        let segment = CodeBuilder::new().with(Op::Const, 256).build().unwrap();
        assert_eq!(segment.code_slice(), &[Op::ExtArg(1), Op::Const(0)]);
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_with_large_extension_value() {
        let segment = CodeBuilder::new()
            .with(Op::Const, 0x0123_4567_89ab_cdef)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[
                Op::ExtArg(0x01),
                Op::ExtArg(0x23),
                Op::ExtArg(0x45),
                Op::ExtArg(0x67),
                Op::ExtArg(0x89),
                Op::ExtArg(0xab),
                Op::ExtArg(0xcd),
                Op::Const(0xef)
            ]
        );
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_with_largest_extension_value() {
        let segment = CodeBuilder::new()
            .with(Op::Const, 0xffff_ffff_ffff_ffff)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::ExtArg(255),
                Op::Const(255)
            ]
        );
        assert!(segment.constant_slice().is_empty());
    }

    #[test]
    fn build_constant() {
        let segment = CodeBuilder::new().constant(Value::Nil).build().unwrap();
        assert_eq!(segment.code_slice(), &[Op::Const(0)]);
        assert_eq!(segment.constant_slice(), &[Value::Nil]);
    }

    #[test]
    fn build_different_constants() {
        let segment = CodeBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .build()
            .unwrap();
        assert_eq!(segment.code_slice(), &[Op::Const(0), Op::Const(1)]);
        assert_eq!(segment.constant_slice(), &[Value::Int(1), Value::Int(2)]);
    }

    #[test]
    fn build_reuse_same_constants() {
        let segment = CodeBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .constant(Value::Int(1))
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[Op::Const(0), Op::Const(1), Op::Const(0)]
        );
        assert_eq!(segment.constant_slice(), &[Value::Int(1), Value::Int(2)]);
    }

    #[test]
    fn build_labels() {
        let cb = CodeBuilder::new()
            .label("a")
            .op(Op::Nop)
            .label("b")
            .label("c")
            .op(Op::Nop)
            .op(Op::Nop)
            .label("d");
        assert_eq!(cb.labels["a"], 0);
        assert_eq!(cb.labels["b"], 1);
        assert_eq!(cb.labels["c"], 1);
        assert_eq!(cb.labels["d"], 3);
    }

    #[test]
    fn build_labelled_closure() {
        let segment = CodeBuilder::new()
            .make_closure("func", 0)
            .op(Op::Halt)
            .label("func")
            .op(Op::Halt)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[
                Op::Integer(1),
                Op::MakeClosure { n_free: 0 },
                Op::Halt,
                Op::Halt
            ]
        );
    }

    #[test]
    fn build_jump_to_label() {
        let segment = CodeBuilder::new()
            .jump_to("func")
            .op(Op::Halt)
            .label("func")
            .op(Op::Halt)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[Op::Jump { forward: 1 }, Op::Halt, Op::Halt]
        );
    }

    #[test]
    fn build_branch_to_label() {
        let segment = CodeBuilder::new()
            .branch_if("func")
            .op(Op::Halt)
            .label("func")
            .op(Op::Halt)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[Op::JumpIfTrue { forward: 1 }, Op::Halt, Op::Halt]
        );
    }
}
