use crate::mem::{Ref, Traceable, Tracer};
use crate::Value;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u16)]
pub enum Op {
    Nop,
    ExtArg(u8),

    Inspect(u8), // for debugging purposes

    Halt,
    Jump { forward: u8 },
    JumpIfTrue { forward: u8 },
    JumpIfVoid { forward: u8 },
    RJump { backward: u8 },
    RJumpIfTrue { backward: u8 },
    RJumpIfVoid { backward: u8 },
    Return,
    Call { n_args: u8 },

    Peek(u8),
    PushLocal,
    DropLocal,
    FetchLocal(u8),
    Fetch(u8),
    Store(u8),

    Integer(u8),
    Const(u8),
    GetStack(u8),

    Dup,
    Drop,
    Swap,

    Eq,
    Inc,
    Dec,

    Cons,
    Car,
    Cdr,

    Table,
    TableSet,
    TableGet,

    MakeClosure { offset: u8 },
}

impl Op {
    #[inline(always)]
    pub fn extend_arg(a: u8, arg: usize) -> usize {
        a as usize + (arg << 8)
    }

    pub fn extended(op_maker: impl FnOnce(u8) -> Op, mut arg: usize) -> Vec<Self> {
        if arg <= u8::max_value() as usize {
            return vec![op_maker(arg as u8)];
        }

        let mut reverse_args = vec![];
        let mut ops = vec![];

        while arg > 0 {
            reverse_args.push((arg & 0xff) as u8);
            arg >>= 8;
        }

        while reverse_args.len() > 1 {
            let ext = reverse_args.pop().unwrap();
            ops.push(Op::ExtArg(ext as u8));
        }

        let idx = reverse_args.pop().unwrap();
        ops.push(op_maker(idx as u8));

        ops
    }
}

impl Traceable for Op {
    fn trace(&self, _: &mut Tracer) {}
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Op::Nop => write!(f, "{}", repr::NOP),
            Op::ExtArg(x) => write!(f, "{} {}", repr::EXTARG, x),
            Op::Inspect(x) => write!(f, "{} {}", repr::INSPECT, x),
            Op::Halt => write!(f, "{}", repr::HALT),
            Op::Jump { forward } => write!(f, "{} {}", repr::JUMP, forward),
            Op::JumpIfTrue { forward } => write!(f, "{} {}", repr::JUMPIFTRUE, forward),
            Op::JumpIfVoid { forward } => write!(f, "{} {}", repr::JUMPIFVOID, forward),
            Op::RJump { backward } => write!(f, "{} {}", repr::RJUMP, backward),
            Op::RJumpIfTrue { backward } => write!(f, "{} {}", repr::RJUMPIFTRUE, backward),
            Op::RJumpIfVoid { backward } => write!(f, "{} {}", repr::RJUMPIFVOID, backward),
            Op::Return => write!(f, "{}", repr::RETURN),
            Op::Call { n_args } => write!(f, "{} {}", repr::CALL, n_args),
            Op::Peek(x) => write!(f, "{} {}", repr::PEEK, x),
            Op::PushLocal => write!(f, "{}", repr::PUSHLOCAL),
            Op::DropLocal => write!(f, "{}", repr::DROPLOCAL),
            Op::FetchLocal(x) => write!(f, "{} {}", repr::FETCHLOCAL, x),
            Op::Fetch(x) => write!(f, "{} {}", repr::FETCH, x),
            Op::Store(x) => write!(f, "{} {}", repr::STORE, x),
            Op::Integer(x) => write!(f, "{} {}", repr::INTEGER, x),
            Op::Const(x) => write!(f, "{} {}", repr::CONST, x),
            Op::GetStack(x) => write!(f, "{} {}", repr::GETSTACK, x),
            Op::Dup => write!(f, "{}", repr::DUP),
            Op::Drop => write!(f, "{}", repr::DROP),
            Op::Swap => write!(f, "{}", repr::SWAP),
            Op::Eq => write!(f, "{}", repr::EQ),
            Op::Inc => write!(f, "{}", repr::INC),
            Op::Dec => write!(f, "{}", repr::DEC),
            Op::Cons => write!(f, "{}", repr::CONS),
            Op::Car => write!(f, "{}", repr::CAR),
            Op::Cdr => write!(f, "{}", repr::CDR),
            Op::Table => write!(f, "{}", repr::TABLE),
            Op::TableSet => write!(f, "{}", repr::TABLESET),
            Op::TableGet => write!(f, "{}", repr::TABLEGET),
            Op::MakeClosure { offset } => write!(f, "{} {}", repr::MAKECLOSURE, offset),
        }
    }
}

pub mod repr {
    pub const NOP: &'static str = "NOP";
    pub const EXTARG: &'static str = "EXTARG";
    pub const INSPECT: &'static str = "INSPECT";
    pub const HALT: &'static str = "HALT";
    pub const JUMP: &'static str = "JUMP";
    pub const JUMPIFTRUE: &'static str = "JUMPIFTRUE";
    pub const JUMPIFVOID: &'static str = "JUMPIFVOID";
    pub const RJUMP: &'static str = "RJUMP";
    pub const RJUMPIFTRUE: &'static str = "RJUMPIFTRUE";
    pub const RJUMPIFVOID: &'static str = "RJUMPIFVOID";
    pub const RETURN: &'static str = "RETURN";
    pub const CALL: &'static str = "CALL";
    pub const PEEK: &'static str = "PEEK";
    pub const PUSHLOCAL: &'static str = "PUSHLOCAL";
    pub const DROPLOCAL: &'static str = "DROPLOCAL";
    pub const FETCHLOCAL: &'static str = "FETCHLOCAL";
    pub const FETCH: &'static str = "FETCH";
    pub const STORE: &'static str = "STORE";
    pub const INTEGER: &'static str = "INTEGER";
    pub const CONST: &'static str = "CONST";
    pub const GETSTACK: &'static str = "GETSTACK";
    pub const DUP: &'static str = "DUP";
    pub const DROP: &'static str = "DROP";
    pub const SWAP: &'static str = "SWAP";
    pub const EQ: &'static str = "EQ";
    pub const INC: &'static str = "INC";
    pub const DEC: &'static str = "DEC";
    pub const CONS: &'static str = "CONS";
    pub const CAR: &'static str = "CAR";
    pub const CDR: &'static str = "CDR";
    pub const TABLE: &'static str = "TABLE";
    pub const TABLESET: &'static str = "TABLESET";
    pub const TABLEGET: &'static str = "TABLEGET";
    pub const MAKECLOSURE: &'static str = "MAKECLOSURE";
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
    pub fn new(code: impl Into<Box<[Op]>>, constants: impl Into<Box<[Value]>>) -> Self {
        CodeSegment {
            code: code.into(),
            constants: constants.into(),
        }
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

    pub fn chain(segments: &[Self]) -> Self {
        let mut code = vec![];
        let mut constants = vec![];

        for segment in segments {
            let constant_offset = constants.len();

            constants.extend(segment.constants.iter().cloned());

            for op in segment.code.iter() {
                match op {
                    Op::Const(idx) => {
                        while let Some(Op::ExtArg(_)) = code.last() {
                            code.pop();
                        }

                        let new_index = *idx as usize + constant_offset;

                        code.extend(Op::extended(Op::Const, new_index));
                    }
                    _ => code.push(*op),
                }
            }
        }

        CodeSegment::new(code, constants)
    }

    pub fn pretty_fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut arg = 0;
        for (i, &op) in self.code.iter().enumerate() {
            write!(f, "  {:>3}  {}", i, op)?;
            match op {
                Op::ExtArg(x) => arg = Op::extend_arg(x, arg),
                Op::Const(x) => {
                    arg = Op::extend_arg(x, arg);
                    write!(f, "  ({})", self.constants[arg])?;
                    arg = 0;
                }
                _ => arg = 0,
            };
            writeln!(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for CodeSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.pretty_fmt(f)
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

    pub fn jump_forward(&mut self, amount: usize) {
        self.position += amount;
    }

    pub fn jump_backward(&mut self, amount: usize) {
        self.position = self.position.checked_sub(amount + 1).unwrap()
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

    pub fn pretty_fmt(&self, n_before: usize, n_after: usize) -> String {
        let mut s = String::new();
        for i in self.position.saturating_sub(n_before)..self.position {
            s += &format!("   {} {}\n", i, self.segment.code_slice()[i]);
        }
        s += &format!(
            "-> {} {} <-\n",
            self.position,
            self.segment.code_slice()[self.position]
        );
        for (i, op) in self.code_slice().iter().enumerate().skip(1).take(n_after) {
            s += &format!("   {} {}\n", i + 1 + self.position, op);
        }
        s
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
                let offset = label_idx as isize - position_reference as isize;
                if offset.abs() > u8::max_value() as isize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                if offset >= 0 {
                    Op::Jump {
                        forward: offset as u8,
                    }
                } else {
                    Op::RJump {
                        backward: -offset as u8,
                    }
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
                let offset = label_idx as isize - position_reference as isize;
                if offset.abs() > u8::max_value() as isize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                if offset >= 0 {
                    Op::JumpIfTrue {
                        forward: offset as u8,
                    }
                } else {
                    Op::RJumpIfTrue {
                        backward: -offset as u8,
                    }
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
                let offset = label_idx as isize - position_reference as isize;
                if offset.abs() > u8::max_value() as isize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                if offset >= 0 {
                    Op::JumpIfVoid {
                        forward: offset as u8,
                    }
                } else {
                    Op::RJumpIfVoid {
                        backward: -offset as u8,
                    }
                }
            }),
        ));

        self
    }

    pub fn make_closure(mut self, label: impl ToString) -> Self {
        let placeholder_pos = self.code.len();
        self = self.op(Op::Nop); // placeholder
        let position_reference = self.code.len();

        self.code[placeholder_pos] = BuildOp::LabelRef(
            label.to_string(),
            Box::new(move |label_idx| {
                let offset = label_idx - position_reference;
                if offset > u8::max_value() as usize {
                    panic!("label placeholders more than 255 away are not yet possible")
                }
                Op::MakeClosure {
                    offset: offset as u8,
                }
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

    pub fn add_constant(&mut self, value: Value) -> usize {
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
    fn chaining_code_segments_appends_their_bytecodes() {
        let first = CodeSegment::new(vec![Op::Integer(1)], vec![]);
        let second = CodeSegment::new(vec![Op::Integer(2)], vec![]);
        let both = CodeSegment::chain(&[first, second]);
        assert_eq!(both.code_slice(), &[Op::Integer(1), Op::Integer(2)]);
    }

    #[test]
    fn chaining_code_segments_relocates_constant_references() {
        let first = CodeSegment::new(vec![Op::Const(0)], vec![Value::Int(1)]);
        let second = CodeSegment::new(vec![Op::Const(0)], vec![Value::Int(2)]);
        let both = CodeSegment::chain(&[first, second]);
        assert_eq!(both.code_slice(), &[Op::Const(0), Op::Const(1)]);
    }

    #[test]
    fn chaining_code_segments_appends_different_constants() {
        let first = CodeSegment::new(vec![Op::Const(0)], vec![Value::Int(1)]);
        let second = CodeSegment::new(vec![Op::Const(0)], vec![Value::Int(2)]);
        let both = CodeSegment::chain(&[first, second]);
        assert_eq!(both.constant_slice(), &[Value::Int(1), Value::Int(2)]);
    }

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
            .make_closure("func")
            .op(Op::Halt)
            .label("func")
            .op(Op::Halt)
            .build()
            .unwrap();
        assert_eq!(
            segment.code_slice(),
            &[Op::MakeClosure { offset: 1 }, Op::Halt, Op::Halt]
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
