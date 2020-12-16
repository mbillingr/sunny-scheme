use crate::mem::{GarbageCollector, Ref, Traceable};
use crate::Value;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u16)]
pub enum Op {
    Nop,
    ExtArg(u8),

    Halt,
    Return,

    Integer(u8),
    Const(u8),

    Cons,

    MakeClosure { n_free: u8 },
}

impl Traceable for Op {
    fn trace(&self, _: &mut GarbageCollector) {}
}

#[derive(Debug, Clone)]
pub struct CodeSegment {
    code: Box<[Op]>,
    constants: Box<[Value]>,
}

impl Traceable for CodeSegment {
    fn trace(&self, gc: &mut GarbageCollector) {
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
    fn trace(&self, gc: &mut GarbageCollector) {
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
    code: Vec<Op>,
    constants: Vec<Value>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        CodeBuilder {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn build(self) -> Result<CodeSegment, Self> {
        let bytecode = self.code.into_boxed_slice();
        let constants = self.constants.into_boxed_slice();
        let segment = CodeSegment::new(bytecode, constants);
        Ok(segment)
    }

    pub fn constant(mut self, value: Value) -> Self {
        let idx = self.add_constant(value);
        self.with(Op::Const, idx)
    }

    pub fn with(mut self, op: impl Fn(u8) -> Op, mut arg: usize) -> Self {
        if arg <= u8::max_value() as usize {
            self.code.push(op(arg as u8));
            return self;
        }

        let mut reverse_args = vec![];

        while arg > 0 {
            reverse_args.push((arg & 0xff) as u8);
            arg = arg >> 8;
        }

        while reverse_args.len() > 1 {
            let ext = reverse_args.pop().unwrap();
            self.code.push(Op::ExtArg(ext as u8));
        }

        let arg = reverse_args.pop().unwrap();
        self.code.push(op(arg as u8));

        self
    }

    pub fn op(mut self, op: Op) -> Self {
        self.code.push(op);
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
}
