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

    MakeClosure(u8),
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
