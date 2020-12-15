use crate::mem::{GarbageCollector, Ref, Traceable};
use crate::opcode::Op;
use crate::storage::ValueStorage;
use crate::vm::{CodePointer, CodeSegment};
use crate::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) code: CodePointer,
    pub(crate) free_vars: Ref<Box<[Value]>>,
}

impl Traceable for Closure {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.code.trace(gc);
        self.free_vars.trace(gc);
    }
}

#[derive(Debug)]
pub struct ClosureBuilder {
    code: Vec<Op>,
    constants: Vec<Value>,
}

impl ClosureBuilder {
    pub fn new() -> Self {
        ClosureBuilder {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn build(self, storage: &mut ValueStorage) -> Result<Closure, Self> {
        if storage.free() < 2 {
            return Err(self);
        }

        let bytecode = self.code.into_boxed_slice();
        let constants = self.constants.into_boxed_slice();
        let segment = storage
            .insert(CodeSegment::new(bytecode, constants))
            .unwrap();
        let code = CodePointer::new(segment);

        let free_vars = storage.insert(vec![].into_boxed_slice()).unwrap();
        Ok(Closure { code, free_vars })
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
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new().build(&mut storage).unwrap();
        assert_eq!(cls.code.code_slice(), &[]);
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_append_opcode() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .op(Op::Halt)
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::Halt]);
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_with_zero_arg() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .with(Op::Const, 0)
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::Const(0)]);
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_with_max_arg_value() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .with(Op::Const, 255)
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::Const(255)]);
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_with_smallest_extension_value() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .with(Op::Const, 256)
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::ExtArg(1), Op::Const(0)]);
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_with_large_extension_value() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .with(Op::Const, 0x0123_4567_89ab_cdef)
            .build(&mut storage)
            .unwrap();
        assert_eq!(
            cls.code.code_slice(),
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
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_with_largest_extension_value() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .with(Op::Const, 0xffff_ffff_ffff_ffff)
            .build(&mut storage)
            .unwrap();
        assert_eq!(
            cls.code.code_slice(),
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
        assert!(cls.code.constant_slice().is_empty());
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_constant() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .constant(Value::Nil)
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::Const(0)]);
        assert_eq!(cls.code.constant_slice(), &[Value::Nil]);
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_different_constants() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .build(&mut storage)
            .unwrap();
        assert_eq!(cls.code.code_slice(), &[Op::Const(0), Op::Const(1)]);
        assert_eq!(cls.code.constant_slice(), &[Value::Int(1), Value::Int(2)]);
        assert!(cls.free_vars.is_empty());
    }

    #[test]
    fn build_reuse_same_constants() {
        let mut storage = ValueStorage::new(1024);
        let cls = ClosureBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .constant(Value::Int(1))
            .build(&mut storage)
            .unwrap();
        assert_eq!(
            cls.code.code_slice(),
            &[Op::Const(0), Op::Const(1), Op::Const(0)]
        );
        assert_eq!(cls.code.constant_slice(), &[Value::Int(1), Value::Int(2)]);
        assert!(cls.free_vars.is_empty());
    }
}
