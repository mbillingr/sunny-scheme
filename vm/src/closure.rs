use crate::opcode::Op;
use crate::Value;
use sunny_memory::gc::{GarbageCollector, Traceable};

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) code: Box<[Op]>,
    pub(crate) constants: Box<[Value]>,
    pub(crate) free_vars: Box<[Value]>,
}

impl Traceable for Closure {
    fn trace(&self, gc: &mut GarbageCollector) {
        self.constants.trace(gc);
        self.free_vars.trace(gc);
    }
}

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

    pub fn build(self) -> Closure {
        Closure {
            code: self.code.into_boxed_slice(),
            constants: self.constants.into_boxed_slice(),
            free_vars: vec![].into_boxed_slice(),
        }
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
        let cls = ClosureBuilder::new().build();
        assert_eq!(
            cls,
            Closure {
                code: vec![].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_append_opcode() {
        let cls = ClosureBuilder::new().op(Op::Halt).build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Halt].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_with_zero_arg() {
        let cls = ClosureBuilder::new().with(Op::Const, 0).build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Const(0)].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_with_max_arg_value() {
        let cls = ClosureBuilder::new().with(Op::Const, 255).build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Const(255)].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_with_smallest_extension_value() {
        let cls = ClosureBuilder::new().with(Op::Const, 256).build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::ExtArg(1), Op::Const(0)].into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_with_large_extension_value() {
        let cls = ClosureBuilder::new()
            .with(Op::Const, 0x0123_4567_89ab_cdef)
            .build();
        assert_eq!(
            cls,
            Closure {
                code: vec![
                    Op::ExtArg(0x01),
                    Op::ExtArg(0x23),
                    Op::ExtArg(0x45),
                    Op::ExtArg(0x67),
                    Op::ExtArg(0x89),
                    Op::ExtArg(0xab),
                    Op::ExtArg(0xcd),
                    Op::Const(0xef)
                ]
                .into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_with_largest_extension_value() {
        let cls = ClosureBuilder::new()
            .with(Op::Const, 0xffff_ffff_ffff_ffff)
            .build();
        assert_eq!(
            cls,
            Closure {
                code: vec![
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::ExtArg(255),
                    Op::Const(255)
                ]
                .into_boxed_slice(),
                constants: vec![].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_constant() {
        let cls = ClosureBuilder::new().constant(Value::Nil).build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Const(0)].into_boxed_slice(),
                constants: vec![Value::Nil].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_different_constants() {
        let cls = ClosureBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Const(0), Op::Const(1)].into_boxed_slice(),
                constants: vec![Value::Int(1), Value::Int(2)].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }

    #[test]
    fn build_reuse_same_constants() {
        let cls = ClosureBuilder::new()
            .constant(Value::Int(1))
            .constant(Value::Int(2))
            .constant(Value::Int(1))
            .build();
        assert_eq!(
            cls,
            Closure {
                code: vec![Op::Const(0), Op::Const(1), Op::Const(0)].into_boxed_slice(),
                constants: vec![Value::Int(1), Value::Int(2)].into_boxed_slice(),
                free_vars: vec![].into_boxed_slice(),
            }
        )
    }
}
