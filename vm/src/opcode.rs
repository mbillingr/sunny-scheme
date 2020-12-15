use crate::mem::{GarbageCollector, Traceable};

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
}

impl Traceable for Op {
    fn trace(&self, _: &mut GarbageCollector) {}
}
