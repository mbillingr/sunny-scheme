use crate::bytecode::CodePointer;
use crate::mem::{Tracer, Ref, Traceable};
use crate::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) code: CodePointer,
    pub(crate) free_vars: Ref<Box<[Value]>>,
}

impl Traceable for Closure {
    fn trace(&self, gc: &mut Tracer) {
        self.code.trace(gc);
        self.free_vars.trace(gc);
    }
}
