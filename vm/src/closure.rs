use crate::code::CodePointer;
use crate::mem::{GarbageCollector, Ref, Traceable};
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
