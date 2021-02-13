use crate::activation::Activation;
use crate::bytecode::CodePointer;
use crate::mem::{Ref, Traceable, Tracer};

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
}

impl Traceable for Closure {
    fn trace(&self, gc: &mut Tracer) {
        self.code.trace(gc);
        self.parent.trace(gc);
    }
}
