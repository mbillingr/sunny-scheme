use crate::bytecode::CodePointer;
use crate::mem::{Ref, Traceable, Tracer};
use crate::Value;

#[derive(Debug)]
pub struct Activation {
    pub(crate) caller: Option<Ref<Activation>>,
    pub(crate) parent: Option<Ref<Activation>>,
    pub(crate) code: CodePointer,
    pub(crate) locals: Vec<Value>,
}

impl Traceable for Activation {
    fn trace(&self, gc: &mut Tracer) {
        self.caller.trace(gc);
        self.parent.trace(gc);
        self.code.trace(gc);
        self.locals.trace(gc);
    }
}
