use crate::activation::Activation;
use crate::mem::{Ref, Traceable, Tracer};
use crate::Value;

pub struct Continuation {
    pub(crate) activation: Ref<Activation>,
    pub(crate) value_stack: Vec<Value>,
}

impl Traceable for Continuation {
    fn trace(&self, gc: &mut Tracer) {
        self.activation.trace(gc);
        self.value_stack.trace(gc);
    }
}

impl std::fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<continuation @ {:p}>", self)
    }
}
