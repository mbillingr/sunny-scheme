use crate::activation::Activation;
use crate::mem::Ref;
use crate::Value;

pub struct Continuation {
    pub(crate) activation: Ref<Activation>,
    pub(crate) value_stack: Vec<Value>,
}

impl std::fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<continuation @ {:p}> {:?}", self, self.activation.code)
    }
}
