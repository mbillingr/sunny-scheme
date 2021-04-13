use crate::activation::Activation;
use crate::mem::Ref;
use sunny_sexpr_parser::Scm;

pub struct Continuation {
    pub(crate) activation: Ref<Activation>,
    pub(crate) value_stack: Vec<Scm>,
}

impl std::fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<continuation @ {:p}> {:?}", self, self.activation.code)
    }
}

impl Continuation {
    pub fn arity(&self) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}
