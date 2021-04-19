use crate::activation::Activation;
use crate::mem::Ref;
use std::any::Any;
use std::collections::HashMap;
use std::hash::Hash;
use sunny_scm::{Scm, ScmHasher, ScmObject};

pub struct Continuation {
    pub(crate) activation: Ref<Activation>,
    pub(crate) value_stack: Vec<Scm>,
}

impl std::fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<continuation {:p}> {:?}", self, self.activation.code)
    }
}

impl std::fmt::Display for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}>", self)
    }
}

impl Continuation {
    pub fn arity(&self) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}

impl ScmObject for Continuation {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.activation == other.activation)
            .unwrap_or(false)
    }

    fn value_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state);
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.activation.hash(state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}
