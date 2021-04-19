use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Copy, Clone, Hash)]
pub struct Bool(bool);

impl Bool {
    pub fn new(b: bool) -> Self {
        Bool(b)
    }

    pub fn as_bool(&self) -> bool {
        self.0
    }
}

impl ScmObject for Bool {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        (*self).into()
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.0 {
            write!(f, "#t")
        } else {
            write!(f, "#f")
        }
    }
}
