use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Copy, Clone)]
pub struct Int(i64);

impl Int {
    pub fn new(x: i64) -> Self {
        Int(x)
    }

    pub fn as_int(&self) -> i64 {
        self.0
    }
}

impl ScmObject for Int {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.0.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        (*self).into()
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}
