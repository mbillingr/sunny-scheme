use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Hash)]
pub struct Null;

impl ScmObject for Null {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn ScmObject) -> bool {
        other.downcast_ref::<Self>().is_some()
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        Scm::null()
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "()")
    }
}
