use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Hash)]
pub struct Eof;

impl ScmObject for Eof {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other.downcast_ref::<Self>().is_some()
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        Scm::eof()
    }
}

impl Display for Eof {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "<eof>")
    }
}
