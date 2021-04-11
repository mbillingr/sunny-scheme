use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug)]
pub struct Null;

impl ScmObject for Null {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn ScmObject) -> bool {
        other.downcast_ref::<Self>().is_some()
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
