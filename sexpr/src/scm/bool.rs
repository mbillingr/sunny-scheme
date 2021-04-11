use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Copy, Clone)]
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

    fn eq(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
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
