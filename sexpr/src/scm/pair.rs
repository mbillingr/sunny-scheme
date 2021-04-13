use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone)]
pub struct Pair {
    first: Scm,
    second: Scm,
}

impl Pair {
    pub fn new(first: impl Into<Scm>, second: impl Into<Scm>) -> Self {
        Pair {
            first: first.into(),
            second: second.into(),
        }
    }

    pub fn as_tuple(&self) -> (&Scm, &Scm) {
        (&self.first, &self.second)
    }
}

impl ScmObject for Pair {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn equals(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.first == other.first && self.second == other.second)
            .unwrap_or(false)
    }

    fn deep_hash(&self, state: &mut ScmHasher) {
        self.first.deep_hash(state);
        self.second.deep_hash(state);
    }

    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm {
        Scm::cons(
            self.first.substitute(mapping),
            self.second.substitute(mapping),
        )
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if !f.alternate() {
            write!(f, "({:#})", self)
        } else {
            write!(f, "{}", self.first)?;
            if self.second.is_null() {
                Ok(())
            } else if self.second.is_pair() {
                write!(f, " {:#}", self.second)
            } else {
                write!(f, " . {}", self.second)
            }
        }
    }
}
