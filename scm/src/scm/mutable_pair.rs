use super::pair;
use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone)]
pub struct MutablePair {
    first: Scm,
    second: Scm,
}

impl MutablePair {
    pub fn new(first: impl Into<Scm>, second: impl Into<Scm>) -> Self {
        MutablePair {
            first: first.into(),
            second: second.into(),
        }
    }

    pub fn as_tuple(&self) -> (&Scm, &Scm) {
        (&self.first, &self.second)
    }

    pub fn set_first(&self, value: Scm) {
        unsafe {
            let mut_ptr = self as *const Self as *mut Self;
            (*mut_ptr).first = value;
        }
    }

    pub fn set_second(&self, value: Scm) {
        unsafe {
            let mut_ptr = self as *const Self as *mut Self;
            (*mut_ptr).second = value;
        }
    }
}

impl ScmObject for MutablePair {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.first == other.first && self.second == other.second)
            .unwrap_or(false)
            || other
                .downcast_ref::<pair::Pair>()
                .map(|p| p.as_tuple())
                .map(|(car, cdr)| &self.first == car && &self.second == cdr)
                .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state);
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.first.equal_hash(state);
        self.second.equal_hash(state);
    }

    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm {
        Scm::cons(
            self.first.substitute(mapping),
            self.second.substitute(mapping),
        )
    }
}

impl Display for MutablePair {
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
