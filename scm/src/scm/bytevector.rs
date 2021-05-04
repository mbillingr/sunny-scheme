use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Debug)]
pub struct ByteVector(Box<[u8]>);

impl<T: Into<Box<[u8]>>> From<T> for ByteVector {
    fn from(data: T) -> Self {
        ByteVector(data.into())
    }
}

impl ScmObject for ByteVector {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.0.hash(state);
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}

impl fmt::Display for ByteVector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#u8(")?;
        match &*self.0 {
            [] => {}
            [x] => {
                write!(f, "{}", x)?;
            }
            [x, more @ ..] => {
                write!(f, "{}", x)?;
                for y in more {
                    write!(f, " {}", y)?;
                }
            }
        }
        write!(f, ")")
    }
}

impl ByteVector {
    pub fn as_slice(&self) -> &[u8] {
        &*self.0
    }
}
