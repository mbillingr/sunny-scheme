use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub struct Vector(Box<[Scm]>);

impl<T: Into<Box<[Scm]>>> From<T> for Vector {
    fn from(data: T) -> Self {
        Vector(data.into())
    }
}

impl ScmObject for Vector {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, _other: &dyn ScmObject) -> bool {
        false
    }

    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .filter(|other| self.0.len() == other.0.len())
            .map(|other| self.0.iter().zip(&*other.0).all(|(a, b)| a.is_equal(b)))
            .unwrap_or(false)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        for x in &*self.0 {
            x.equal_hash(state);
        }
    }

    fn substitute(&self, _mapping: &HashMap<&str, Scm>) -> Scm {
        unimplemented!()
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#(")?;
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

impl Vector {
    pub fn as_slice(&self) -> &[Scm] {
        &*self.0
    }
}
