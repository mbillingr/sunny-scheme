use crate::scm::interner::{interned_string, Strong};
use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Clone, Hash)]
#[repr(transparent)]
pub struct ConstantString(Box<str>);

impl ConstantString {
    pub fn interned(name: &str) -> Strong<ConstantString> {
        let string = interned_string(name);
        unsafe {
            // # Safety: converting to a repr(transparent) wrapper
            // type is safe.
            std::mem::transmute(string)
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl ScmObject for ConstantString {
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
        // TODO: I'm sure this is very inefficient.
        //       It looks up the string in the interner...
        //       Would be better if we had the original Scm available to return.
        //       Actually, substitution should probably be not a member of ScmObject anyway.
        Scm::string(&self.0)
    }
}

impl Display for ConstantString {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}
