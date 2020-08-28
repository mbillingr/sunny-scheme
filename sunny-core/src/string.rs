use crate::memory_model::prelude::{ref_as_ptr, str_reference, string_reference, Ref};

#[derive(Clone, PartialEq)]
#[cfg_attr(feature = "scm_copy", derive(Copy))]
pub struct ScmString(Ref<str>);

impl ScmString {
    pub fn from_static(s: &'static str) -> Self {
        ScmString(str_reference(s))
    }

    pub fn from_string(s: impl Into<Box<str>>) -> Self {
        ScmString(string_reference(s))
    }

    pub fn is_ptreq(&self, other: &Self) -> bool {
        ref_as_ptr(&self.0) == ref_as_ptr(&other.0)
    }

    pub fn as_str(&self) -> &str {
        &*self.0
    }
}

impl std::fmt::Debug for ScmString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::fmt::Display for ScmString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
