use crate::memory_model::prelude::{str_reference, string_reference, Ref};

#[derive(Clone)]
#[cfg_attr(feature = "scm_copy", derive(Copy))]
pub struct ScmString(Ref<str>);

impl ScmString {
    pub fn from_static(s: &'static str) -> Self {
        ScmString(str_reference(s))
    }

    pub fn from_string(s: String) -> Self {
        ScmString(string_reference(s))
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
