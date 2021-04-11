use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::{Add, Deref};
use std::rc::Rc;

#[derive(Clone)]
pub struct SharedStr {
    string: Rc<str>,
}

impl SharedStr {
    pub fn new() -> Self {
        SharedStr {
            string: String::new().into(),
        }
    }
}

impl Default for SharedStr {
    fn default() -> Self {
        Self::new()
    }
}

impl From<&str> for SharedStr {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl From<&String> for SharedStr {
    fn from(s: &String) -> Self {
        s.clone().into()
    }
}

impl From<String> for SharedStr {
    fn from(s: String) -> Self {
        SharedStr { string: s.into() }
    }
}

impl Deref for SharedStr {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl PartialEq for SharedStr {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.string), Rc::as_ptr(&other.string))
            || self.string == other.string
    }
}

impl Debug for SharedStr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Debug::fmt(&self.string, f)
    }
}

impl Display for SharedStr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self.string, f)
    }
}

impl<T: Deref<Target = str>> Add<T> for SharedStr {
    type Output = SharedStr;
    fn add(self, rhs: T) -> SharedStr {
        SharedStr::from(format!("{}{}", self.string, rhs.deref()))
    }
}
