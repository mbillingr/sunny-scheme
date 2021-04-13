mod bool;
mod int;
mod interner;
mod null;
mod pair;
mod string;
mod symbol;

use crate::cxr::CxR;
use crate::Int;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub trait ScmObject: Any + Debug + Display {
    fn as_any(&self) -> &dyn Any;
    fn eq(&self, other: &dyn ScmObject) -> bool;
    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm;
}

impl dyn ScmObject {
    #[inline]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Scm(Rc<dyn ScmObject>);

impl Scm {
    pub fn null() -> Self {
        null::Null.into()
    }

    pub fn bool(b: bool) -> Self {
        bool::Bool::new(b).into()
    }

    pub fn int(i: Int) -> Self {
        int::Int::new(i).into()
    }

    pub fn symbol(name: &str) -> Self {
        Scm(symbol::Symbol::interned(name))
    }

    pub fn string(name: &str) -> Self {
        Scm(string::String::interned(name))
    }

    pub fn cons(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        pair::Pair::new(car, cdr).into()
    }

    pub fn list(items: impl DoubleEndedIterator<Item = Scm>) -> Self {
        items.rfold(Self::null(), |acc, x| Self::cons(x, acc))
    }

    pub fn obj(obj: impl ScmObject) -> Self {
        Scm(Rc::new(obj))
    }

    pub fn is<T: 'static>(&self) -> bool {
        self.as_type::<T>().is_some()
    }

    pub fn as_type<T: 'static>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }

    pub fn is_null(&self) -> bool {
        self.is::<null::Null>()
    }

    pub fn as_bool(&self) -> Option<bool> {
        self.as_type::<bool::Bool>().map(bool::Bool::as_bool)
    }

    pub fn as_int(&self) -> Option<Int> {
        self.as_type::<int::Int>().map(int::Int::as_int)
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.as_int().map(|i| i as usize)
    }

    pub fn as_symbol(&self) -> Option<&str> {
        self.as_type::<symbol::Symbol>().map(symbol::Symbol::as_str)
    }

    pub fn as_string(&self) -> Option<&str> {
        self.as_type::<string::String>().map(string::String::as_str)
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<(&Scm, &Scm)> {
        self.as_type::<pair::Pair>().map(pair::Pair::as_tuple)
    }

    pub fn last_cdr(&self) -> &Self {
        self.as_pair()
            .map(|(_, cdr)| cdr.last_cdr())
            .unwrap_or(self)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scm> {
        let mut cursor = Some(self);
        (0..)
            .map(move |_| {
                if cursor.is_none() {
                    None
                } else if cursor.unwrap().is_null() {
                    None
                } else if let Some(pair) = cursor.unwrap().as_pair() {
                    cursor = Some(pair.1);
                    Some(pair.0)
                } else {
                    cursor.take()
                }
            })
            .take_while(|x| x.is_some())
            .map(Option::unwrap)
    }

    pub fn list_length(&self) -> usize {
        if self.is_pair() {
            1 + self.cdr().unwrap().list_length()
        } else {
            0
        }
    }

    pub fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Self {
        self.0.substitute(mapping)
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0)) || &self.0 == &other.0
    }
}

impl PartialEq for dyn ScmObject {
    fn eq(&self, other: &Self) -> bool {
        ScmObject::eq(self, other)
    }
}

impl<T: ScmObject> From<T> for Scm {
    fn from(obj: T) -> Self {
        Scm(Rc::new(obj))
    }
}

impl Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl CxR for Scm {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.as_pair().map(|(car, _)| car)
    }

    fn cdr(&self) -> Option<&Self> {
        self.as_pair().map(|(_, cdr)| cdr)
    }
}

impl From<i64> for Scm {
    fn from(i: i64) -> Self {
        Scm::int(i)
    }
}

impl<'s> From<&'s str> for Scm {
    fn from(s: &'s str) -> Self {
        Scm::symbol(s)
    }
}

impl Eq for Scm {}

impl Hash for Scm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}
