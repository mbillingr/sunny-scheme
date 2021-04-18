mod arithmetic;
mod bool;
mod int;
mod interner;
mod null;
mod pair;
mod sexpr_impls;
mod string;
mod symbol;
mod void;

use crate::Int;
use sexpr_generics::cxr::CxR;
use std::any::Any;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};

pub type ScmHasher = std::collections::hash_map::DefaultHasher;

pub trait ScmObject: Any + Debug + Display {
    fn as_any(&self) -> &dyn Any;
    fn is_eqv(&self, other: &dyn ScmObject) -> bool;
    fn is_equal(&self, other: &dyn ScmObject) -> bool {
        self.is_eqv(other)
    }
    fn deep_hash(&self, state: &mut ScmHasher);
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

impl Default for Scm {
    fn default() -> Self {
        Scm::void()
    }
}

impl Scm {
    pub fn void() -> Self {
        void::Void.into()
    }

    pub fn null() -> Self {
        NULL.with(Clone::clone)
    }

    pub fn bool(b: bool) -> Self {
        if b {
            TRUE.with(Clone::clone)
        } else {
            FALSE.with(Clone::clone)
        }
    }

    pub fn int(i: Int) -> Self {
        int::Int::new(i).into()
    }

    pub fn number(i: impl Into<Int>) -> Self {
        int::Int::new(i.into()).into()
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

    pub fn list<T: Into<Scm>>(items: impl DoubleEndedIterator<Item = T>) -> Self {
        items.rfold(Self::null(), |acc, x| Self::cons(x, acc))
    }

    pub fn obj(obj: impl ScmObject) -> Self {
        Scm(Rc::new(obj))
    }

    pub fn is_like_true(&self) -> bool {
        self.as_bool().unwrap_or(true) && !self.is_void()
    }

    pub fn is<T: 'static>(&self) -> bool {
        self.as_type::<T>().is_some()
    }

    pub fn as_type<T: 'static>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }

    pub fn is_void(&self) -> bool {
        self.is::<void::Void>()
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

    pub fn ptr_eq(&self, other: &Scm) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn is_eqv(&self, other: &Scm) -> bool {
        self.ptr_eq(other) || self.0.is_eqv(&*other.0)
    }

    pub fn is_equal(&self, other: &Scm) -> bool {
        self.ptr_eq(other) || self.0.is_equal(&*other.0)
    }

    pub fn deep_hash(&self, state: &mut ScmHasher) {
        self.0.deep_hash(state)
    }

    pub fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Self {
        self.0.substitute(mapping)
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0)) || self.is_equal(other)
    }
}

impl PartialEq for dyn ScmObject {
    fn eq(&self, other: &Self) -> bool {
        ScmObject::is_equal(self, other)
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

impl From<i32> for Scm {
    fn from(i: i32) -> Self {
        Scm::int(i as i64)
    }
}

impl From<i64> for Scm {
    fn from(i: i64) -> Self {
        Scm::int(i)
    }
}

impl From<usize> for Scm {
    fn from(i: usize) -> Self {
        Scm::int(i as i64)
    }
}

impl<'s> From<&'s str> for Scm {
    fn from(s: &'s str) -> Self {
        Scm::symbol(s)
    }
}

impl Eq for Scm {}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct HashPtrEq(Scm);

impl HashPtrEq {
    pub fn from_ref(scm: &Scm) -> &Self {
        unsafe {
            // # Safety : safe if Self transparently wraps Scm
            std::mem::transmute(scm)
        }
    }

    pub fn into_scm(self) -> Scm {
        self.0
    }
}

impl Hash for HashPtrEq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0 .0).hash(state)
    }
}

impl Eq for HashPtrEq {}

impl PartialEq for HashPtrEq {
    fn eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }
}

impl From<Scm> for HashPtrEq {
    fn from(scm: Scm) -> Self {
        HashPtrEq(scm)
    }
}

impl Borrow<Scm> for HashPtrEq {
    fn borrow(&self) -> &Scm {
        &self.0
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct HashEqual(Scm);

impl HashEqual {
    pub fn from_ref(scm: &Scm) -> &Self {
        unsafe {
            // # Safety : safe if Self transparently wraps Scm
            std::mem::transmute(scm)
        }
    }

    pub fn into_scm(self) -> Scm {
        self.0
    }
}

impl Hash for HashEqual {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We use an ScmHasher to compute the recursive hash of our value,
        // then hash the result again with the provided hasher.
        // I could not figure out a nicer way...
        let mut hasher = ScmHasher::new();
        self.0 .0.deep_hash(&mut hasher);
        hasher.finish().hash(state);
    }
}

impl Eq for HashEqual {}

impl PartialEq for HashEqual {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_equal(&other.0)
    }
}

impl From<Scm> for HashEqual {
    fn from(scm: Scm) -> Self {
        HashEqual(scm)
    }
}

impl Borrow<Scm> for HashEqual {
    fn borrow(&self) -> &Scm {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct WeakScm(Weak<dyn ScmObject>);

impl Scm {
    pub fn downgrade(&self) -> WeakScm {
        WeakScm(Rc::downgrade(&self.0))
    }
}

impl WeakScm {
    pub fn upgrade(&self) -> Option<Scm> {
        self.0.upgrade().map(Scm)
    }

    pub fn is_dead(&self) -> bool {
        self.upgrade().is_none()
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl Hash for WeakScm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Weak::as_ptr(&self.0).hash(state)
    }
}

impl Eq for WeakScm {}

impl PartialEq for WeakScm {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}

// Special values that must always refer to the same object
thread_local! {
    static NULL: Scm = null::Null.into();
    static TRUE: Scm = bool::Bool::new(true).into();
    static FALSE: Scm = bool::Bool::new(false).into();
}
