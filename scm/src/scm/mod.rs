mod arithmetic;
mod bool;
mod bytevector;
mod char;
mod eof;
mod interner;
mod mutable_pair;
mod null;
pub mod number;
mod pair;
mod sexpr_impls;
mod string;
mod symbol;
mod vector;
mod void;

use crate::scm::interner::Internable;
use std::any::Any;
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
    fn eqv_hash(&self, state: &mut ScmHasher) {
        std::ptr::hash(self, state)
    }
    fn equal_hash(&self, state: &mut ScmHasher);
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

    pub fn int(i: i64) -> Self {
        number::Number::int(i).into()
    }

    pub fn float(x: f64) -> Self {
        number::Number::float(x).into()
    }

    pub fn char(ch: char) -> Self {
        ch.into()
    }

    pub fn number(i: impl Into<number::Number>) -> Self {
        i.into().into()
    }

    pub fn symbol(name: &str) -> Self {
        Scm(symbol::Symbol::interned(name))
    }

    pub fn uninterned_symbol(name: &str) -> Self {
        Scm(symbol::Symbol::uninterned(name))
    }

    pub fn string(content: &str) -> Self {
        Self::raw_string(string::escape(content))
    }

    pub fn raw_string(content: impl Internable<Box<str>>) -> Self {
        Scm(string::ConstantString::interned(content))
    }

    pub fn cons_const(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        pair::Pair::new(car, cdr).into()
    }

    pub fn cons(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        mutable_pair::MutablePair::new(car, cdr).into()
    }

    pub fn list<T: Into<Scm>>(items: impl DoubleEndedIterator<Item = T>) -> Self {
        items.rfold(Self::null(), |acc, x| Self::cons(x, acc))
    }

    pub fn bytevector(data: impl Into<bytevector::ByteVector>) -> Scm {
        Scm::obj(data.into())
    }

    pub fn eof() -> Self {
        EOF.with(Clone::clone)
    }

    pub fn vector(data: impl Into<vector::Vector>) -> Scm {
        Scm::obj(data.into())
    }

    pub fn obj(obj: impl ScmObject) -> Self {
        Scm(Rc::new(obj))
    }

    pub fn as_dyn(&self) -> &dyn ScmObject {
        &*self.0
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

    pub fn as_int(&self) -> Option<i64> {
        self.as_number().and_then(number::Number::as_int).copied()
    }

    pub fn as_number(&self) -> Option<&number::Number> {
        self.as_type::<number::Number>()
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.as_int().map(|i| i as usize)
    }

    pub fn as_symbol(&self) -> Option<&str> {
        self.as_type::<symbol::Symbol>().map(symbol::Symbol::as_str)
    }

    pub fn as_string(&self) -> Option<&str> {
        self.as_type::<string::ConstantString>()
            .map(string::ConstantString::as_str)
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<(&Scm, &Scm)> {
        self.as_type::<mutable_pair::MutablePair>()
            .map(mutable_pair::MutablePair::as_tuple)
            .or_else(|| self.as_type::<pair::Pair>().map(pair::Pair::as_tuple))
    }

    pub fn set_car(&self, value: Scm) -> Option<()> {
        self.as_type::<mutable_pair::MutablePair>()
            .map(|p| p.set_first(value))
    }

    pub fn set_cdr(&self, value: Scm) -> Option<()> {
        self.as_type::<mutable_pair::MutablePair>()
            .map(|p| p.set_second(value))
    }

    pub fn last_cdr(&self) -> &Self {
        self.as_pair()
            .map(|(_, cdr)| cdr.last_cdr())
            .unwrap_or(self)
    }

    pub fn is_bytevector(&self) -> bool {
        self.as_bytevector().is_some()
    }

    pub fn as_bytevector(&self) -> Option<&[u8]> {
        self.as_type::<bytevector::ByteVector>()
            .map(bytevector::ByteVector::as_slice)
    }

    pub fn is_eof(&self) -> bool {
        self.is::<eof::Eof>()
    }

    pub fn is_vector(&self) -> bool {
        self.as_vector().is_some()
    }

    pub fn as_vector(&self) -> Option<&[Scm]> {
        self.as_type::<vector::Vector>()
            .map(vector::Vector::as_slice)
    }

    pub fn as_ref(&self) -> &dyn ScmObject {
        &*self.0
    }

    pub fn as_ptr(&self) -> *const dyn ScmObject {
        Rc::as_ptr(&self.0)
    }

    pub fn is_eq(&self, other: &Scm) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn is_eqv(&self, other: &Scm) -> bool {
        self.is_eq(other) || self.0.is_eqv(&*other.0)
    }

    pub fn is_equal(&self, other: &Scm) -> bool {
        self.is_eq(other) || self.0.is_equal(&*other.0)
    }

    pub fn eq_hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }

    pub fn eqv_hash<H: Hasher>(&self, state: &mut H) {
        // We use an ScmHasher to compute the hash of our value,
        // then hash the result again with the provided hasher.
        // I could not figure out a nicer way...
        let mut hasher = ScmHasher::new();
        self.0.eqv_hash(&mut hasher);
        hasher.finish().hash(state);
    }

    pub fn equal_hash<H: Hasher>(&self, state: &mut H) {
        // We use an ScmHasher to compute the recursive hash of our value,
        // then hash the result again with the provided hasher.
        // I could not figure out a nicer way...
        let mut hasher = ScmHasher::new();
        self.0.equal_hash(&mut hasher);
        hasher.finish().hash(state);
    }

    pub fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Self {
        self.0.substitute(mapping)
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        self.is_equal(other)
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
    static EOF: Scm = eof::Eof.into();
}
