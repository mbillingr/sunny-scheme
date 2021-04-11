mod null;
use crate::cxr::CxR;
use crate::Int;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Scm(Rc<dyn ScmObject>);

impl Scm {
    pub fn null() -> Self {
        null::Null.into()
    }

    pub fn bool(b: bool) -> Self {
        Sexpr::bool(b).into()
    }

    pub fn int(i: Int) -> Self {
        Sexpr::int(i).into()
    }

    pub fn symbol(name: &str) -> Self {
        Sexpr::symbol(name).into()
    }

    pub fn string(name: &str) -> Self {
        Sexpr::string(name).into()
    }

    pub fn cons(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        Sexpr::cons(car, cdr).into()
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
        self.0.downcast_ref::<Sexpr>().and_then(Sexpr::as_bool)
    }

    pub fn as_int(&self) -> Option<Int> {
        self.0.downcast_ref::<Sexpr>().and_then(Sexpr::as_int)
    }

    pub fn as_usize(&self) -> Option<usize> {
        self.as_int().map(|i| i as usize)
    }

    pub fn as_symbol(&self) -> Option<&str> {
        self.0.downcast_ref::<Sexpr>().and_then(Sexpr::as_symbol)
    }

    pub fn as_string(&self) -> Option<&str> {
        self.0.downcast_ref::<Sexpr>().and_then(Sexpr::as_string)
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<(&Scm, &Scm)> {
        self.0.downcast_ref::<Sexpr>().and_then(Sexpr::as_pair)
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

impl PartialEq for dyn ScmObject {
    fn eq(&self, other: &Self) -> bool {
        ScmObject::eq(self, other)
    }
}

pub trait SexprObject: Any + Display + Debug {
    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Rc<dyn AnySexprObject>;
}

pub trait AnySexprObject: Any + SexprObject {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any + SexprObject> AnySexprObject for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub enum Sexpr {
    Bool(bool),
    Integer(Int),
    Symbol(String),
    String(String),
    Pair((Scm, Scm)),
    Object(Scm),
}

impl<T: ScmObject> From<T> for Scm {
    fn from(obj: T) -> Self {
        Scm(Rc::new(obj))
    }
}

impl ScmObject for Sexpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eq(&self, other: &dyn ScmObject) -> bool {
        if let Some(other) = other.downcast_ref() {
            self == other
        } else {
            false
        }
    }

    fn substitute(&self, mapping: &HashMap<&str, Scm>) -> Scm {
        match self {
            Sexpr::Bool(_) | Sexpr::Integer(_) | Sexpr::String(_) => self.clone().into(),
            Sexpr::Pair(p) => Scm::cons(p.0.substitute(mapping), p.1.substitute(mapping)),
            Sexpr::Symbol(s) => {
                if let Some(replacement) = mapping.get(s.as_str()) {
                    replacement.clone()
                } else {
                    self.clone().into()
                }
            }
            Sexpr::Object(obj) => Sexpr::Object(obj.substitute(mapping)).into(),
        }
    }
}

impl PartialEq for Sexpr {
    fn eq(&self, other: &Self) -> bool {
        use Sexpr::*;
        match (self, other) {
            (Bool(a), Bool(b)) => a == b,
            (Integer(a), Integer(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            (Object(a), Object(b)) => a == b,
            _ => false,
        }
    }
}

impl Sexpr {
    pub fn bool(b: bool) -> Self {
        Self::Bool(b)
    }

    pub fn int(i: Int) -> Self {
        Sexpr::Integer(i)
    }

    pub fn symbol(name: &str) -> Self {
        Sexpr::Symbol(name.to_string())
    }

    pub fn string(name: &str) -> Self {
        Sexpr::String(name.to_string())
    }

    pub fn cons(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        Sexpr::Pair((car.into(), cdr.into()))
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Sexpr::Pair(_))
    }

    pub fn as_pair(&self) -> Option<(&Scm, &Scm)> {
        match self {
            Sexpr::Pair(x) => Some((&x.0, &x.1)),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Sexpr::Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<Int> {
        match self {
            Sexpr::Integer(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            Sexpr::Integer(x) if *x >= 0 => Some(*x as usize),
            _ => None,
        }
    }

    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_some()
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Sexpr::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Sexpr::String(s) => Some(s),
            _ => None,
        }
    }
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Bool(true) => write!(f, "#t"),
            Sexpr::Bool(false) => write!(f, "#f"),
            Sexpr::Integer(i) => write!(f, "{}", i),
            Sexpr::Symbol(s) => write!(f, "{}", s),
            Sexpr::String(s) => write!(f, "{:?}", s),
            Sexpr::Pair(p) => {
                if !f.alternate() {
                    write!(f, "({:#})", self)
                } else {
                    write!(f, "{}", p.0)?;
                    if p.1.is_null() {
                        Ok(())
                    } else if p.1.is_pair() {
                        write!(f, " {:#}", p.1)
                    } else {
                        write!(f, " . {}", p.1)
                    }
                }
            }
            Sexpr::Object(obj) => write!(f, "{}", obj),
        }
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
