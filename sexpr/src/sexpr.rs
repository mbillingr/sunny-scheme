use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::cxr::CxR;
use crate::Int;
use std::any::Any;
use std::hash::{Hash, Hasher};

pub trait SexprObject: Any + Display + Debug {
    fn substitute(&self, mapping: &HashMap<&str, Sexpr>) -> Rc<dyn AnySexprObject>;
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
    Nil,
    Bool(bool),
    Integer(Int),
    Symbol(String),
    String(String),
    Pair(Rc<(Sexpr, Sexpr)>),
    Object(Rc<dyn AnySexprObject>),
}

impl PartialEq for Sexpr {
    fn eq(&self, other: &Self) -> bool {
        use Sexpr::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Integer(a), Integer(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            (Object(a), Object(b)) => {
                std::ptr::eq(Rc::as_ptr(a) as *const u8, Rc::as_ptr(b) as *const u8)
            }
            _ => false,
        }
    }
}

impl Sexpr {
    pub fn nil() -> Self {
        Sexpr::Nil
    }

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

    pub fn cons(car: impl Into<Sexpr>, cdr: impl Into<Sexpr>) -> Self {
        Sexpr::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn list(items: impl DoubleEndedIterator<Item = Sexpr>) -> Self {
        items.rfold(Self::nil(), |acc, x| Self::cons(x, acc))
    }

    pub fn obj(obj: impl SexprObject) -> Self {
        Sexpr::Object(Rc::new(obj))
    }

    pub fn is_null(&self) -> bool {
        self == &Sexpr::Nil
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, Sexpr::Pair(_))
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

    pub fn is_an_object(&self) -> bool {
        matches!(self, Sexpr::Object(_))
    }

    pub fn is_object<T: Any>(&self) -> bool {
        self.as_object::<T>().is_some()
    }

    pub fn as_object<T: Any>(&self) -> Option<&T> {
        match self {
            Sexpr::Object(obj) => obj.as_any().downcast_ref(),
            _ => None,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Sexpr> {
        let mut cursor = self;
        (0..)
            .map(move |_| match cursor {
                Sexpr::Pair(pair) => {
                    cursor = &pair.1;
                    &pair.0
                }
                _ => std::mem::replace(&mut cursor, &Sexpr::Nil),
            })
            .take_while(|x| x != &&Sexpr::Nil)
    }

    pub fn list_length(&self) -> usize {
        if self.is_pair() {
            1 + self.cdr().unwrap().list_length()
        } else {
            0
        }
    }

    pub fn last_cdr(&self) -> &Sexpr {
        match self {
            Sexpr::Pair(p) => p.1.last_cdr(),
            _ => self,
        }
    }

    pub fn substitute(template: &Self, mapping: &HashMap<&str, Sexpr>) -> Self {
        match template {
            Sexpr::Nil | Sexpr::Bool(_) | Sexpr::Integer(_) | Sexpr::String(_) => template.clone(),
            Sexpr::Pair(p) => Sexpr::Pair(Rc::new((
                Sexpr::substitute(&p.0, mapping),
                Sexpr::substitute(&p.1, mapping),
            ))),
            Sexpr::Symbol(s) => {
                if let Some(replacement) = mapping.get(s.as_str()) {
                    replacement.clone()
                } else {
                    template.clone()
                }
            }
            Sexpr::Object(obj) => Sexpr::Object(obj.substitute(mapping)),
        }
    }
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Nil => write!(f, "()"),
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
                    match p.1 {
                        Sexpr::Nil => Ok(()),
                        Sexpr::Pair(_) => write!(f, " {:#}", p.1),
                        _ => write!(f, " . {}", p.1),
                    }
                }
            }
            Sexpr::Object(obj) => write!(f, "{}", obj),
        }
    }
}

impl CxR for Sexpr {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        match self {
            Sexpr::Pair(p) => Some(&p.0),
            _ => None,
        }
    }

    fn cdr(&self) -> Option<&Self> {
        match self {
            Sexpr::Pair(p) => Some(&p.1),
            _ => None,
        }
    }
}

impl From<i64> for Sexpr {
    fn from(i: i64) -> Self {
        Sexpr::int(i)
    }
}

impl<'s> From<&'s str> for Sexpr {
    fn from(s: &'s str) -> Self {
        Sexpr::symbol(s)
    }
}

impl Eq for Sexpr {}

impl Hash for Sexpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Sexpr::Nil => 0u8.hash(state),
            Sexpr::Bool(b) => {
                1u8.hash(state);
                b.hash(state)
            }
            Sexpr::Integer(i) => {
                2u8.hash(state);
                i.hash(state)
            }
            Sexpr::Symbol(s) => {
                3u8.hash(state);
                s.hash(state)
            }
            Sexpr::String(s) => {
                4u8.hash(state);
                s.hash(state)
            }
            Sexpr::Pair(p) => {
                5u8.hash(state);
                Rc::as_ptr(p).hash(state)
            }
            Sexpr::Object(obj) => {
                6u8.hash(state);
                Rc::as_ptr(obj).hash(state)
            }
        }
    }
}
