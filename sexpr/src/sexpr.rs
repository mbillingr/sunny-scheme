use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::cxr::CxR;
use crate::{Int, SourceLocation};
use std::any::Any;

pub type SrcExpr = SourceLocation<Sexpr>;
pub type RefExpr<'a> = &'a SrcExpr;

pub trait SexprObject: Any + Display + Debug {
    fn substitute(&self, mapping: &HashMap<&str, SrcExpr>) -> Rc<dyn AnySexprObject>;
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
    Pair(Rc<(SrcExpr, SrcExpr)>),
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
            (Object(a), Object(b)) => Rc::ptr_eq(a, b),
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

    pub fn cons(car: impl Into<SrcExpr>, cdr: impl Into<SrcExpr>) -> Self {
        Sexpr::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn list(items: impl DoubleEndedIterator<Item = SrcExpr>) -> Self {
        let l = items.rfold(Self::nil(), |acc, x| Self::cons(x, acc));
        l
    }

    pub fn obj(obj: impl SexprObject) -> Self {
        Sexpr::Object(Rc::new(obj))
    }

    pub fn is_null(&self) -> bool {
        self == &Sexpr::Nil
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Sexpr::Pair(_) => true,
            _ => false,
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

    pub fn is_an_object(&self) -> bool {
        match self {
            Sexpr::Object(_) => true,
            _ => false,
        }
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
                    cursor = pair.1.get_value();
                    pair.0.get_value()
                }
                _ => std::mem::replace(&mut cursor, &Sexpr::Nil),
            })
            .take_while(|x| x != &&Sexpr::Nil)
    }

    pub fn len(&self) -> usize {
        if self.is_pair() {
            1 + self.cdr().unwrap().len()
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

    pub fn substitute(
        template: &SourceLocation<Self>,
        mapping: &HashMap<&str, SrcExpr>,
    ) -> SourceLocation<Self> {
        match template.get_value() {
            Sexpr::Nil | Sexpr::Bool(_) | Sexpr::Integer(_) | Sexpr::String(_) => template.clone(),
            Sexpr::Pair(p) => template.map_value(Sexpr::Pair(Rc::new((
                Sexpr::substitute(&p.0, mapping),
                Sexpr::substitute(&p.1, mapping),
            )))),
            Sexpr::Symbol(s) => {
                if let Some(replacement) = mapping.get(s.as_str()) {
                    replacement.clone()
                } else {
                    template.clone()
                }
            }
            Sexpr::Object(obj) => template.map_value(Sexpr::Object(obj.substitute(mapping))),
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
                    match p.1.get_value() {
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
    type Result = SourceLocation<Self>;

    fn car(&self) -> Option<&SourceLocation<Self>> {
        match self {
            Sexpr::Pair(p) => Some(&p.0),
            _ => None,
        }
    }

    fn cdr(&self) -> Option<&SourceLocation<Self>> {
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

impl From<SrcExpr> for Sexpr {
    fn from(c: SourceLocation<Sexpr>) -> Sexpr {
        c.into_value()
    }
}
