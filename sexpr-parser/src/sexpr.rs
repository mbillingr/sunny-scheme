use crate::{CxR, Int};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Sexpr {
    Nil,
    Integer(Int),
    Symbol(Box<str>),
    String(Box<str>),
    Pair(Rc<(Context<Sexpr>, Context<Sexpr>)>),
}

impl Sexpr {
    pub fn nil() -> Self {
        Sexpr::Nil
    }
    pub fn int(i: Int) -> Self {
        Sexpr::Integer(i)
    }

    pub fn symbol(name: impl Into<Box<str>>) -> Self {
        Sexpr::Symbol(name.into())
    }

    pub fn string(name: impl Into<Box<str>>) -> Self {
        Sexpr::String(name.into())
    }

    pub fn cons(car: impl Into<Context<Sexpr>>, cdr: impl Into<Context<Sexpr>>) -> Self {
        Sexpr::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn list(items: impl DoubleEndedIterator<Item = Context<Sexpr>>) -> Self {
        let l = items.rfold(Self::nil(), |acc, x| Self::cons(x, acc));
        l
    }

    pub fn iter(&self) -> impl Iterator<Item = &Self> {
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
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Nil => write!(f, "()"),
            Sexpr::Integer(i) => write!(f, "{}", i),
            Sexpr::Symbol(s) => write!(f, "{}", s),
            Sexpr::String(s) => write!(f, "{:?}", s),
            Sexpr::Pair(p) => write!(f, "({} . {})", p.0, p.1),
        }
    }
}

impl CxR for Sexpr {
    type Result = Context<Self>;

    fn car(&self) -> Option<&Context<Self>> {
        match self {
            Sexpr::Pair(p) => Some(&p.0),
            _ => None,
        }
    }

    fn cdr(&self) -> Option<&Context<Self>> {
        match self {
            Sexpr::Pair(p) => Some(&p.1),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Context<T> {
    None(T),
    File(String, Box<Context<T>>),
    String(String, Box<Context<T>>),
    Offset(usize, Box<Context<T>>),
    Cursor(usize, usize, Box<Context<T>>),
}

impl<T> Context<T> {
    pub fn get_value(&self) -> &T {
        match self {
            Context::None(x) => x,
            Context::File(_, c)
            | Context::String(_, c)
            | Context::Offset(_, c)
            | Context::Cursor(_, _, c) => c.get_value(),
        }
    }

    pub fn into_value(self) -> T {
        match self {
            Context::None(x) => x,
            Context::File(_, c)
            | Context::String(_, c)
            | Context::Offset(_, c)
            | Context::Cursor(_, _, c) => c.into_value(),
        }
    }

    pub fn string(s: impl ToString, inner: impl Into<Context<T>>) -> Self {
        Context::String(s.to_string(), Box::new(inner.into()))
    }

    pub fn offset(ofs: usize, inner: impl Into<Context<T>>) -> Self {
        Context::Offset(ofs, Box::new(inner.into()))
    }

    pub fn convert<U: From<T>>(self) -> Context<U> {
        use Context::*;
        match self {
            None(x) => None(x.into()),
            File(c, x) => File(c, Box::new(x.convert())),
            String(c, x) => String(c, Box::new(x.convert())),
            Offset(c, x) => Offset(c, Box::new(x.convert())),
            Cursor(c, d, x) => Cursor(c, d, Box::new(x.convert())),
        }
    }
}

impl Context<Sexpr> {
    pub fn iter(&self) -> impl Iterator<Item = &Context<Sexpr>> {
        let mut cursor = self;
        (0..)
            .map(move |_| match cursor.get_value() {
                Sexpr::Pair(pair) => {
                    cursor = &pair.1;
                    &pair.0
                }
                _ => std::mem::replace(&mut cursor, &Context::None(Sexpr::Nil)),
            })
            .take_while(|x| x != &&Sexpr::Nil)
    }
}

impl<T> Deref for Context<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.get_value()
    }
}

impl CxR for Context<Sexpr> {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.get_value().car()
    }

    fn cdr(&self) -> Option<&Self> {
        self.get_value().cdr()
    }
}

impl<T> From<T> for Context<T> {
    fn from(x: T) -> Context<T> {
        Context::None(x)
    }
}

impl From<Context<Sexpr>> for Sexpr {
    fn from(c: Context<Sexpr>) -> Sexpr {
        c.into_value()
    }
}

impl PartialEq<Sexpr> for Context<Sexpr> {
    fn eq(&self, other: &Sexpr) -> bool {
        self.get_value() == other
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Context<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        //self.get_value().fmt(f)
        write!(f, "{}", self.get_value())
    }
}
