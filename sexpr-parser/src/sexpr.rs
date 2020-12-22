use crate::Int;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Sexpr {
    Integer(Int),
    Symbol(Box<str>),
    Pair((Rc<Context<Sexpr>>, Rc<Context<Sexpr>>)),
}

impl Sexpr {
    pub fn int(i: Int) -> Self {
        Sexpr::Integer(i)
    }
    pub fn symbol(name: impl Into<Box<str>>) -> Self {
        Sexpr::Symbol(name.into())
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
