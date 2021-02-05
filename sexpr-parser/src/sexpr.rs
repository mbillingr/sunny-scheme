use crate::{CxR, Int};
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Sexpr<'src> {
    Nil,
    Integer(Int),
    Symbol(&'src str),
    String(&'src str),
    Pair(Rc<(Context<Sexpr<'src>>, Context<Sexpr<'src>>)>),
}

impl<'src> Sexpr<'src> {
    pub fn nil() -> Self {
        Sexpr::Nil
    }
    pub fn int(i: Int) -> Self {
        Sexpr::Integer(i)
    }

    pub fn symbol(name: &'src str) -> Self {
        Sexpr::Symbol(name)
    }

    pub fn string(name: &'src str) -> Self {
        Sexpr::String(name)
    }

    pub fn cons(
        car: impl Into<Context<Sexpr<'src>>>,
        cdr: impl Into<Context<Sexpr<'src>>>,
    ) -> Self {
        Sexpr::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn list(items: impl DoubleEndedIterator<Item = Context<Sexpr<'src>>>) -> Self {
        let l = items.rfold(Self::nil(), |acc, x| Self::cons(x, acc));
        l
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
            Sexpr::Integer(x) if *x > 0 => Some(*x as usize),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Sexpr::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Sexpr<'_>> {
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

impl std::fmt::Display for Sexpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Sexpr::Nil => write!(f, "()"),
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
        }
    }
}

impl CxR for Sexpr<'_> {
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
    Extern(T),
    Span { begin: usize, end: usize, value: T },
}

impl<T> Context<T> {
    pub fn new(begin: usize, end: usize, value: T) -> Self {
        Context::Span { begin, end, value }
    }

    pub fn get_value(&self) -> &T {
        match self {
            Context::Extern(value) => value,
            Context::Span { value, .. } => value,
        }
    }

    pub fn into_value(self) -> T {
        match self {
            Context::Extern(value) => value,
            Context::Span { value, .. } => value,
        }
    }

    pub fn convert<U: From<T>>(self) -> Context<U> {
        match self {
            Context::Extern(value) => Context::Extern(value.into()),
            Context::Span { begin, end, value } => Context::Span {
                begin,
                end,
                value: value.into(),
            },
        }
    }

    pub fn map<U>(&self, new_value: U) -> Context<U> {
        match self {
            Context::Extern(_) => Context::Extern(new_value),
            Context::Span { begin, end, .. } => Context::Span {
                begin: *begin,
                end: *end,
                value: new_value,
            },
        }
    }

    pub fn map_after<U>(&self, new_value: U) -> Context<U> {
        match self {
            Context::Extern(_) => Context::Extern(new_value),
            Context::Span { end, .. } => Context::Span {
                begin: *end,
                end: *end,
                value: new_value,
            },
        }
    }
}

impl Context<Sexpr<'_>> {
    pub fn iter(&self) -> impl Iterator<Item = &Context<Sexpr>> {
        let mut cursor = self;
        (0..)
            .map(move |_| match cursor.get_value() {
                Sexpr::Pair(pair) => {
                    cursor = &pair.1;
                    &pair.0
                }
                _ => std::mem::replace(&mut cursor, &Context::Extern(Sexpr::Nil)),
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

impl CxR for Context<Sexpr<'_>> {
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
        Context::Extern(x)
    }
}

impl<'src> From<Context<Sexpr<'src>>> for Sexpr<'src> {
    fn from(c: Context<Sexpr>) -> Sexpr {
        c.into_value()
    }
}

impl PartialEq<Sexpr<'_>> for Context<Sexpr<'_>> {
    fn eq(&self, other: &Sexpr) -> bool {
        self.get_value() == other
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Context<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.get_value().fmt(f)
    }
}
