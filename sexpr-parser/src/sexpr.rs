use crate::{CxR, Int, SourceLocation};
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Sexpr<'src> {
    Nil,
    Bool(bool),
    Integer(Int),
    Symbol(&'src str),
    String(&'src str),
    Pair(Rc<(SourceLocation<Sexpr<'src>>, SourceLocation<Sexpr<'src>>)>),
}

impl<'src> Sexpr<'src> {
    pub fn nil() -> Self {
        Sexpr::Nil
    }

    pub fn bool(b: bool) -> Self {
        Self::Bool(b)
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
        car: impl Into<SourceLocation<Sexpr<'src>>>,
        cdr: impl Into<SourceLocation<Sexpr<'src>>>,
    ) -> Self {
        Sexpr::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn list(items: impl DoubleEndedIterator<Item = SourceLocation<Sexpr<'src>>>) -> Self {
        let l = items.rfold(Self::nil(), |acc, x| Self::cons(x, acc));
        l
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

    pub fn len(&self) -> usize {
        if self.is_pair() {
            1 + self.cdr().unwrap().len()
        } else {
            0
        }
    }
}

impl std::fmt::Display for Sexpr<'_> {
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
        }
    }
}

impl CxR for Sexpr<'_> {
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

impl From<i64> for Sexpr<'_> {
    fn from(i: i64) -> Self {
        Sexpr::int(i)
    }
}

impl<'s> From<&'s str> for Sexpr<'s> {
    fn from(s: &'s str) -> Self {
        Sexpr::symbol(s)
    }
}

impl<'src> From<SourceLocation<Sexpr<'src>>> for Sexpr<'src> {
    fn from(c: SourceLocation<Sexpr>) -> Sexpr {
        c.into_value()
    }
}
