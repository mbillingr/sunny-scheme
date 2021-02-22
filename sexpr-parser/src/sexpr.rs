use crate::str_utils::{find_end_of_line, find_start_of_line, line_number};
use crate::{CxR, Int};
use std::fmt::Debug;
use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum SourceKind {
    None,
    String(Arc<String>),
    Filename(Arc<PathBuf>),
}

impl Default for SourceKind {
    fn default() -> Self {
        SourceKind::None
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SourceLocation<T> {
    inner_value: T,
    span: Range<usize>,
    source: SourceKind,
}

impl<T> SourceLocation<T> {
    pub fn new(value: T) -> Self {
        SourceLocation {
            inner_value: value,
            span: Range::default(),
            source: SourceKind::None,
        }
    }

    pub fn in_string(self, s: impl ToString) -> Self {
        SourceLocation {
            source: SourceKind::String(Arc::new(s.to_string())),
            ..self
        }
    }

    pub fn in_file(self, path: impl Into<PathBuf>) -> Self {
        SourceLocation {
            source: SourceKind::Filename(Arc::new(path.into())),
            ..self
        }
    }

    pub fn with_span(self, span: Range<usize>) -> Self {
        SourceLocation { span, ..self }
    }

    pub fn with_position(self, pos: usize) -> Self {
        SourceLocation {
            span: pos..pos,
            ..self
        }
    }

    pub fn get_value(&self) -> &T {
        &self.inner_value
    }

    pub fn into_value(self) -> T {
        self.inner_value
    }

    pub fn convert<U: From<T>>(self) -> SourceLocation<U> {
        SourceLocation {
            inner_value: self.inner_value.into(),
            span: self.span,
            source: self.source,
        }
    }

    pub fn map<U>(&self, new_value: U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: new_value,
            span: self.span.clone(),
            source: self.source.clone(),
        }
    }

    pub fn map_after<U>(&self, new_value: U) -> SourceLocation<U> {
        SourceLocation {
            inner_value: new_value,
            span: self.span.end..self.span.end,
            source: self.source.clone(),
        }
    }
}

impl<T: std::fmt::Display> SourceLocation<T> {
    pub fn pretty_fmt(&self) -> String {
        match &self.source {
            SourceKind::None => format!("{}", self.inner_value),
            SourceKind::Filename(f) => format!("{}\n{:?} in {:?}", self.inner_value, self.span, f),
            SourceKind::String(s) => self.pretty_fmt_in_source(s),
        }
    }

    fn pretty_fmt_in_source(&self, src: &str) -> String {
        if self.span.len() == 0 {
            let pos = self.span.start;
            let line_start = find_start_of_line(src, pos);
            let line_end = find_end_of_line(src, pos);
            let line_number = line_number(src, pos);

            if pos >= src.len() {
                format!(
                    "{}\n{:5}  {}\n       {: <s$}^",
                    self.inner_value,
                    line_number,
                    &src[line_start..line_end],
                    "",
                    s = pos - line_start
                )
            } else {
                format!(
                    "{} `{}`\n{:5}  {}\n       {: <s$}^",
                    self.inner_value,
                    &src[pos..pos + 1],
                    line_number,
                    &src[line_start..line_end],
                    "",
                    s = pos - line_start,
                )
            }
        } else {
            let line_start = find_start_of_line(src, self.span.start);
            let line_end = find_end_of_line(src, line_start);
            let line_number = line_number(src, self.span.start);

            format!(
                "{} '{}'\n{:5}  {}\n       {: <s$}{:^<e$}",
                self.inner_value,
                &src[self.span.clone()],
                line_number,
                &src[line_start..line_end],
                "",
                "",
                s = self.span.start - line_start,
                e = self.span.len()
            )
        }
    }
}

impl<T> SourceLocation<T> {
    pub fn pretty_fmt_inline(&self) -> String {
        match &self.source {
            SourceKind::None => String::new(),
            SourceKind::Filename(_) => String::new(),
            SourceKind::String(src) => {
                if self.span.len() <= 1 {
                    let pos = self.span.start;
                    let line_start = find_start_of_line(src, pos);
                    let line_end = find_end_of_line(src, pos);
                    format!("{}", &src[line_start..line_end])
                } else {
                    format!("{}", &src[self.span.clone()])
                }
            }
        }
    }
}

impl SourceLocation<Sexpr<'_>> {
    pub fn iter(&self) -> impl Iterator<Item = &SourceLocation<Sexpr>> {
        let mut cursor = self;
        (0..)
            .map(move |_| match cursor.get_value() {
                Sexpr::Pair(pair) => {
                    cursor = &pair.1;
                    Some(&pair.0)
                }
                _ => None,
            })
            .take_while(Option::is_some)
            .map(Option::unwrap)
    }
}

impl<T> Deref for SourceLocation<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.get_value()
    }
}

impl CxR for SourceLocation<Sexpr<'_>> {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.get_value().car()
    }

    fn cdr(&self) -> Option<&Self> {
        self.get_value().cdr()
    }
}

impl<T> From<T> for SourceLocation<T> {
    fn from(x: T) -> SourceLocation<T> {
        SourceLocation::new(x)
    }
}

impl<'src> From<SourceLocation<Sexpr<'src>>> for Sexpr<'src> {
    fn from(c: SourceLocation<Sexpr>) -> Sexpr {
        c.into_value()
    }
}

impl PartialEq<Sexpr<'_>> for SourceLocation<Sexpr<'_>> {
    fn eq(&self, other: &Sexpr) -> bool {
        self.get_value() == other
    }
}

impl<T: std::fmt::Display> std::fmt::Display for SourceLocation<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.get_value().fmt(f)
    }
}
