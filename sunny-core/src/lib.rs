extern crate lazy_static;

mod memory_model;
pub mod port;
pub mod string;
pub mod symbol;

use crate::port::{InputPort, OutputPort};
pub use memory_model::prelude::Mut;
use memory_model::prelude::*;
use std::cell::RefCell;
use string::ScmString;
use symbol::Symbol;

pub type BoxedScm = Boxed<Scm>;

pub const MEMORY_MODEL_KIND: &'static str = memory_model::KIND;

#[derive(Clone)]
#[cfg_attr(feature = "scm_copy", derive(Copy))]
pub enum Scm {
    Nil,
    Eof,
    False,
    True,
    Int(i64),
    Char(char),
    Symbol(Symbol),
    String(ScmString),
    Pair(Ref<(Mut<Scm>, Mut<Scm>)>),
    Func(Ref<dyn Fn(&[Scm]) -> Scm>),

    InputPort(Ref<RefCell<dyn InputPort>>),
    OutputPort(Ref<RefCell<dyn OutputPort>>),
}

impl Scm {
    pub fn uninitialized() -> Self {
        Scm::symbol("*UNINITIALIZED*")
    }

    pub fn into_boxed(self) -> BoxedScm {
        BoxedScm::new(self)
    }

    #[cfg(feature = "scm_copy")]
    pub fn duplicate(&self) -> Self {
        *self
    }

    #[cfg(not(feature = "scm_copy"))]
    pub fn duplicate(&self) -> Self {
        self.clone()
    }

    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn eof() -> Self {
        Self::Eof
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => Self::True,
            false => Self::False,
        }
    }

    pub fn int(i: i64) -> Self {
        Scm::Int(i)
    }

    pub fn char(ch: char) -> Self {
        Scm::Char(ch)
    }

    pub fn char_apostrophe() -> Self {
        Scm::Char('\'')
    }

    pub fn symbol(s: &str) -> Self {
        Scm::Symbol(Symbol::new(s))
    }

    pub fn str(s: &'static str) -> Self {
        Scm::String(ScmString::from_static(s))
    }

    pub fn string(s: impl Into<Box<str>>) -> Self {
        Scm::String(ScmString::from_string(s))
    }

    pub fn pair(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        Scm::Pair(make_ref!((Mut::new(car.into()), Mut::new(cdr.into()))))
    }

    pub fn list(items: &[Scm]) -> Self {
        let mut list = Scm::Nil;
        for x in items.iter().rev() {
            list = Scm::pair(x, list);
        }
        list
    }

    pub fn func(f: impl Fn(&[Scm]) -> Scm + 'static) -> Self {
        Scm::Func(make_ref!(f))
    }

    pub fn func0<T: Into<Scm>>(f: impl Fn() -> T + 'static) -> Self {
        Scm::Func(make_ref!(move |args: &[Scm]| match args {
            [] => f().into(),
            _ => panic!("0-ary function called with {} arguments", args.len()),
        }))
    }

    pub fn func1<T: Into<Scm>>(f: impl Fn(&Scm) -> T + 'static) -> Self {
        Scm::Func(make_ref!(move |args: &[Scm]| match args {
            [a] => f(a).into(),
            _ => panic!("1-ary function called with {} arguments", args.len()),
        }))
    }

    pub fn func2<T: Into<Scm>>(f: impl Fn(&Scm, &Scm) -> T + 'static) -> Self {
        Scm::Func(make_ref!(move |args: &[Scm]| match args {
            [a, b] => f(a, b).into(),
            _ => panic!("2-ary function called with {} arguments", args.len()),
        }))
    }

    pub fn input_port(port: impl InputPort + 'static) -> Self {
        Scm::InputPort(make_ref!(RefCell::new(port)))
    }

    pub fn output_port(port: impl OutputPort + 'static) -> Self {
        Scm::OutputPort(make_ref!(RefCell::new(port)))
    }

    pub fn is_true(&self) -> bool {
        match self {
            Scm::Nil | Scm::False => false,
            _ => true,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Scm::Nil => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self {
            Scm::Eof => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Scm::String(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Scm::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            Scm::Char(_) => true,
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Scm::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self {
            Scm::Func(_) => true,
            _ => false,
        }
    }

    pub fn invoke(&self, args: &[Scm]) -> Scm {
        match self {
            Scm::Func(func) => func(args),
            _ => panic!("Attempt to call {:?}", self),
        }
    }

    pub fn as_char(&self) -> Option<char> {
        match self {
            Scm::Char(ch) => Some(*ch),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            Scm::Symbol(s) => Some(*s),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<ScmString> {
        match self {
            Scm::String(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn as_pair(&self) -> Option<(Scm, Scm)> {
        match self {
            Scm::Pair(p) => Some((p.0.get(), p.1.get())),
            _ => None,
        }
    }

    pub fn with_input_port<T>(&self, f: impl FnOnce(&mut dyn InputPort) -> T) -> Option<T> {
        match self {
            Scm::InputPort(p) => Some(f(&mut *p.borrow_mut())),
            _ => None,
        }
    }

    pub fn with_output_port<T>(&self, f: impl FnOnce(&mut dyn OutputPort) -> T) -> Option<T> {
        match self {
            Scm::OutputPort(p) => Some(f(&mut *p.borrow_mut())),
            _ => None,
        }
    }

    pub fn car(&self) -> Option<Scm> {
        match self {
            Scm::Pair(p) => Some(p.0.get()),
            _ => None,
        }
    }

    pub fn cdr(&self) -> Option<Scm> {
        match self {
            Scm::Pair(p) => Some(p.1.get()),
            _ => None,
        }
    }

    pub fn set_car(&self, value: Scm) -> Option<()> {
        match self {
            Scm::Pair(p) => {
                p.0.set(value);
                Some(())
            }
            _ => None,
        }
    }

    pub fn set_cdr(&self, value: Scm) -> Option<()> {
        match self {
            Scm::Pair(p) => {
                p.1.set(value);
                Some(())
            }
            _ => None,
        }
    }

    pub fn caar(&self) -> Option<Scm> {
        self.car()?.car()
    }

    pub fn cadr(&self) -> Option<Scm> {
        self.cdr()?.car()
    }

    pub fn cdar(&self) -> Option<Scm> {
        self.car()?.cdr()
    }

    pub fn cddr(&self) -> Option<Scm> {
        self.cdr()?.cdr()
    }

    pub fn close_port(&self) {
        match self {
            Scm::InputPort(p) => p.borrow_mut().close(),
            Scm::OutputPort(p) => p.borrow_mut().close(),
            _ => panic!("not a port: {:?}", self),
        }
    }
}

impl From<()> for Scm {
    fn from(_: ()) -> Self {
        Scm::nil()
    }
}

impl From<bool> for Scm {
    fn from(b: bool) -> Self {
        Scm::bool(b)
    }
}

impl From<i64> for Scm {
    fn from(i: i64) -> Self {
        Scm::Int(i)
    }
}

impl From<char> for Scm {
    fn from(ch: char) -> Self {
        Scm::Char(ch)
    }
}

impl From<&'static str> for Scm {
    fn from(s: &'static str) -> Self {
        Scm::String(ScmString::from_static(s))
    }
}

impl From<String> for Scm {
    fn from(s: String) -> Self {
        Scm::String(ScmString::from_string(s))
    }
}

impl From<&Scm> for Scm {
    fn from(x: &Scm) -> Self {
        x.duplicate()
    }
}

impl From<BoxedScm> for Scm {
    fn from(x: BoxedScm) -> Self {
        x.get()
    }
}

impl From<&BoxedScm> for Scm {
    fn from(x: &BoxedScm) -> Self {
        x.get()
    }
}

impl<T: Fn(&[Scm]) -> Scm + 'static> From<T> for Scm {
    fn from(f: T) -> Self {
        Scm::func(f)
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Char(a), Char(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Pair(a), Pair(b)) => a.0.get() == b.0.get() && a.1.get() == b.1.get(),
            _ => false,
        }
    }
}

impl std::fmt::Debug for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "()"),
            Scm::Eof => write!(f, "#<eof>"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Char(ch) => write!(f, "#\\{}", ch),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::String(s) => write!(f, "\"{}\"", s.as_str().replace('"', "\\\"")),
            Scm::Int(x) => write!(f, "{:?}", x),
            Scm::Pair(p) => {
                write!(f, "(")?;
                write!(f, "{:?}", p.0.get())?;
                let mut x = p.1.get();
                while let Some((car, cdr)) = x.as_pair() {
                    write!(f, " {:?}", car)?;
                    x = cdr;
                }
                if !x.is_null() {
                    write!(f, " . {:?}", x)?;
                }
                write!(f, ")")
            }
            Scm::Func(x) => write!(f, "<procedure {:p}>", &*x),
            Scm::InputPort(x) => write!(f, "<input port {:p}>", &*x),
            Scm::OutputPort(x) => write!(f, "<output port {:p}>", &*x),
        }
    }
}

impl std::fmt::Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "()"),
            Scm::Eof => write!(f, "#<eof>"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Char(ch) => write!(f, "{}", ch),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::String(s) => write!(f, "{}", s),
            Scm::Int(x) => write!(f, "{}", x),
            Scm::Pair(p) => {
                write!(f, "(")?;
                write!(f, "{}", p.0.get())?;
                let mut x = p.1.get();
                while let Some((car, cdr)) = x.as_pair() {
                    write!(f, " {}", car)?;
                    x = cdr;
                }
                if !x.is_null() {
                    write!(f, " . {}", x)?;
                }
                write!(f, ")")
            }
            Scm::Func(x) => write!(f, "<procedure {:p}>", &*x),
            Scm::InputPort(x) => write!(f, "<input port {:p}>", &*x),
            Scm::OutputPort(x) => write!(f, "<output port {:p}>", &*x),
        }
    }
}

pub fn car(args: &[Scm]) -> Scm {
    match args {
        [Scm::Pair(p)] => p.0.get(),
        [_] => panic!("Not a pair: car {:?}", args),
        _ => panic!("Incorrect arity: cons {:?}", args),
    }
}

pub fn cdr(args: &[Scm]) -> Scm {
    match args {
        [Scm::Pair(p)] => p.1.get(),
        [_] => panic!("Not a pair: car {:?}", args),
        _ => panic!("Incorrect arity: cons {:?}", args),
    }
}

pub fn cons(args: &[Scm]) -> Scm {
    match args {
        [car, cdr] => Scm::pair(car.duplicate(), cdr.duplicate()),
        _ => panic!("Incorrect arity: cons {:?}", args),
    }
}

pub fn is_numeq(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::from(x == y),
        _ => panic!("Cannot compare {:?}", args),
    }
}

pub fn is_numgt(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::from(x > y),
        _ => panic!("Cannot compare {:?}", args),
    }
}

pub fn is_numlt(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::from(x < y),
        _ => panic!("Cannot compare {:?}", args),
    }
}

pub fn is_ptreq(args: &[Scm]) -> Scm {
    use Scm::*;
    match (&args[0], &args[1]) {
        (Nil, Nil) => Scm::True,
        (True, True) => Scm::True,
        (False, False) => Scm::True,
        (Int(a), Int(b)) => Scm::bool(a == b),
        (Char(a), Char(b)) => Scm::bool(a == b),
        (Symbol(a), Symbol(b)) => Scm::bool(a == b),
        (String(a), String(b)) => Scm::bool(a.is_ptreq(b)),
        (Pair(a), Pair(b)) => Scm::bool(ref_as_ptr(a) == ref_as_ptr(b)),
        (Func(a), Func(b)) => Scm::bool(ref_as_ptr(a) == ref_as_ptr(b)),
        (InputPort(a), InputPort(b)) => Scm::bool(ref_as_ptr(a) == ref_as_ptr(b)),
        _ => Scm::False,
    }
}

pub fn add(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::Int(x + y),
        _ => panic!("Cannot add {:?}", args),
    }
}

pub fn sub(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::Int(x - y),
        _ => panic!("Cannot sub {:?}", args),
    }
}

pub fn less(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => {
            if x < y {
                Scm::True
            } else {
                Scm::False
            }
        }
        _ => panic!("Cannot compare < {:?}", args),
    }
}

pub fn square(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x)] => Scm::Int(x * x),
        _ => panic!("Cannot square {:?} ", args),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::port::{FileInputPort, MemoryInputPort};

    #[test]
    fn it_works() {
        assert_eq!(add(&[Scm::Int(2), Scm::Int(2)]), Scm::Int(4));
    }

    #[test]
    fn input_port() {
        let source = MemoryInputPort::from_str("foo");
        let scm = Scm::input_port(source);
        let line = scm
            .with_input_port(|port| port.read_line().unwrap())
            .unwrap();
        assert_eq!(line, "foo");
    }

    #[test]
    fn input_port2() {
        let source = FileInputPort::open("Cargo.toml");
        let scm = Scm::input_port(source);
        let line = scm
            .with_input_port(|port| port.read_line().unwrap())
            .unwrap();
        assert_eq!(line, "[package]");
    }
}
