extern crate lazy_static;

mod memory_model;
pub mod symbol;

pub use memory_model::prelude::Mut;
use memory_model::prelude::*;
use symbol::Symbol;

pub type BoxedScm = Boxed<Scm>;

pub const MEMORY_MODEL_KIND: &'static str = memory_model::KIND;

#[derive(Clone)]
#[cfg_attr(feature = "scm_copy", derive(Copy))]
pub enum Scm {
    Nil,
    False,
    True,
    Int(i64),
    Symbol(Symbol),
    Pair(Ref<(Mut<Scm>, Mut<Scm>)>),
    Func(Ref<dyn Fn(&[Scm]) -> Scm>),
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

    pub fn bool(b: bool) -> Self {
        match b {
            true => Self::True,
            false => Self::False,
        }
    }

    pub fn symbol(s: &str) -> Self {
        Scm::Symbol(Symbol::new(s))
    }

    pub fn pair(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        Scm::Pair(make_ref!((Mut::new(car.into()), Mut::new(cdr.into()))))
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

    pub fn is_pair(&self) -> bool {
        match self {
            Scm::Pair(_) => true,
            _ => false,
        }
    }

    pub fn invoke(&self, args: &[Scm]) -> Scm {
        match self {
            Scm::Func(func) => func(args),
            _ => panic!("Attempt to call {:?}", self),
        }
    }

    pub fn as_pair(&self) -> Option<(Scm, Scm)> {
        match self {
            Scm::Pair(p) => Some((p.0.get(), p.1.get())),
            _ => None,
        }
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
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(a), Pair(b)) => a.0.get() == b.0.get() && a.1.get() == b.1.get(),
            _ => false,
        }
    }
}

impl std::fmt::Debug for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "'()"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::Int(x) => write!(f, "{:?}", x),
            Scm::Pair(p) => write!(f, "({:?} . {:?})", p.0, p.1),
            Scm::Func(x) => write!(f, "<procedure {:p}>", &*x),
        }
    }
}

impl std::fmt::Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "'()"),
            Scm::True => write!(f, "#t"),
            Scm::False => write!(f, "#f"),
            Scm::Symbol(s) => write!(f, "{}", s),
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

pub fn is_ptreq(args: &[Scm]) -> Scm {
    use Scm::*;
    match (&args[0], &args[1]) {
        (Nil, Nil) => Scm::True,
        (True, True) => Scm::True,
        (False, False) => Scm::True,
        (Int(a), Int(b)) => Scm::bool(a == b),
        (Symbol(a), Symbol(b)) => Scm::bool(a == b),
        (Pair(a), Pair(b)) => Scm::bool(ref_as_ptr(a) == ref_as_ptr(b)),
        (Func(a), Func(b)) => Scm::bool(ref_as_ptr(a) == ref_as_ptr(b)),
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

    #[test]
    fn it_works() {
        assert_eq!(add(&[Scm::Int(2), Scm::Int(2)]), Scm::Int(4));
    }
}
