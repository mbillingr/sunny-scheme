use std::rc::Rc;

#[derive(Clone)]
pub enum Scm {
    Nil,
    Int(i64),
    Symbol(Rc<String>),
    Pair(Rc<(Scm, Scm)>),
    Func(Rc<dyn Fn(&[Scm]) -> Scm>),
}

impl Scm {
    pub fn symbol(s: &str) -> Self {
        Scm::Symbol(Rc::new(s.to_owned()))
    }

    pub fn pair(car: impl Into<Scm>, cdr: impl Into<Scm>) -> Self {
        Scm::Pair(Rc::new((car.into(), cdr.into())))
    }

    pub fn func(f: impl Fn(&[Scm]) -> Scm + 'static) -> Self {
        Scm::Func(Rc::new(f))
    }

    pub fn invoke(&self, args: &[Scm]) -> Scm {
        match self {
            Scm::Func(func) => func(args),
            _ => panic!("Attempt to call {:?}", self),
        }
    }
}

impl From<i64> for Scm {
    fn from(i: i64) -> Self {
        Scm::Int(i)
    }
}

impl From<&Scm> for Scm {
    fn from(x: &Scm) -> Self {
        x.clone()
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
            (Int(a), Int(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "'()"),
            Scm::Symbol(s) => write!(f, "{}", s),
            Scm::Int(x) => write!(f, "{:?}", x),
            Scm::Pair(p) => write!(f, "({:?} . {:?})", p.0, p.1),
            Scm::Func(x) => write!(f, "<procedure {:p}>", &**x),
        }
    }
}

pub fn cons(args: &[Scm]) -> Scm {
    match args {
        [car, cdr] => Scm::pair(car.clone(), cdr.clone()),
        _ => panic!("Incorrect arity: cons {:?}", args),
    }
}

pub fn add(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::Int(x + y),
        _ => panic!("Cannot add {:?}", args),
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
