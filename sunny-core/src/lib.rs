use std::rc::Rc;

#[derive(Clone)]
enum Scm {
    Nil,
    Int(i64),
    Pair(Rc<(Scm, Scm)>),
    Func(Rc<dyn Fn(&[Scm]) -> Scm>),
}

impl Scm {
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
            (Pair(a), Pair(b)) => a.0 == b.0 && a.1 == b.1,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Scm::Nil => write!(f, "'()"),
            Scm::Int(x) => write!(f, "{:?}", x),
            Scm::Pair(p) => write!(f, "({:?} . {:?})", p.0, p.1),
            Scm::Func(x) => write!(f, "<procedure {:p}>", &**x),
        }
    }
}

macro_rules! scm {
    ((lambda ($($param:ident),*) $($body:tt)+)) => {
        Scm::func(|args: &[Scm]| match args {
            [$($param),*] => { $(scm![$body]);+ }
            _ => panic!("Incorrect arity")
        })
    };

    ((quote ($first:tt $($rest:tt)*))) => {
        Scm::pair(
            scm![(quote $first)],
            scm![(quote ($($rest)*))]
        )
    };

    ((quote ())) => {
        Scm::Nil
    };

    ((quote $x:expr)) => {
        scm![$x]
    };

    (($func:tt $($arg:tt)*)) => {
        scm![$func].invoke(&[$(scm![$arg]),*])
     };

    //($x:ident) => {($x).clone()};

    ($x:expr) => {Scm::from($x)};
}

fn cons(args: &[Scm]) -> Scm {
    match args {
        [car, cdr] => Scm::pair(car.clone(), cdr.clone()),
        _ => panic!("Incorrect arity: cons {:?}", args),
    }
}

fn add(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x), Scm::Int(y)] => Scm::Int(x + y),
        _ => panic!("Cannot add {:?}", args),
    }
}

fn square(args: &[Scm]) -> Scm {
    match args {
        [Scm::Int(x)] => Scm::Int(x * x),
        _ => panic!("Cannot square {:?} ", args),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_constant() {
        assert_eq!(scm![1], Scm::Int(1));
    }

    #[test]
    fn lambda_simple() {
        assert_eq!(scm![(lambda () 0)].invoke(&[]), Scm::Int(0));
    }

    #[test]
    fn lambda_simple_sequence() {
        assert_eq!(scm![(lambda () 1 2 3)].invoke(&[]), Scm::Int(3));
    }

    #[test]
    fn lambda_identity() {
        assert_eq!(scm![(lambda (x) x)].invoke(&[Scm::Int(1)]), Scm::Int(1));
    }

    #[test]
    fn apply_lambda_simple() {
        assert_eq!(scm![((lambda () 0))], Scm::Int(0));
    }

    #[test]
    fn apply_lambda_identity() {
        assert_eq!(scm![((lambda (x) x) 1)], Scm::Int(1));
    }

    #[test]
    fn apply_binary_primitive() {
        assert_eq!(scm![(add 1 2)], Scm::Int(3));
    }

    #[test]
    fn apply_unary_primitive() {
        assert_eq!(scm![(square 3)], Scm::Int(9));
    }

    #[test]
    fn apply_abstraction() {
        assert_eq!(scm![((lambda (x) (add x x)) 3)], Scm::Int(6));
    }

    #[test]
    fn pair() {
        assert_eq!(scm![(cons 1 2)], Scm::pair(1, 2));
    }

    #[test]
    fn quote() {
        assert_eq!(scm![(quote (1 2))], Scm::pair(1, Scm::pair(2, Scm::Nil)));
    }
}
