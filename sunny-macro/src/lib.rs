#[macro_export]
macro_rules! scm {
    ((lambda ($($param:ident)*) $($body:tt)+)) => {
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

    ((quote $name:ident)) => {
        Scm::symbol(stringify!($name))
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


#[cfg(test)]
mod tests {
    use super::*;

    use sunny_core::Scm;
    use sunny_core::{add, cons, square};

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

    #[test]
    fn symbol() {
        assert_eq!(scm![(quote foo)], Scm::symbol("foo"));
    }

    #[test]
    fn first_order_abstractions() {
        assert_eq!(scm![((lambda (f) (f)) (lambda () 1))], Scm::Int(1));
    }

    #[test]
    fn first_order_primitives() {
        assert_eq!(scm![((lambda (f) (f 3)) square)], Scm::Int(9));
    }

    #[test]
    fn apply_abstraction_two_args() {
        assert_eq!(scm![((lambda (a b) (add a b)) 1 2)], Scm::Int(3));
    }

    #[test]
    fn apply_abstraction_four_args() {
        assert_eq!(scm![((lambda (a b c d) (add (add a b) (add c d))) 1 2 3 4)], Scm::Int(10));
    }
}
