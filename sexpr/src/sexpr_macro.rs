#[macro_export]
macro_rules! sexpr {
    ($factory:expr; ()) => { $factory.null() };

    (($factory:expr; $x:tt . $y:tt)) => {
        $factory.cons(
            sexpr![$factory; $x],
            sexpr![$factory; $y],
        )
    };

    ($factory:expr; ($x:tt $($rest:tt)*)) => {
        $factory.cons(
            sexpr![$factory $x],
            sexpr![$factory ($($rest)*)],
        )
    };

    ($factory:expr; {true}) => { $factory.bool(true) };
    ($factory:expr; {false}) => { $factory.bool(false) };

    ($factory:expr; {$x:expr}) => { $factory.symbol($x) };

    ($factory:expr; $x:ident) => { $factory.symbol(stringify!($x)) };

    ($factory:expr; $x:expr) => { $factory.from($x) };


    ($sexpr:tt) => { sexpr!{$crate::prelude::StatelessFactory; $sexpr} };
}


#[cfg(test)]
mod tests {
    use super::*;
    use Value::*;
    use crate::prelude::*;

    #[derive(Debug, PartialEq)]
    enum Value {
        Null,
        True,
        False,
    }

    impl NullFactory<Value> for StatelessFactory {
        fn null(&mut self) -> Value { Value::Null }
    }

    impl Nullable for Value {
        fn is_null(&self) -> bool {
            matches!(self, Null)
        }
    }

    impl BoolFactory<Value> for StatelessFactory {
        fn bool(&mut self, b: bool) -> Value {
            match b {
                true => True,
                false => False,
            }
        }
    }

    impl MaybeBool for Value {
        fn to_bool(&self) -> Option<bool> {
            match self {
                True => Some(true),
                False => Some(false),
                _ => None
            }
        }
    }

    #[test]
    fn build_null() {
        let expr: Value = sexpr![()];
        assert_eq!(expr, Null)
    }

    #[test]
    fn build_true() {
        let expr: Value = sexpr![{true}];
        assert_eq!(expr, True)
    }

    #[test]
    fn build_false() {
        let expr: Value = sexpr![{false}];
        assert_eq!(expr, False)
    }
}