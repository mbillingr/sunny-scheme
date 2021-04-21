#[macro_export]
macro_rules! sexpr {
    ($sexpr:tt) => { sexpr!{$crate::prelude::StatelessFactory; $sexpr} };

    ($factory:expr; ()) => { $factory.null() };

    ($factory:expr; {true}) => { $factory.bool(true) };
    ($factory:expr; {false}) => { $factory.bool(false) };

    ($factory:expr; {$x:expr}) => { $factory.interned_symbol($x) };

    ($factory:expr; {@ $x:ident}) => { $factory.build_from($x) };

    ($factory:expr; $x:ident) => { $factory.interned_symbol(stringify!($x)) };

    ($factory:expr; ($x:tt . $y:tt)) => {
        $factory.pair(
            sexpr![$factory; $x],
            sexpr![$factory; $y],
        )
    };

    ($factory:expr; ($x:tt $($rest:tt)*)) => {
        $factory.pair(
            sexpr![$factory; $x],
            sexpr![$factory; ($($rest)*)],
        )
    };

    ($factory:expr; $x:expr) => { $factory.build_from($x) };
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;
    use Value::*;

    #[derive(Debug, PartialEq)]
    enum Value {
        Null,
        True,
        False,
        Int(i32),
        Symbol(&'static str),
        String(&'static str),
        Pair(Box<(Value, Value)>),
    }

    impl GenericFactory<i32, Value> for StatelessFactory {
        fn build_from(&mut self, value: i32) -> Value {
            Int(value)
        }
    }

    impl GenericFactory<&'static str, Value> for StatelessFactory {
        fn build_from(&mut self, value: &'static str) -> Value {
            String(value)
        }
    }

    impl NullFactory<Value> for StatelessFactory {
        fn null(&mut self) -> Value {
            Value::Null
        }
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
                _ => None,
            }
        }
    }

    impl SymbolFactory<&'static str, Value> for StatelessFactory {
        fn interned_symbol(&mut self, name: &'static str) -> Value {
            Symbol(name)
        }

        fn uninterned_symbol(&mut self, _: &'static str) -> Value {
            unimplemented!()
        }
    }

    impl MaybeSymbol for Value {
        fn to_symbol(&self) -> Option<&str> {
            match self {
                Symbol(name) => Some(name),
                _ => None,
            }
        }
    }

    impl PairFactory<Value> for StatelessFactory {
        fn pair(&mut self, first: Value, second: Value) -> Value {
            Pair(Box::new((first, second)))
        }
    }

    impl MaybePair for Value {
        type Left = Value;
        type Right = Value;

        fn left(&self) -> Option<&Self::Left> {
            match self {
                Pair(p) => Some(&p.0),
                _ => None,
            }
        }

        fn right(&self) -> Option<&Self::Right> {
            match self {
                Pair(p) => Some(&p.1),
                _ => None,
            }
        }
    }

    #[test]
    fn build_null() {
        let expr: Value = sexpr![()];
        assert_eq!(expr, Null);
    }

    #[test]
    fn build_true() {
        let expr: Value = sexpr![{ true }];
        assert_eq!(expr, True);
    }

    #[test]
    fn build_false() {
        let expr: Value = sexpr![{ false }];
        assert_eq!(expr, False);
    }

    #[test]
    fn build_generic_expr() {
        let expr: Value = sexpr![42];
        assert_eq!(expr, Int(42));
    }

    #[test]
    fn build_symbol() {
        let expr: Value = sexpr![foo];
        assert_eq!(expr, Symbol("foo"));
    }

    #[test]
    fn build_special_symbol() {
        let expr: Value = sexpr![{ "foo-bar" }];
        assert_eq!(expr, Symbol("foo-bar"));
    }

    #[test]
    fn build_string() {
        let expr: Value = sexpr!["foo bar"];
        assert_eq!(expr, String("foo bar"));
    }

    #[test]
    fn build_variable() {
        let foo = 1 + 2;
        let expr: Value = sexpr![{@foo}];
        assert_eq!(expr, Int(3));
    }

    #[test]
    fn build_pair() {
        let expr: Value = sexpr![(foo.bar)];
        assert_eq!(expr, Pair(Box::new((Symbol("foo"), Symbol("bar")))));
    }

    #[test]
    fn build_list() {
        let expr: Value = sexpr![(1 2)];
        assert_eq!(
            expr,
            Pair(Box::new((Int(1), Pair(Box::new((Int(2), Null))))))
        );
    }

    #[test]
    fn build_dotted_list() {
        let expr: Value = sexpr![(1 2 . 3)];
        assert_eq!(
            expr,
            Pair(Box::new((Int(1), Pair(Box::new((Int(2), Int(3)))))))
        );
    }
}
