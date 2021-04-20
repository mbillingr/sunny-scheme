#[macro_export]
macro_rules! with_sexpr_matcher {
    (match $expr:expr, {
        $($rules:tt)*
    }) => {{
        $crate::match_sexpr![$expr; $($rules)*]
    }};
}

#[macro_export]
macro_rules! match_sexpr {
    ($expr:expr; ) => {
        {}
    };

    ($expr:expr; $pattern:tt => $action:block) => {
        $crate::match_sexpr_pattern![
            if $pattern matches $expr,
            then $action
            else {}
        ]
    };

    ($expr:expr; $pattern:tt => $action:block $($rest:tt)*) => {
        $crate::match_sexpr_pattern![
            if $pattern matches $expr,
            then $action
            else { $crate::match_sexpr![$expr; $($rest)*] }
        ]
    };
}

#[macro_export]
macro_rules! match_sexpr_pattern {
    // readable interface to the macro
    (if $pattern:tt matches $expr:expr, then $then:block else $else:block) => {
        $crate::match_sexpr_pattern!($pattern, $expr, $then, $else)
    };

    // match anything
    (_, $expr:expr, $then:block, $else:block) => {{
        $then
    }};

    // match nothing
    (~, $expr:expr, $then:block, $else:block) => {{
        $else
    }};

    // match anything and bind it
    ($var:ident, $expr:expr, $then:block, $else:block) => {{
        let $var = $expr;
        $then
    }};

    // match the empty list
    ((), $expr:expr, $then:block, $else:block) => {
        if $expr.is_null() $then else $else
    };

    // match literal symbol - identifier version
    ((:$name:ident), $expr:expr, $then:block, $else:block) => {{
        $crate::match_sexpr_pattern![(:stringify!($name)), $expr, $then, $else]
    }};

    // match literal symbol - string version
    ((:$name:expr), $expr:expr, $then:block, $else:block) => {{
        if let Some($name) = $expr.to_symbol() $then else $else
    }};

    // match any symbol
    ({_: Symbol}, $expr:expr, $then:block, $else:block) => {{
        if $expr.is_symbol() $then else $else
    }};

    // match any symbol and bind it
    ({$var:ident: Symbol}, $expr:expr, $then:block, $else:block) => {{
        if let Some($var) = $expr.to_symbol() $then else $else
    }};

    // match any list
    ({_: List}, $expr:expr, $then:block, $else:block) => {{
        if $crate::is_proper_list($expr) $then else $else
    }};

    // match any list and bind it
    ({$var:ident: List}, $expr:expr, $then:block, $else:block) => {{
        if $crate::lists::is_proper_list($expr) {
            let $var = $expr;
            $then
        } else $else
    }};

    // match any Object
    ({_: Obj<$t:ty>}, $expr:expr, $then:block, $else:block) => {{
        if $expr.is_of_type($t) $then else $else
    }};

    // match any Object and bind it
    ({$var:ident: Obj<$t:ty>}, $expr:expr, $then:block, $else:block) => {{
        if let Some($var) = $expr.to_type::<$t>() {
            $then
        } else $else
    }};

    // match a pair
    (($first:tt . $second:tt), $expr:expr, $then:block, $else:block) => {
        match (if $expr.is_pair() {
            $crate::match_sexpr_pattern![
                if $first matches $expr.left().unwrap(),
                then {
                    $crate::match_sexpr_pattern![
                        if $second matches $expr.right().unwrap(),
                        then { Some($then) }
                        else { None }
                    ]
                }
                else { None }
            ]
        } else { None }) {
            Some(res) => res,
            None => $else,
        }
    };

    // match a single argument list
    (($first:tt), $expr:expr, $then:block, $else:block) => {
        match (if let (Some(_element), Some(tail)) = ($expr.first(), $expr.rest()) {
            if tail.is_null() {
                $crate::match_sexpr_pattern![
                    if $first matches _element,
                    then { Some($then) }
                    else { None }
                ]
            } else {
                None
            }
        } else {
            None
        }) {
            Some(res) => res,
            None => $else,
        }
    };

    // match a list
    (($first:tt $($rest:tt)+), $expr:expr, $then:block, $else:block) => {
        match (if let (Some(_element), Some(tail)) = ($expr.first(), $expr.rest()) {
            $crate::match_sexpr_pattern![
                if $first matches _element,
                then {
                    $crate::match_sexpr_pattern![
                        if ($($rest)+) matches tail,
                        then { Some($then) }
                        else { None }
                    ]
                }
                else { None }
            ]
        } else {
            None
        }) {
            Some(res) => res,
            None => $else,
        }
    };

    // match expression literal
    ($literal:expr, $expr:expr, $then:block, $else:block) => {{
        if $expr == $literal $then else $else
    }};
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;
    use std::any::Any;
    use std::rc::Rc;

    type S = Rc<dyn Any>;
    type Null = ();
    type Pair = (S, S);
    type Symbol = &'static str;
    type Str = String;
    type Num = i32;

    impl Nullable for S {
        fn is_null(&self) -> bool {
            self.downcast_ref::<Null>().is_some()
        }
    }

    impl MaybePair for S {
        type Left = S;
        type Right = S;

        fn left(&self) -> Option<&S> {
            self.downcast_ref::<Pair>().map(|(x, _)| x)
        }

        fn right(&self) -> Option<&S> {
            self.downcast_ref::<Pair>().map(|(_, x)| x)
        }
    }

    impl MaybeNumber for S {
        type Number = Num;
        fn to_number(&self) -> Option<&Self::Number> {
            self.downcast_ref::<Num>()
        }
    }

    impl MaybeSymbol for S {
        fn to_symbol(&self) -> Option<&str> {
            self.downcast_ref::<Symbol>().copied()
        }
    }

    impl MaybeString for S {
        fn to_str(&self) -> Option<&str> {
            self.downcast_ref::<Str>().map(Str::as_str)
        }
        fn to_mut_str(&mut self) -> Option<&mut str> {
            unimplemented!()
        }
    }

    #[test]
    fn empty_match() {
        let result = with_sexpr_matcher! {
            match value, { }
        };
        assert_eq!(result, ());
    }

    #[test]
    fn pattern_that_never_matches() {
        let entered_branch = false;
        with_sexpr_matcher! {
            match 0, {
                ~ => { entered_branch = true; }
            }
        }
        assert!(!entered_branch);
    }

    #[test]
    fn pattern_that_never_matches_is_not_even_expanded() {
        with_sexpr_matcher! {
            match 0, {
                ~ => { foo.bar }  // would cause a compilation error
            }
        }
    }

    #[test]
    fn match_anything() {
        let entered_branch;
        with_sexpr_matcher! {
            match 0, {
                _ => { entered_branch = true; }
            }
        }
        assert!(entered_branch);
    }

    #[test]
    fn match_anything_return_bound_value() {
        let value = 42;
        let result = with_sexpr_matcher! {
            match value, {
                x => { x }
            }
        };
        assert_eq!(result, value);
    }

    #[test]
    fn match_multiple_patterns_takes_first_that_matches() {
        let result = with_sexpr_matcher! {
            match 42, {
                _ => { 1 }
                _ => { 2 }
            }
        };
        assert_eq!(result, 1);

        let result = with_sexpr_matcher! {
            match 42, {
                ~ => { 1 }
                _ => { 2 }
            }
        };
        assert_eq!(result, 2);
    }

    #[test]
    fn match_empty_list() {
        let value: S = Rc::new(());
        let did_match = with_sexpr_matcher! {
            match value, {
                () => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let value: S = Rc::new(42);
        let did_match = with_sexpr_matcher! {
            match value, {
                () => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_pair() {
        let pair: Pair = (Rc::new(1), Rc::new(2));
        let value: S = Rc::new(pair);
        let did_match = with_sexpr_matcher! {
            match value, {
                (_ . _) => { true }
                _ => { false }
            }
        };
        assert!(did_match);
    }

    #[test]
    fn mismatch_literal_pair() {
        let pair: Pair = (Rc::new(1), Rc::new(2));
        let value: S = Rc::new(pair);
        let did_match = with_sexpr_matcher! {
            match value, {
                (() . ()) => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_and_bind_pair() {
        let pair: Pair = (Rc::new(1), Rc::new(2));
        let value: S = Rc::new(pair.clone());
        let result = with_sexpr_matcher! {
            match value, {
                (car . cdr) => { Some((car, cdr)) }
                _ => { None }
            }
        };

        let result = result.unwrap();
        assert!(Rc::ptr_eq(result.0, &pair.0));
        assert!(Rc::ptr_eq(result.1, &pair.1));
    }

    #[test]
    fn match_literal_symbol() {
        let value: S = Rc::new(Symbol::from("foo"));
        let did_match = with_sexpr_matcher! {
            match value, {
                (:foo) => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let value: S = Rc::new(Symbol::from("bar"));
        let did_match = with_sexpr_matcher! {
            match value, {
                (:foo) => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_literal_symbol_arbitrary_characters() {
        let value: S = Rc::new(Symbol::from("$&[{} !]+"));
        let did_match = with_sexpr_matcher! {
            match value, {
                (:"$&[{} !]+") => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let value: S = Rc::new(Symbol::from(" + + + "));
        let did_match = with_sexpr_matcher! {
            match value, {
                (:"$&[{} !]+") => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_literals() {
        let value = "foo";
        let did_match = with_sexpr_matcher! {
            match value, {
                "foo" => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let value = "bar";
        let did_match = with_sexpr_matcher! {
            match value, {
                "foo" => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_symbol_bind_name() {
        let value: S = Rc::new(Symbol::from("foo"));
        let result = with_sexpr_matcher! {
            match value, {
                {name: Symbol} => { name }
                _ => { "" }
            }
        };
        assert_eq!(result, "foo");

        let value: S = Rc::new(Symbol::from("bar"));
        let result = with_sexpr_matcher! {
            match value, {
                {name: Symbol} => { name }
                _ => { "" }
            }
        };
        assert_eq!(result, "bar");

        let value: S = Rc::new(42);
        let result = with_sexpr_matcher! {
            match value, {
                {name: Symbol} => { name }
                _ => { "" }
            }
        };
        assert_eq!(result, "");
    }

    #[test]
    fn match_list() {
        let pair: Pair = (Rc::new(1), Rc::new(()));
        let list1: S = Rc::new(pair);

        let pair: Pair = (Rc::new(1), list1.clone());
        let list2: S = Rc::new(pair);

        let did_match = with_sexpr_matcher! {
            match list1, {
                (_) => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let did_match = with_sexpr_matcher! {
            match list2, {
                (_) => { true }
                _ => { false }
            }
        };
        assert!(!did_match);

        let did_match = with_sexpr_matcher! {
            match list2, {
                (_ _) => { true }
                _ => { false }
            }
        };
        assert!(did_match);

        let did_match = with_sexpr_matcher! {
            match list1, {
                (_ _) => { true }
                _ => { false }
            }
        };
        assert!(!did_match);
    }

    #[test]
    fn match_list_bind_name() {
        let value: S = Rc::new(42);
        let result = with_sexpr_matcher! {
            match &value, {
                {var: List} => { let _ = var; true }
                _ => { false }
            }
        };
        assert_eq!(result, false);

        let value: S = Rc::new(());
        let result = with_sexpr_matcher! {
            match &value, {
                {var: List} => { let _ = var; true }
                _ => { false }
            }
        };
        assert_eq!(result, true);
    }
}
