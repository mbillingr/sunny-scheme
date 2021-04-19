#[macro_export]
macro_rules! match_sexpr {
    ([$($rules:tt)*]) => {
        match_sexpr_rule![$($rules)*]
    };

    ([$($first:tt)*] $([$($rest:tt)*])+) => {{
        |expr| {
            match_sexpr![[$($first)*]](expr)
                $(
                    .or_else(||match_sexpr![[$($rest)*]](expr))
                )+
        }
    }};
}

macro_rules! match_sexpr_rule {
    (_ => $action:block) => {
        |_| Some($action)
    };

    ($name:ident => $action:block) => {
        |$name| Some($action)
    };
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
            self.downcast_ref::<()>().is_some()
        }
    }

    impl MaybePair for S {
        type First = S;
        type Second = S;

        fn first(&self) -> Option<&S> {
            self.downcast_ref::<Pair>().map(|(x, _)| x)
        }

        fn second(&self) -> Option<&S> {
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
    fn match_anything() {
        assert!(match_sexpr![
            [_ => { }]
        ](Rc::new(42))
        .is_some());
    }

    #[test]
    fn match_anything_and_bind() {
        assert_eq!(
            match_sexpr![
                [x => { x }]
            ](Rc::new(42)),
            Some(Rc::new(42))
        );
    }
}
