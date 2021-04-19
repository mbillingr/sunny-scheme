use crate::Scm;
use sexpr_generics::core_traits::{MaybeBool, MaybeChar, MaybeNumber, MaybePair, Nullable};
use sexpr_generics::factory_traits::{NumberFactory, StatelessFactory};

impl Nullable for Scm {
    fn is_null(&self) -> bool {
        Scm::is_null(self)
    }
}

impl MaybeBool for Scm {
    fn to_bool(&self) -> Option<bool> {
        Scm::as_bool(self)
    }
}

impl MaybeChar for Scm {
    fn to_char(&self) -> Option<char> {
        unimplemented!()
    }

    fn to_ascii(&self) -> Option<u8> {
        unimplemented!()
    }
}

impl MaybeNumber for Scm {
    type Number = i64;
    fn to_number(&self) -> Option<&i64> {
        Scm::as_number(self)
    }
}

impl NumberFactory<Scm> for StatelessFactory {
    fn number(&mut self, n: i64) -> Scm {
        Scm::int(n)
    }
    fn raw_zero(&mut self) -> i64 {
        0
    }
    fn raw_one(&mut self) -> i64 {
        1
    }
}

impl MaybePair for Scm {
    type First = Scm;
    type Second = Scm;

    fn first(&self) -> Option<&Self::First> {
        self.as_pair().map(|(car, _)| car)
    }

    fn second(&self) -> Option<&Self::First> {
        self.as_pair().map(|(_, cdr)| cdr)
    }
}
