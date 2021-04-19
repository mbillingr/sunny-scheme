use crate::Scm;
use sexpr_generics::prelude::*;

impl Nullable for Scm {
    fn is_null(&self) -> bool {
        Scm::is_null(self)
    }
}

impl NullFactory<Scm> for StatelessFactory {
    fn null(&mut self) -> Scm {
        Scm::null()
    }
}

impl MaybeBool for Scm {
    fn to_bool(&self) -> Option<bool> {
        Scm::as_bool(self)
    }
}

impl BoolFactory<Scm> for StatelessFactory {
    fn bool(&mut self, b: bool) -> Scm {
        Scm::bool(b)
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

impl CharFactory<Scm> for StatelessFactory {
    fn char(&mut self, _ch: char) -> Scm {
        unimplemented!()
    }

    fn ascii(&mut self, _ch: u8) -> Scm {
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

impl PairFactory<Scm> for StatelessFactory {
    fn pair(&mut self, first: Scm, second: Scm) -> Scm {
        Scm::cons(first, second)
    }
}

impl MaybeSymbol for Scm {
    fn to_symbol(&self) -> Option<&str> {
        self.as_symbol()
    }
}

impl SymbolFactory<&str, Scm> for StatelessFactory {
    fn interned_symbol(&mut self, name: &str) -> Scm {
        Scm::symbol(name)
    }

    fn uninterned_symbol(&mut self, name: &str) -> Scm {
        Scm::uninterned_symbol(name)
    }
}
