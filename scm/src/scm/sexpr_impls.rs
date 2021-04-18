use crate::Scm;
use sexpr_generics::core_traits::{MaybeBool, MaybeChar, MaybePair, Nullable};
use sexpr_generics::cxr::CxR;

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

impl CxR for Scm {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.as_pair().map(|(car, _)| car)
    }

    fn cdr(&self) -> Option<&Self> {
        self.as_pair().map(|(_, cdr)| cdr)
    }
}
