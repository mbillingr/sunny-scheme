use crate::Scm;
use sexpr_generics::cxr::CxR;

impl CxR for Scm {
    type Result = Self;

    fn car(&self) -> Option<&Self> {
        self.as_pair().map(|(car, _)| car)
    }

    fn cdr(&self) -> Option<&Self> {
        self.as_pair().map(|(_, cdr)| cdr)
    }
}
