pub trait CxR {
    type Result: CxR<Result = Self::Result>;

    fn car(&self) -> Option<&Self::Result>;
    fn cdr(&self) -> Option<&Self::Result>;

    fn caar(&self) -> Option<&Self::Result> {
        self.car()?.car()
    }

    fn cadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()
    }

    fn cdar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()
    }

    fn cddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()
    }

    fn caaar(&self) -> Option<&Self::Result> {
        self.car()?.car()?.car()
    }

    fn caadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()?.car()
    }

    fn cadar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()?.car()
    }

    fn caddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()?.car()
    }

    fn cdaar(&self) -> Option<&Self::Result> {
        self.car()?.car()?.cdr()
    }

    fn cdadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()?.cdr()
    }

    fn cddar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()?.cdr()
    }

    fn cdddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()?.cdr()
    }

    fn caaaar(&self) -> Option<&Self::Result> {
        self.caar()?.caar()
    }

    fn caaadr(&self) -> Option<&Self::Result> {
        self.cadr()?.caar()
    }

    fn caadar(&self) -> Option<&Self::Result> {
        self.cdar()?.caar()
    }

    fn caaddr(&self) -> Option<&Self::Result> {
        self.cddr()?.caar()
    }

    fn cadaar(&self) -> Option<&Self::Result> {
        self.caar()?.cadr()
    }

    fn cadadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cadr()
    }

    fn caddar(&self) -> Option<&Self::Result> {
        self.cdar()?.cadr()
    }

    fn cadddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cadr()
    }

    fn cdaaar(&self) -> Option<&Self::Result> {
        self.caar()?.cdar()
    }

    fn cdaadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cdar()
    }

    fn cdadar(&self) -> Option<&Self::Result> {
        self.cdar()?.cdar()
    }

    fn cdaddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cdar()
    }

    fn cddaar(&self) -> Option<&Self::Result> {
        self.caar()?.cddr()
    }

    fn cddadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cddr()
    }

    fn cdddar(&self) -> Option<&Self::Result> {
        self.cdar()?.cddr()
    }

    fn cddddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cddr()
    }
}