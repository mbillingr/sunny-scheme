use crate::frontend::{
    ast::{Ast, AstNode},
    error::{Error, Result},
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

macro_rules! match_sexpr {
    ([$expr:tt: $($rules:tt)*]) => {
        _match_sexpr![$expr; $($rules)*]
    };

    ([$($first:tt)*] $($rest:tt)*) => {{
        match match_sexpr![[$($first)*]] {
            Some(r) => Some(r),
            None => match_sexpr![$($rest)*],
        }
    }};
}

macro_rules! _match_sexpr {
    ($expr:tt; _ => $action:block) => {{ let _ = $expr; Some($action) }};

    ($expr:tt; $x:ident => $action:block) => {{ let $x = $expr; Some($action) }};

    ($expr:tt; () => $action:block) => {
        if $expr.is_null() {
            Some($action)
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt . $second:tt) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first => {
                _match_sexpr![cdr; $second => $action]
            }].flatten()
        } else {
            None
        }
    };

    ($expr:tt; ($first:tt $($rest:tt)*) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first => {
                _match_sexpr![cdr; ($($rest)*) => $action]
            }].flatten()
        } else {
            None
        }
    };
}

pub struct Begin;

impl SyntaxExpander for Begin {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_ . rest) => {
                Sequence.expand(rest, further)
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_ arg) => { Ok(Ast::constant(arg.clone())) }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Sequence;

impl SyntaxExpander for Sequence {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (expr) => { further.expand(expr, further) }]
            [sexpr: (expr . rest) => {
                let first = further.expand(expr, further)?;
                let rest = self.expand(rest, further)?;
                Ok(Ast::sequence(first, rest))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}

pub struct Branch;

impl SyntaxExpander for Branch {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            [sexpr: (_ condition consequence alternative) => {
                let condition = further.expand(condition, further)?;
                let consequence = further.expand(consequence, further)?;
                let alternative = further.expand(alternative, further)?;
                Ok(Ast::ifexpr(sexpr.map(()), condition, consequence, alternative))
            }]
        ]
        .unwrap_or_else(|| Err(sexpr.map(Error::InvalidForm)))
    }
}
