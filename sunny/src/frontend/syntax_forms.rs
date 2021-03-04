use crate::frontend::{
    ast::{Ast, AstNode},
    error::{error_at, Error, Expectation, Result},
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

macro_rules! match_sexpr {
    ($expr:tt; $($rules:tt)*) => {
        _match_sexpr![$expr; $($rules)*].map_err(|cte| cte.map(cte.get_value().clone().into()))
    }
}

macro_rules! _match_sexpr {
    ($expr:tt; _ => $action:block) => {{ let _ = $expr; $action }};

    ($expr:tt; $x:ident => $action:block) => {{ let $x = $expr; $action }};

    ($expr:tt; () => $action:block) => {
        if $expr.is_null() {
            $action
        } else {
            Err($expr.map(Expectation::EmptyList))
        }
    };

    ($expr:tt; ($first:tt $($rest:tt)*) => $action:block) => {
        if $expr.is_pair() {
            let car = $expr.car().unwrap();
            let cdr = $expr.cdr().unwrap();
            _match_sexpr![car; $first => {
                _match_sexpr![cdr; ($($rest)*) => $action]
            }]
        } else {
            Err($expr.map(Expectation::List))
        }
    };
}

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        match_sexpr![
            sexpr; (_ arg) => { Ok(Ast::constant(arg.clone())) }
        ]
    }
}

pub struct Sequence;

impl SyntaxExpander for Sequence {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        let body = sexpr.cdr().unwrap();

        let first_expr = body
            .car()
            .ok_or_else(|| error_at(body, Error::ExpectedSymbol))?;
        let rest_expr = body.cdr().unwrap();

        if rest_expr.is_null() {
            further.expand(first_expr, further)
        } else {
            let first = further.expand(first_expr, further)?;
            // expand `body` instead of `rest_expr` because the expander
            // ignores the first argument.
            let rest = self.expand(body, further)?;
            Ok(Ast::sequence(first, rest))
        }
    }
}
