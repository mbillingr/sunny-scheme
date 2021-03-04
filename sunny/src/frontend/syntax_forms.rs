use crate::frontend::{
    ast::{Ast, AstNode},
    error::{error_after, error_at, Error, Result},
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
    ) -> Result<AstNode<'src>> {
        let arg = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), Error::MissingArgument))?;
        Ok(Ast::constant(arg.clone()))
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
