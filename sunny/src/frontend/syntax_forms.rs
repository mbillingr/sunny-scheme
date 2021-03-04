use crate::frontend::{
    ast::{Ast, AstNode},
    error::{error_after, Error, Result},
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

pub struct Quotation;

impl SyntaxExpander for Quotation {
    fn expand<'src>(&self, sexpr: &'src SourceLocation<Sexpr<'src>>) -> Result<AstNode<'src>> {
        let arg = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), Error::MissingArgument))?;
        Ok(Ast::constant(arg.clone()))
    }
}
