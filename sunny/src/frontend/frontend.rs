use crate::frontend::{
    ast::{Ast, AstNode},
    environment::Env,
    error::Result,
    SyntaxExpander,
};
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

pub struct Frontend;

impl SyntaxExpander for Frontend {
    fn expand<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        _further: &dyn SyntaxExpander,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        if is_atom(sexpr) {
            if let Some(name) = sexpr.as_symbol() {
                let (depth, binding) = env.lookup_or_insert_global(name);
                binding.meaning_reference(sexpr.map(()), depth)
            } else {
                Ok(Ast::constant(sexpr.clone()))
            }
        } else {
            let first = sexpr.car().unwrap();
            if let Some(s) = first.as_symbol() {
                match s {
                    _ => {
                        if let Some(sx) = env.lookup_syntax(s) {
                            sx.expand(sexpr, self, env)
                        } else {
                            self.meaning_application(sexpr, env)
                        }
                    }
                }
            } else {
                self.meaning_application(sexpr, env)
            }
        }
    }
}

impl Frontend {
    fn meaning_application<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.expand(a, self, env)?);
        }
        Ok(Ast::invoke(sexpr.map(()), args))
    }
}

fn is_atom(sexpr: &SourceLocation<Sexpr>) -> bool {
    !sexpr.get_value().is_pair()
}
