use crate::frontend::syntax_forms::Sequence;
use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_after, error_at, Error, Result},
    SyntaxExpander,
};
use log::warn;
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
                    "define-library" => {
                        let libname = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, Error::MissingArgument))?;
                        self.library_definition(libname, sexpr.cddr().unwrap())
                    }
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
    pub fn meaning_application<'src>(
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

    pub fn meaning_definition<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        let name = self.definition_name(sexpr)?;
        let value = self.definition_value(sexpr, env)?;
        let (depth, binding) = env.lookup_or_insert_global(name);
        binding.meaning_assignment(sexpr.map(()), depth, value)
    }

    fn definition_name<'a>(&self, sexpr: &'a SourceLocation<Sexpr>) -> Result<&'a str> {
        let arg1 = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), Error::MissingArgument))?;
        if let Some(name) = arg1.as_symbol() {
            Ok(name)
        } else if let Some(first) = arg1.car() {
            first
                .as_symbol()
                .ok_or_else(|| error_at(arg1, Error::ExpectedSymbol))
        } else {
            Err(error_at(arg1, Error::ExpectedSymbol))
        }
    }

    fn definition_value<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr>,
        env: &Env,
    ) -> Result<AstNode<'src>> {
        if sexpr.cadr().unwrap().is_symbol() {
            let argval = sexpr
                .caddr()
                .ok_or_else(|| error_after(sexpr.cddr().unwrap(), Error::MissingArgument))?;
            self.expand(argval, self, env)
        } else {
            let args = sexpr.cdadr().unwrap();
            let body = sexpr.cddr().unwrap();

            let body_env = env.extend(args)?;
            let body = Sequence.expand(body, self, &body_env)?;

            Ok(Ast::lambda(sexpr.map(()), args.len(), body))
        }
    }

    pub fn library_definition<'src>(
        &self,
        libname: &'src SourceLocation<Sexpr<'src>>,
        statements: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let lib_env = base_environment();

        let mut exports = vec![];
        let mut body_parts = vec![];
        for stmt in statements.iter() {
            match stmt.car().and_then(|s| s.as_symbol()) {
                Some("import") => warn!("Ignoring (import ...) statement in library definition"),
                Some("export") => {
                    for export_item in stmt.cdr().unwrap().iter() {
                        let export_name = export_item
                            .as_symbol()
                            .ok_or_else(|| error_at(export_item, Error::ExpectedSymbol))?;
                        let (_, binding) = lib_env.ensure_global(export_name);
                        if let EnvBinding::Variable(var_idx) = binding {
                            exports.push((export_name, var_idx));
                        } else {
                            unimplemented!()
                        }
                    }
                }
                Some("begin") => {
                    body_parts.push(Sequence.expand(stmt.cdr().unwrap(), self, &lib_env)?)
                }
                _ => return Err(error_at(stmt, Error::UnexpectedStatement)),
            }
        }

        let mut body = body_parts.pop().expect("Empty library body");
        while let Some(prev_part) = body_parts.pop() {
            body = Ast::sequence(prev_part, body);
        }

        let meaning_exports = Ast::export(exports);
        body = Ast::sequence(body, meaning_exports);

        Ok(Ast::module(body))
    }
}

fn is_atom(sexpr: &SourceLocation<Sexpr>) -> bool {
    !sexpr.get_value().is_pair()
}
