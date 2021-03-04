use log::warn;

use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

use crate::frontend::{
    ast::{Ast, AstNode},
    base_environment,
    environment::{Env, EnvBinding},
    error::{error_after, error_at, Error, Result},
    SyntaxExpander,
};

pub struct Frontend {
    env: Env,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend {
            env: base_environment(),
        }
    }

    pub fn meaning<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        if is_atom(sexpr) {
            if let Some(name) = sexpr.as_symbol() {
                let (depth, binding) = self.lookup(name);
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
                    "set!" => self.meaning_assignment(sexpr),
                    "define" => self.meaning_definition(sexpr),
                    "cons" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, Error::MissingArgument))?;
                        let arg2 = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, Error::MissingArgument))?;
                        let car = self.meaning(arg1)?;
                        let cdr = self.meaning(arg2)?;
                        Ok(Ast::cons(sexpr.map(()), car, cdr))
                    }
                    "begin" => self.meaning_sequence(sexpr.cdr().unwrap()),
                    "if" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, Error::MissingArgument))?;
                        let arg2 = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, Error::MissingArgument))?;
                        let arg3 = sexpr
                            .cadddr()
                            .ok_or_else(|| error_after(arg2, Error::MissingArgument))?;
                        let condition = self.meaning(arg1)?;
                        let consequent = self.meaning(arg2)?;
                        let alternative = self.meaning(arg3)?;
                        Ok(Ast::ifexpr(
                            sexpr.map(()),
                            condition,
                            consequent,
                            alternative,
                        ))
                    }
                    "lambda" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, Error::MissingArgument))?;
                        let body = sexpr.cddr().unwrap();

                        self.meaning_lambda(sexpr, arg1, body)
                    }
                    _ => {
                        if let Some(sx) = self.lookup_syntax(s) {
                            sx.expand(sexpr)
                        } else {
                            self.meaning_application(sexpr)
                        }
                    }
                }
            } else {
                self.meaning_application(sexpr)
            }
        }
    }

    pub fn meaning_application<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.meaning(a)?);
        }
        Ok(Ast::invoke(sexpr.map(()), args))
    }

    pub fn meaning_assignment<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let arg1 = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), Error::MissingArgument))?;
        let name = arg1
            .as_symbol()
            .ok_or_else(|| error_at(arg1, Error::ExpectedSymbol))?;
        let argval = sexpr
            .caddr()
            .ok_or_else(|| error_after(arg1, Error::MissingArgument))?;
        let value = self.meaning(argval)?;
        let (depth, binding) = self.lookup(name);
        binding.meaning_assignment(sexpr.map(()), depth, value)
    }

    pub fn meaning_definition<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let name = self.definition_name(sexpr)?;
        let value = self.definition_value(sexpr)?;
        let (depth, binding) = self.lookup(name);
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
        &mut self,
        sexpr: &'src SourceLocation<Sexpr>,
    ) -> Result<AstNode<'src>> {
        if sexpr.cadr().unwrap().is_symbol() {
            let argval = sexpr
                .caddr()
                .ok_or_else(|| error_after(sexpr.cddr().unwrap(), Error::MissingArgument))?;
            self.meaning(argval)
        } else {
            let args = sexpr.cdadr().unwrap();
            let body = sexpr.cddr().unwrap();
            self.meaning_lambda(sexpr, args, body)
        }
    }

    pub fn meaning_lambda<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
        args: &'src SourceLocation<Sexpr<'src>>,
        body: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        self.push_new_scope(args)?;
        let body = self.meaning_sequence(body)?;
        self.pop_scope();
        Ok(Ast::lambda(sexpr.map(()), args.len(), body))
    }

    pub fn meaning_sequence<'src>(
        &mut self,
        body: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let first_expr = body
            .car()
            .ok_or_else(|| error_at(body, Error::ExpectedSymbol))?;
        let rest_expr = body.cdr().unwrap();

        if rest_expr.is_null() {
            self.meaning(first_expr)
        } else {
            let first = self.meaning(first_expr)?;
            let rest = self.meaning_sequence(rest_expr)?;
            Ok(Ast::sequence(first, rest))
        }
    }

    pub fn library_definition<'src>(
        &mut self,
        libname: &'src SourceLocation<Sexpr<'src>>,
        statements: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        let mut lib_frontend = Frontend::new();

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
                        let (_, binding) = lib_frontend.ensure_global(export_name);
                        if let EnvBinding::Variable(var_idx) = binding {
                            exports.push((export_name, var_idx));
                        } else {
                            unimplemented!()
                        }
                    }
                }
                Some("begin") => {
                    body_parts.push(lib_frontend.meaning_sequence(stmt.cdr().unwrap())?)
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

    fn lookup_syntax(&mut self, name: &str) -> Option<&dyn SyntaxExpander> {
        self.env.lookup(name).map(|(_, b)| b).and_then(|b| {
            if let EnvBinding::Syntax(sx) = b {
                Some(&**sx)
            } else {
                None
            }
        })
    }

    fn lookup(&mut self, name: &str) -> (usize, EnvBinding) {
        self.env
            .lookup(name)
            .map(|(d, b)| (d, b.clone()))
            .unwrap_or_else(|| self.add_global(name.to_string()))
    }

    fn ensure_global(&mut self, name: &str) -> (usize, EnvBinding) {
        self.env
            .outermost_env()
            .lookup(name)
            .map(|(d, b)| (d, b.clone()))
            .unwrap_or_else(|| self.add_global(name.to_string()))
    }

    fn add_global(&mut self, name: String) -> (usize, EnvBinding) {
        let (depth, binding) = self.env.add_global(name);
        (depth, binding.clone())
    }

    fn push_new_scope(&mut self, vars: &SourceLocation<Sexpr>) -> Result<()> {
        let env = std::mem::replace(&mut self.env, Env::new());
        self.env = env.extend(vars)?;
        Ok(())
    }

    fn pop_scope(&mut self) {
        self.env.pop_scope();
    }
}

fn is_atom(sexpr: &SourceLocation<Sexpr>) -> bool {
    !sexpr.get_value().is_pair()
}
