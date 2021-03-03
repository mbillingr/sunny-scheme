use crate::ast::{Ast, AstNode};
use crate::frontend::Error::*;
use log::warn;
use maplit::hashmap;
use std::collections::HashMap;
use std::rc::Rc;
use sunny_sexpr_parser::SourceLocation;
use sunny_sexpr_parser::{CxR, Sexpr};

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingArgument,
    ExpectedSymbol,
    ExpectedList,
    UnexpectedStatement,
    SyntaxAsValue,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingArgument => write!(f, "Missing argument"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::UnexpectedStatement => write!(f, "Unexpected statement"),
            Error::SyntaxAsValue => write!(f, "Syntax used as value"),
        }
    }
}

pub struct Frontend {
    env: Env,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend {
            env: Env::new_base(),
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
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        self.library_definition(libname, sexpr.cddr().unwrap())
                    }
                    "set!" => self.meaning_assignment(sexpr),
                    "define" => self.meaning_definition(sexpr),
                    "cons" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let arg2 = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, MissingArgument))?;
                        let car = self.meaning(arg1)?;
                        let cdr = self.meaning(arg2)?;
                        Ok(Ast::cons(sexpr.map(()), car, cdr))
                    }
                    "begin" => self.meaning_sequence(sexpr.cdr().unwrap()),
                    "if" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let arg2 = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, MissingArgument))?;
                        let arg3 = sexpr
                            .cadddr()
                            .ok_or_else(|| error_after(arg2, MissingArgument))?;
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
                            .ok_or_else(|| error_after(first, MissingArgument))?;
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
            .ok_or_else(|| error_after(sexpr.car().unwrap(), MissingArgument))?;
        let name = arg1
            .as_symbol()
            .ok_or_else(|| error_at(arg1, ExpectedSymbol))?;
        let argval = sexpr
            .caddr()
            .ok_or_else(|| error_after(arg1, MissingArgument))?;
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
            .ok_or_else(|| error_after(sexpr.car().unwrap(), MissingArgument))?;
        if let Some(name) = arg1.as_symbol() {
            Ok(name)
        } else if let Some(first) = arg1.car() {
            first
                .as_symbol()
                .ok_or_else(|| error_at(arg1, ExpectedSymbol))
        } else {
            Err(error_at(arg1, ExpectedSymbol))
        }
    }

    fn definition_value<'src>(
        &mut self,
        sexpr: &'src SourceLocation<Sexpr>,
    ) -> Result<AstNode<'src>> {
        if sexpr.cadr().unwrap().is_symbol() {
            let argval = sexpr
                .caddr()
                .ok_or_else(|| error_after(sexpr.cddr().unwrap(), MissingArgument))?;
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
        let first_expr = body.car().ok_or_else(|| error_at(body, ExpectedSymbol))?;
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
                            .ok_or_else(|| error_at(export_item, ExpectedSymbol))?;
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
        let parent = self.env.parent.take().unwrap();
        self.env = *parent;
    }
}

fn error_at<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map(error.into())
}

fn error_after<T>(sexpr: &SourceLocation<T>, error: impl Into<Error>) -> SourceLocation<Error> {
    sexpr.map_after(error.into())
}

fn is_atom(sexpr: &SourceLocation<Sexpr>) -> bool {
    !sexpr.get_value().is_pair()
}

pub trait SyntaxExpander {
    fn expand<'src>(&self, sexpr: &'src SourceLocation<Sexpr<'src>>) -> Result<AstNode<'src>>;
}

struct Quotation;
impl SyntaxExpander for Quotation {
    fn expand<'src>(&self, sexpr: &'src SourceLocation<Sexpr<'src>>) -> Result<AstNode<'src>> {
        let arg = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), MissingArgument))?;
        Ok(Ast::constant(arg.clone()))
    }
}

#[derive(Clone)]
enum EnvBinding {
    Variable(usize),
    Syntax(Rc<dyn SyntaxExpander>),
}

impl EnvBinding {
    pub fn meaning_reference<'src>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::fetch(context, depth, *idx)),
            EnvBinding::Syntax(_) => Err(context.map(Error::SyntaxAsValue)),
        }
    }

    pub fn meaning_assignment<'src>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
        value: AstNode<'src>,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(idx) => Ok(Ast::store(context, depth, *idx, value)),
            EnvBinding::Syntax(_) => Err(context.map(Error::SyntaxAsValue)),
        }
    }

    pub fn expand_syntax<'src>(
        &self,
        sexpr: &'src SourceLocation<Sexpr<'src>>,
    ) -> Result<AstNode<'src>> {
        match self {
            EnvBinding::Variable(_) => panic!("Attempt to expand variable as syntax"),
            EnvBinding::Syntax(x) => x.expand(sexpr),
        }
    }

    pub fn index(&self) -> Option<usize> {
        match self {
            EnvBinding::Variable(idx) => Some(*idx),
            EnvBinding::Syntax(_) => None,
        }
    }

    pub fn is_variable(&self) -> bool {
        match self {
            EnvBinding::Variable(_) => true,
            EnvBinding::Syntax(_) => false,
        }
    }
}

impl<T: 'static + SyntaxExpander> From<T> for EnvBinding {
    fn from(se: T) -> Self {
        EnvBinding::Syntax(Rc::new(se))
    }
}

#[derive(Default)]
pub struct Env {
    parent: Option<Box<Env>>,
    variables: HashMap<String, EnvBinding>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            parent: None,
            variables: HashMap::new(),
        }
    }

    pub fn new_base() -> Self {
        let variables = hashmap![
            "quote".to_string() => EnvBinding::from(Quotation),
        ];
        Env {
            parent: None,
            variables,
        }
    }

    fn from_sexpr(vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut variables = HashMap::new();

        for v in vars.iter() {
            let name = v
                .as_symbol()
                .ok_or_else(|| error_at(v, ExpectedSymbol))?
                .to_string();

            let idx = variables.len();
            variables.insert(name, EnvBinding::Variable(idx));
        }

        Ok(Env {
            parent: None,
            variables,
        })
    }

    fn extend(self, vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut env = Env::from_sexpr(vars)?;
        env.parent = Some(Box::new(self));
        Ok(env)
    }

    fn lookup(&self, name: &str) -> Option<(usize, &EnvBinding)> {
        self.variables.get(name).map(|b| (0, b)).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.lookup(name))
                .map(|(depth, b)| (1 + depth, b))
        })
    }

    fn add_global(&mut self, name: String) -> (usize, &EnvBinding) {
        if let Some(p) = &mut self.parent {
            let (depth, idx) = p.add_global(name);
            return (1 + depth, idx);
        }

        let idx = self.variables.values().filter(|b| b.is_variable()).count();
        self.variables
            .insert(name.clone(), EnvBinding::Variable(idx));
        (0, self.variables.get(&name).unwrap())
    }

    fn outermost_env(&self) -> &Env {
        if let Some(ref p) = self.parent {
            p.outermost_env()
        } else {
            self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! sexpr {
        ($t:ty:()) => { <$t>::nil() };

        ($t:ty:($x:tt $($rest:tt)*)) => {
            <$t>::cons(
                sexpr![$t:$x],
                sexpr![$t:($($rest)*)],
            )
        };

        ($t:ty:[$x:expr]) => { <$t>::symbol($x) };

        ($t:ty:$x:ident) => { <$t>::symbol(stringify!($x)) };

        ($t:ty:$x:expr) => { <$t>::from($x) };
    }

    macro_rules! meaning_of {
        ($expr:tt) => {{
            let sexpr = sexpr![Sexpr: $expr];
            Frontend::new().meaning(&sexpr.into())
        }};
    }

    #[test]
    fn meaning_of_constant() {
        assert_eq!(meaning_of![1], Ok(ast!(const 1)));
    }

    #[test]
    fn meaning_of_symbol() {
        assert_eq!(meaning_of![x], Ok(ast!(ref 0 0)));
    }

    #[test]
    fn meaning_of_cons() {
        assert_eq!(meaning_of![(cons 1 2)], Ok(ast!(cons (const 1) (const 2))));
    }

    #[test]
    fn meaning_of_quote() {
        assert_eq!(meaning_of![(quote x)], Ok(ast!(const "x")));
    }

    #[test]
    fn meaning_of_if() {
        assert_eq!(
            meaning_of![(if 1 2 3)],
            Ok(ast!(if (const 1) (const 2) (const 3)))
        );
    }

    #[test]
    fn meaning_of_set() {
        assert_eq!(meaning_of![(["set!"] x 42)], Ok(ast!(set 0 0 (const 42))));
    }

    #[test]
    fn meaning_of_trivial_lambda() {
        assert_eq!(meaning_of![(lambda () 0)], Ok(ast!(lambda 0 (const 0))));
    }

    #[test]
    fn meaning_of_lambda_application() {
        assert_eq!(
            meaning_of![((lambda () 1))],
            Ok(ast!(invoke (lambda 0 (const 1))))
        );
    }

    #[test]
    fn meaning_of_variable_application() {
        assert_eq!(meaning_of![(foo)], Ok(ast!(invoke (ref 0 0))));
    }

    #[test]
    fn meaning_of_lambda_identity() {
        assert_eq!(meaning_of![(lambda (x) x)], Ok(ast!(lambda 1 (ref 0 0))));
    }

    #[test]
    fn meaning_of_global_ref_in_lambda() {
        assert_eq!(meaning_of![(lambda () x)], Ok(ast!(lambda 0 (ref 1 0))));
    }

    #[test]
    fn meaning_of_begin() {
        assert_eq!(
            meaning_of![(begin 1 2 3)],
            Ok(ast!(begin (const 1) (const 2) (const 3)))
        );
    }

    #[test]
    fn meaning_of_singleton_begin_is_meaning_of_inner_expression() {
        assert_eq!(meaning_of![(begin 1)], Ok(ast!(const 1)));
    }
}
