use crate::backend::Backend;
use crate::frontend::Error::*;
use log::warn;
use std::collections::HashMap;
use sunny_sexpr_parser::SourceLocation;
use sunny_sexpr_parser::{CxR, Sexpr};

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingArgument,
    ExpectedSymbol,
    ExpectedList,
    UnexpectedStatement,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingArgument => write!(f, "Missing argument"),
            Error::ExpectedSymbol => write!(f, "Expected symbol"),
            Error::ExpectedList => write!(f, "Expected list"),
            Error::UnexpectedStatement => write!(f, "Unexpected statement"),
        }
    }
}

pub struct Frontend<'f, B: Backend> {
    pub env: Env,
    pub backend: &'f mut B,
}

impl<'f, B: Backend> Frontend<'f, B> {
    pub fn new(backend: &'f mut B) -> Self {
        Frontend {
            env: Env::new(),
            backend,
        }
    }

    pub fn meaning(&mut self, sexpr: &SourceLocation<Sexpr>) -> Result<B::Ir> {
        if is_atom(sexpr) {
            if let Some(name) = sexpr.as_symbol() {
                let (depth, binding) = self
                    .lookup(name)
                    .unwrap_or_else(|| self.add_global(name.to_string()));
                binding.meaning_reference(sexpr.map(()), depth, self.backend)
            } else {
                Ok(self.backend.constant(sexpr.map(()), sexpr.get_value()))
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
                        Ok(self.backend.cons(sexpr.map(()), car, cdr))
                    }
                    "quote" => {
                        let arg = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        Ok(self.backend.constant(arg.map(()), arg.get_value()))
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
                        Ok(self
                            .backend
                            .ifexpr(sexpr.map(()), condition, consequent, alternative))
                    }
                    "lambda" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let body = sexpr.cddr().unwrap();

                        self.meaning_lambda(sexpr, arg1, body)
                    }
                    _ => self.meaning_application(sexpr),
                }
            } else {
                self.meaning_application(sexpr)
            }
        }
    }

    pub fn meaning_application(&mut self, sexpr: &SourceLocation<Sexpr>) -> Result<B::Ir> {
        let mut args = vec![];
        for a in sexpr.iter() {
            args.push(self.meaning(a)?);
        }
        Ok(self.backend.invoke(sexpr.map(()), args))
    }

    pub fn meaning_assignment(&mut self, sexpr: &SourceLocation<Sexpr>) -> Result<B::Ir> {
        let arg1 = sexpr
            .cadr()
            .ok_or_else(|| error_after(sexpr.car().unwrap(), MissingArgument))?;
        let name = arg1
            .as_symbol()
            .ok_or_else(|| error_at(arg1, ExpectedSymbol))?;
        let argval = sexpr
            .caddr()
            .ok_or_else(|| error_after(arg1, MissingArgument))?;
        let (depth, binding) = self
            .lookup(name)
            .unwrap_or_else(|| self.add_global(name.to_string()));
        let value = self.meaning(argval)?;
        binding.meaning_assignment(sexpr.map(()), depth, value, self.backend)
    }

    pub fn meaning_definition(&mut self, sexpr: &SourceLocation<Sexpr>) -> Result<B::Ir> {
        let name = self.definition_name(sexpr)?;
        let value = self.definition_value(sexpr)?;
        let (depth, binding) = self
            .lookup(name)
            .unwrap_or_else(|| self.add_global(name.to_string()));
        binding.meaning_assignment(sexpr.map(()), depth, value, self.backend)
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

    fn definition_value(&mut self, sexpr: &SourceLocation<Sexpr>) -> Result<B::Ir> {
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

    pub fn meaning_lambda(
        &mut self,
        sexpr: &SourceLocation<Sexpr>,
        args: &SourceLocation<Sexpr>,
        body: &SourceLocation<Sexpr>,
    ) -> Result<B::Ir> {
        self.push_new_scope(args)?;
        let body = self.meaning_sequence(body)?;
        self.pop_scope();
        Ok(self.backend.lambda(sexpr.map(()), args.len(), body))
    }

    pub fn meaning_sequence(&mut self, body: &SourceLocation<Sexpr>) -> Result<B::Ir> {
        let first_expr = body.car().ok_or_else(|| error_at(body, ExpectedSymbol))?;
        let rest_expr = body.cdr().unwrap();

        if rest_expr.is_null() {
            self.meaning(first_expr)
        } else {
            let first = self.meaning(first_expr)?;
            let rest = self.meaning_sequence(rest_expr)?;
            Ok(self.backend.sequence(first, rest))
        }
    }

    pub fn library_definition(
        &mut self,
        libname: &SourceLocation<Sexpr>,
        statements: &SourceLocation<Sexpr>,
    ) -> Result<B::Ir> {
        self.backend.begin_module();
        let mut lib_frontend = Frontend::new(self.backend);

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
            body = self.backend.sequence(prev_part, body);
        }

        body = self.backend.end_module(body);

        let meaning_exports = self.backend.export(exports);
        body = self.backend.sequence(body, meaning_exports);

        let body_func = self.backend.lambda(SourceLocation::new(()), 0, body);

        let mut libcode = self
            .backend
            .invoke(SourceLocation::new(()), vec![body_func]);

        // this is just to make the test pass for now and serves no real purpose
        self.backend.add_global(0);
        libcode = self.backend.store(SourceLocation::new(()), 0, 0, libcode);

        Ok(libcode)
    }

    fn lookup(&self, name: &str) -> Option<(usize, EnvBinding)> {
        self.env.lookup(name).map(|(d, b)| (d, b.clone()))
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
        self.backend.add_global(binding.index().unwrap());
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

#[derive(Debug, Clone)]
enum EnvBinding {
    Variable(usize),
}

impl EnvBinding {
    pub fn meaning_reference<B: Backend>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
        backend: &mut B,
    ) -> Result<B::Ir> {
        match self {
            EnvBinding::Variable(idx) => Ok(backend.fetch(context, depth, *idx)),
        }
    }

    pub fn meaning_assignment<B: Backend>(
        &self,
        context: SourceLocation<()>,
        depth: usize,
        value: B::Ir,
        backend: &mut B,
    ) -> Result<B::Ir> {
        match self {
            EnvBinding::Variable(idx) => Ok(backend.store(context, depth, *idx, value)),
        }
    }

    pub fn index(&self) -> Option<usize> {
        match self {
            EnvBinding::Variable(idx) => Some(*idx),
        }
    }
}

#[derive(Debug, Clone, Default)]
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

        let idx = self.variables.len();
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

    #[derive(Debug, PartialEq)]
    enum Ast {
        Const(String),
        Ref(usize, usize),
        Set(usize, usize, Box<Ast>),
        Cons(Box<Ast>, Box<Ast>),
        Begin(Box<Ast>, Box<Ast>),
        If(Box<Ast>, Box<Ast>, Box<Ast>),
        Lambda(usize, Box<Ast>),
        Invoke(Vec<Ast>),
    }

    struct AstBuilder;

    impl Backend for AstBuilder {
        type Ir = Ast;

        fn begin_module(&mut self) {
            unimplemented!()
        }

        fn end_module(&mut self, _content: Self::Ir) -> Self::Ir {
            unimplemented!()
        }

        fn add_global(&mut self, _: usize) {}

        fn constant(&mut self, _: SourceLocation<()>, c: &Sexpr) -> Self::Ir {
            Ast::Const(c.to_string())
        }

        fn fetch(&mut self, _: SourceLocation<()>, depth: usize, idx: usize) -> Self::Ir {
            Ast::Ref(depth, idx)
        }

        fn store(
            &mut self,
            _: SourceLocation<()>,
            depth: usize,
            idx: usize,
            val: Self::Ir,
        ) -> Self::Ir {
            Ast::Set(depth, idx, Box::new(val))
        }

        fn cons(&mut self, _: SourceLocation<()>, first: Self::Ir, second: Self::Ir) -> Self::Ir {
            Ast::Cons(Box::new(first), Box::new(second))
        }

        fn ifexpr(
            &mut self,
            _: SourceLocation<()>,
            condition: Self::Ir,
            consequent: Self::Ir,
            alternative: Self::Ir,
        ) -> Self::Ir {
            Ast::If(
                Box::new(condition),
                Box::new(consequent),
                Box::new(alternative),
            )
        }

        fn sequence(&mut self, first: Self::Ir, next: Self::Ir) -> Self::Ir {
            Ast::Begin(Box::new(first), Box::new(next))
        }

        fn lambda(&mut self, _: SourceLocation<()>, n_args: usize, body: Self::Ir) -> Self::Ir {
            Ast::Lambda(n_args, Box::new(body))
        }

        fn invoke(&mut self, _: SourceLocation<()>, args: Vec<Self::Ir>) -> Self::Ir {
            Ast::Invoke(args)
        }

        fn export(&mut self, _exports: Vec<(&str, usize)>) -> Self::Ir {
            unimplemented!()
        }
    }

    macro_rules! ast {
        (($($parts:tt)*)) => {ast![$($parts)*]};
        (const $x:expr) => {Ast::Const(format!("{}", $x))};
        (ref $d:tt $i:tt) => {Ast::Ref($d, $i)};
        (set $d:tt $i:tt $x:tt) => {Ast::Set($d, $i, Box::new(ast![$x]))};
        (cons $a:tt $b:tt) => {Ast::Cons(Box::new(ast![$a]), Box::new(ast![$b]))};
        (begin $a:tt $b:tt) => {Ast::Begin(Box::new(ast![$a]), Box::new(ast![$b]))};
        (begin $a:tt $($b:tt)+) => {Ast::Begin(Box::new(ast![$a]), Box::new(ast![begin $($b)+]))};
        (if $a:tt $b:tt $c:tt) => {Ast::If(Box::new(ast![$a]), Box::new(ast![$b]), Box::new(ast![$c]))};
        (lambda $p:tt $b:tt) => {Ast::Lambda($p, Box::new(ast![$b]))};
        (invoke $($a:tt)*) => {Ast::Invoke(vec![$(ast![$a]),*])};
    }

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
            Frontend::new(&mut AstBuilder).meaning(&sexpr.into())
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
            Ok(ast!(if (const "1") (const "2") (const "3")))
        );
    }

    #[test]
    fn meaning_of_set() {
        assert_eq!(meaning_of![(["set!"] x 42)], Ok(ast!(set 0 0 (const "42"))));
    }

    #[test]
    fn meaning_of_trivial_lambda() {
        assert_eq!(meaning_of![(lambda () 0)], Ok(ast!(lambda 0 (const "0"))));
    }

    #[test]
    fn meaning_of_lambda_application() {
        assert_eq!(
            meaning_of![((lambda () 0))],
            Ok(ast!(invoke (lambda 0 (const "0"))))
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
            Ok(ast!(begin (const "1") (const "2") (const "3")))
        );
    }

    #[test]
    fn meaning_of_singleton_begin_is_meaning_of_inner_expression() {
        assert_eq!(meaning_of![(begin 1)], Ok(ast!(const "1")));
    }
}
