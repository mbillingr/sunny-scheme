use crate::backend::Backend;
use crate::frontend::Error::*;
use sunny_sexpr_parser::{Context, CxR, Sexpr};

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownSpecialForm(String),
    MissingArgument,
    ExpectedSymbol,
    ExpectedList,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnknownSpecialForm(s) => write!(f, "Unknown special form {}.", s),
            Error::MissingArgument => write!(f, "Missing argument."),
            Error::ExpectedSymbol => write!(f, "Expected symbol."),
            Error::ExpectedList => write!(f, "Expected list."),
        }
    }
}

pub struct Frontend {
    globals: Vec<String>,
    locals: Option<Env>,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend {
            globals: vec![],
            locals: None,
        }
    }

    pub fn meaning<B: Backend>(
        &mut self,
        sexpr: &Context<Sexpr>,
        backend: &mut B,
    ) -> Result<B::Output> {
        if is_atom(sexpr) {
            if let Some(name) = sexpr.as_symbol() {
                let (depth, idx) = self
                    .lookup(name)
                    .unwrap_or_else(|| self.add_global(name.to_string(), backend));
                Ok(backend.fetch(depth, idx))
            } else {
                Ok(backend.constant(sexpr.get_value()))
            }
        } else {
            let first = sexpr.car().unwrap();
            if let Some(s) = first.as_symbol() {
                match s {
                    "set!" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let name = arg1
                            .as_symbol()
                            .ok_or_else(|| error_at(arg1, ExpectedSymbol))?;
                        let argval = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, MissingArgument))?;
                        let (depth, idx) = self
                            .lookup(name)
                            .unwrap_or_else(|| self.add_global(name.to_string(), backend));
                        let value = self.meaning(argval, backend)?;
                        Ok(backend.store(depth, idx, value))
                    }
                    "cons" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let arg2 = sexpr
                            .caddr()
                            .ok_or_else(|| error_after(arg1, MissingArgument))?;
                        let car = self.meaning(arg1, backend)?;
                        let cdr = self.meaning(arg2, backend)?;
                        Ok(backend.cons(car, cdr))
                    }
                    "quote" => {
                        let arg = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        Ok(backend.constant(arg.get_value()))
                    }
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
                        let condition = self.meaning(arg1, backend)?;
                        let consequent = self.meaning(arg2, backend)?;
                        let alternative = self.meaning(arg3, backend)?;
                        Ok(backend.ifexpr(condition, consequent, alternative))
                    }
                    "lambda" => {
                        let arg1 = sexpr
                            .cadr()
                            .ok_or_else(|| error_after(first, MissingArgument))?;
                        let body = sexpr.cddr().unwrap();

                        self.push_new_scope(arg1)?;
                        let body = self.meaning_sequence(body, backend)?;
                        self.pop_scope();

                        Ok(backend.lambda(arg1.len(), body))
                    }
                    _ => Err(error_at(sexpr, Error::UnknownSpecialForm(s.to_string()))),
                }
            } else {
                unimplemented!()
            }
        }
    }

    pub fn meaning_sequence<B: Backend>(
        &mut self,
        body: &Context<Sexpr>,
        backend: &mut B,
    ) -> Result<B::Output> {
        let first_expr = body.car().ok_or_else(|| error_at(body, ExpectedSymbol))?;
        let rest_expr = body.cdr().unwrap();

        if rest_expr.is_null() {
            self.meaning(first_expr, backend)
        } else {
            let first = self.meaning(first_expr, backend)?;
            let rest = self.meaning_sequence(rest_expr, backend)?;
            Ok(backend.sequence(first, rest))
        }
    }

    fn lookup(&self, name: &str) -> Option<(usize, usize)> {
        self.globals
            .iter()
            .position(|gvar| gvar == name)
            .map(|idx| (self.current_lexical_depth(), idx))
    }

    fn add_global<B: Backend>(&mut self, name: String, backend: &mut B) -> (usize, usize) {
        let idx = self.globals.len();
        self.globals.push(name);
        backend.add_global(idx);
        (self.current_lexical_depth(), idx)
    }

    fn current_lexical_depth(&self) -> usize {
        0
    }

    fn push_new_scope(&mut self, vars: &Context<Sexpr>) -> Result<()> {
        let mut env = Env::from_sexpr(vars)?;
        env.parent = self.locals.take().map(Box::new);
        self.locals = Some(env);
        Ok(())
    }

    fn pop_scope(&mut self) {
        let env = self.locals.take().unwrap();
        self.locals = env.parent.map(|p| *p)
    }
}

fn error_at<T>(sexpr: &Context<T>, error: impl Into<Error>) -> Context<Error> {
    sexpr.map(error.into())
}

fn error_after<T>(sexpr: &Context<T>, error: impl Into<Error>) -> Context<Error> {
    sexpr.map_after(error.into())
}

fn is_atom(sexpr: &Context<Sexpr>) -> bool {
    !sexpr.get_value().is_pair()
}

struct Env {
    parent: Option<Box<Env>>,
    variables: Vec<String>,
}

impl Env {
    fn from_sexpr(vars: &Context<Sexpr>) -> Result<Self> {
        let mut variables = vec![];

        for v in vars.iter() {
            variables.push(
                v.as_symbol()
                    .ok_or_else(|| error_at(v, ExpectedSymbol))?
                    .to_string(),
            )
        }

        Ok(Env {
            parent: None,
            variables,
        })
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
        If(Box<Ast>, Box<Ast>, Box<Ast>),
        Lambda(usize, Box<Ast>),
    }

    struct AstBuilder;

    impl Backend for AstBuilder {
        type Output = Ast;

        fn add_global(&mut self, _: usize) {}

        fn constant(&mut self, c: &Sexpr) -> Self::Output {
            Ast::Const(c.to_string())
        }

        fn fetch(&mut self, depth: usize, idx: usize) -> Self::Output {
            Ast::Ref(depth, idx)
        }

        fn store(&mut self, depth: usize, idx: usize, val: Self::Output) -> Self::Output {
            Ast::Set(depth, idx, Box::new(val))
        }

        fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output {
            Ast::Cons(Box::new(first), Box::new(second))
        }

        fn ifexpr(
            &mut self,
            condition: Self::Output,
            consequent: Self::Output,
            alternative: Self::Output,
        ) -> Self::Output {
            Ast::If(
                Box::new(condition),
                Box::new(consequent),
                Box::new(alternative),
            )
        }

        fn sequence(&mut self, _first: Self::Output, _next: Self::Output) -> Self::Output {
            unimplemented!()
        }

        fn lambda(&mut self, n_args: usize, body: Self::Output) -> Self::Output {
            Ast::Lambda(n_args, Box::new(body))
        }
    }

    macro_rules! ast {
        (($($parts:tt)*)) => {ast![$($parts)*]};
        (const $x:expr) => {Ast::Const(format!("{}", $x))};
        (ref $d:tt $i:tt) => {Ast::Ref($d, $i)};
        (set $d:tt $i:tt $x:tt) => {Ast::Set($d, $i, Box::new(ast![$x]))};
        (cons $a:tt $b:tt) => {Ast::Cons(Box::new(ast![$a]), Box::new(ast![$b]))};
        (if $a:tt $b:tt $c:tt) => {Ast::If(Box::new(ast![$a]), Box::new(ast![$b]), Box::new(ast![$c]))};
        (lambda $p:tt $b:tt) => {Ast::Lambda($p, Box::new(ast![$b]))};
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
            Frontend::new().meaning(&sexpr.into(), &mut AstBuilder)
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
}