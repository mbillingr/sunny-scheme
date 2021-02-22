use crate::backend::Backend;
use crate::frontend::Error::*;
use sunny_sexpr_parser::{CxR, Sexpr, SourceLocation};

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingArgument,
    ExpectedSymbol,
    ExpectedList,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingArgument => write!(f, "Missing argument."),
            Error::ExpectedSymbol => write!(f, "Expected symbol."),
            Error::ExpectedList => write!(f, "Expected list."),
        }
    }
}

pub struct Frontend {
    env: Env,
}

impl Frontend {
    pub fn new() -> Self {
        Frontend { env: Env::new() }
    }

    pub fn meaning<B: Backend>(
        &mut self,
        sexpr: &SourceLocation<Sexpr>,
        backend: &mut B,
    ) -> Result<B::Output> {
        if is_atom(sexpr) {
            if let Some(name) = sexpr.as_symbol() {
                let (depth, idx) = self
                    .lookup(name)
                    .unwrap_or_else(|| self.add_global(name.to_string(), backend));
                Ok(backend.fetch(sexpr.map(()), depth, idx))
            } else {
                Ok(backend.constant(sexpr.map(()), sexpr.get_value()))
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
                        Ok(backend.constant(arg.map(()), arg.get_value()))
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
                    _ => self.meaning_application(first, sexpr.cdr().unwrap(), backend),
                }
            } else {
                self.meaning_application(first, sexpr.cdr().unwrap(), backend)
            }
        }
    }

    pub fn meaning_application<B: Backend>(
        &mut self,
        func: &SourceLocation<Sexpr>,
        arg_exprs: &SourceLocation<Sexpr>,
        backend: &mut B,
    ) -> Result<B::Output> {
        let func = self.meaning(func, backend)?;
        let mut args = vec![];
        for a in arg_exprs.iter() {
            args.push(self.meaning(a, backend)?);
        }
        Ok(backend.invoke(func, args))
    }

    pub fn meaning_sequence<B: Backend>(
        &mut self,
        body: &SourceLocation<Sexpr>,
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
        self.env.lookup(name)
    }

    fn add_global<B: Backend>(&mut self, name: String, backend: &mut B) -> (usize, usize) {
        let (depth, idx) = self.env.add_global(name);
        backend.add_global(idx);
        (depth, idx)
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

struct Env {
    parent: Option<Box<Env>>,
    variables: Vec<String>,
}

impl Env {
    fn new() -> Self {
        Env {
            parent: None,
            variables: vec![],
        }
    }

    fn from_sexpr(vars: &SourceLocation<Sexpr>) -> Result<Self> {
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

    fn extend(self, vars: &SourceLocation<Sexpr>) -> Result<Self> {
        let mut env = Env::from_sexpr(vars)?;
        env.parent = Some(Box::new(self));
        Ok(env)
    }

    fn lookup(&self, name: &str) -> Option<(usize, usize)> {
        self.variables
            .iter()
            .position(|v| v == name)
            .map(|idx| (0, idx))
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|p| p.lookup(name))
                    .map(|(depth, i)| (1 + depth, i))
            })
    }

    fn add_global(&mut self, name: String) -> (usize, usize) {
        if let Some(p) = &mut self.parent {
            let (depth, idx) = p.add_global(name);
            return (1 + depth, idx);
        }

        let idx = self.variables.len();
        self.variables.push(name);
        (0, idx)
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
        Invoke(Box<Ast>, Vec<Ast>),
    }

    struct AstBuilder;

    impl Backend for AstBuilder {
        type Output = Ast;

        fn add_global(&mut self, _: usize) {}

        fn constant(&mut self, _: SourceLocation<()>, c: &Sexpr) -> Self::Output {
            Ast::Const(c.to_string())
        }

        fn fetch(&mut self, _: SourceLocation<()>, depth: usize, idx: usize) -> Self::Output {
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

        fn invoke(&mut self, func: Self::Output, args: Vec<Self::Output>) -> Self::Output {
            Ast::Invoke(Box::new(func), args)
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
        (invoke $f:tt $($a:tt)*) => {Ast::Invoke(Box::new(ast![$f]), vec![$(ast![$a]),*])};
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
}
