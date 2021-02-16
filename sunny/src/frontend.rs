use crate::backend::Backend;
use crate::frontend::Error::MissingArgument;
use sunny_sexpr_parser::{Context, CxR, Sexpr};

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownSpecialForm(String),
    MissingArgument,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::UnknownSpecialForm(s) => write!(f, "Unknown special form {}.", s),
            Error::MissingArgument => write!(f, "Missing argument."),
        }
    }
}

pub struct Frontend {}

impl Frontend {
    pub fn new() -> Self {
        Frontend {}
    }

    pub fn meaning<B: Backend>(
        &mut self,
        sexpr: &Context<Sexpr>,
        backend: &mut B,
    ) -> Result<B::Output> {
        if is_atom(sexpr) {
            Ok(backend.constant(sexpr.get_value()))
        } else {
            let first = sexpr.car().unwrap();
            if let Some(s) = first.as_symbol() {
                match s {
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
                    _ => Err(error_at(sexpr, Error::UnknownSpecialForm(s.to_string()))),
                }
            } else {
                unimplemented!()
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    enum Ast {
        Const(String),
        Cons(Box<Ast>, Box<Ast>),
    }

    struct AstBuilder;

    impl Backend for AstBuilder {
        type Output = Ast;

        fn constant(&mut self, c: &Sexpr) -> Self::Output {
            Ast::Const(c.to_string())
        }

        fn cons(&mut self, first: Self::Output, second: Self::Output) -> Self::Output {
            Ast::Cons(Box::new(first), Box::new(second))
        }
    }

    #[test]
    fn meaning_of_constant() {
        let b = &mut AstBuilder;
        let m = Frontend::new().meaning(&Sexpr::int(1).into(), b);
        assert_eq!(m, Ok(Ast::Const("1".to_string())));
    }

    #[test]
    fn meaning_of_cons() {
        let b = &mut AstBuilder;
        let m = Frontend::new().meaning(
            &Sexpr::list(
                vec![
                    Sexpr::Symbol("cons").into(),
                    Sexpr::int(1).into(),
                    Sexpr::int(2).into(),
                ]
                .into_iter(),
            )
            .into(),
            b,
        );
        assert_eq!(
            m,
            Ok(Ast::Cons(
                Box::new(Ast::Const("1".to_string())),
                Box::new(Ast::Const("2".to_string()))
            ))
        );
    }

    #[test]
    fn meaning_of_quote() {
        let sexpr =
            Sexpr::list(vec![Sexpr::symbol("quote").into(), Sexpr::symbol("x").into()].into_iter());
        let b = &mut AstBuilder;
        let m = Frontend::new().meaning(&sexpr.into(), b);
        assert_eq!(m, Ok(Ast::Const("x".to_string())));
    }
}
