use crate::backend::Backend;
use sunny_sexpr_parser::{Context, CxR, Sexpr};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownSpecialForm(Context<String>),
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
                        let first = self.meaning(sexpr.cadr().unwrap(), backend)?;
                        let second = self.meaning(sexpr.caddr().unwrap(), backend)?;
                        Ok(backend.cons(first, second))
                    }
                    "quote" => {
                        let arg = sexpr.cadr().unwrap();
                        Ok(backend.constant(arg.get_value()))
                    }
                    _ => Err(Error::UnknownSpecialForm(first.map(s.to_string()))),
                }
            } else {
                unimplemented!()
            }
        }
    }
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
        let sexpr = Sexpr::list(
            vec![
                Sexpr::symbol("quote").into(),
                Sexpr::symbol("x").into(),
            ]
                .into_iter());
        let b = &mut AstBuilder;
        let m = Frontend::new().meaning(&sexpr.into(), b);
        assert_eq!(m, Ok(Ast::Const("x".to_string())));
    }
}
