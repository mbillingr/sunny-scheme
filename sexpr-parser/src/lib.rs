#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub sexpr_grammar); // synthesized by LALRPOP

mod sexpr;
pub mod str_utils;

pub use sexpr::{Context, Sexpr};

type Int = i64;

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEof,
    InvalidNumber,
    Utf8Error,
    UnexpectedToken,
    InvalidToken(String),
    MissingDelimiter,
    Expected(String),

    UnrecognizedToken(String, Vec<String>),
}

impl From<lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>>
    for Context<Error>
{
    fn from(
        pe: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>,
    ) -> Self {
        match pe {
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (l, t, r),
                expected,
            } => {
                println!("{:?}", t);
                Context::new(l, r, Error::UnrecognizedToken(t.to_string(), expected))
            }
            _ => unimplemented!("{:?}", pe),
        }
    }
}

pub fn parse_str(s: &str) -> Result<Context<Sexpr>> {
    Ok(sexpr_grammar::DatumParser::new().parse(s)?)
}

pub trait CxR {
    type Result: CxR<Result = Self::Result>;

    fn car(&self) -> Option<&Self::Result>;
    fn cdr(&self) -> Option<&Self::Result>;

    fn caar(&self) -> Option<&Self::Result> {
        self.car()?.car()
    }

    fn cadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()
    }

    fn cdar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()
    }

    fn cddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()
    }

    fn caaar(&self) -> Option<&Self::Result> {
        self.car()?.car()?.car()
    }

    fn caadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()?.car()
    }

    fn cadar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()?.car()
    }

    fn caddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()?.car()
    }

    fn cdaar(&self) -> Option<&Self::Result> {
        self.car()?.car()?.cdr()
    }

    fn cdadr(&self) -> Option<&Self::Result> {
        self.cdr()?.car()?.cdr()
    }

    fn cddar(&self) -> Option<&Self::Result> {
        self.car()?.cdr()?.cdr()
    }

    fn cdddr(&self) -> Option<&Self::Result> {
        self.cdr()?.cdr()?.cdr()
    }

    fn caaaar(&self) -> Option<&Self::Result> {
        self.caar()?.caar()
    }

    fn caaadr(&self) -> Option<&Self::Result> {
        self.cadr()?.caar()
    }

    fn caadar(&self) -> Option<&Self::Result> {
        self.cdar()?.caar()
    }

    fn caaddr(&self) -> Option<&Self::Result> {
        self.cddr()?.caar()
    }

    fn cadaar(&self) -> Option<&Self::Result> {
        self.caar()?.cadr()
    }

    fn cadadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cadr()
    }

    fn caddar(&self) -> Option<&Self::Result> {
        self.cdar()?.cadr()
    }

    fn cadddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cadr()
    }

    fn cdaaar(&self) -> Option<&Self::Result> {
        self.caar()?.cdar()
    }

    fn cdaadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cdar()
    }

    fn cdadar(&self) -> Option<&Self::Result> {
        self.cdar()?.cdar()
    }

    fn cdaddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cdar()
    }

    fn cddaar(&self) -> Option<&Self::Result> {
        self.caar()?.cddr()
    }

    fn cddadr(&self) -> Option<&Self::Result> {
        self.cadr()?.cddr()
    }

    fn cdddar(&self) -> Option<&Self::Result> {
        self.cdar()?.cddr()
    }

    fn cddddr(&self) -> Option<&Self::Result> {
        self.cddr()?.cddr()
    }
}

#[cfg(test)]
mod acceptance_tests {
    use crate::*;

    #[test]
    fn can_parse_integer() {
        let sexpr = parse_str("0");
        assert_eq!(sexpr.unwrap(), Sexpr::int(0));
    }

    #[test]
    fn can_parse_symbol() {
        let sexpr = parse_str("foo");
        assert_eq!(sexpr.unwrap(), Sexpr::symbol("foo"));
    }

    #[test]
    fn can_parse_string() {
        let sexpr = parse_str("\"foo\"");
        assert_eq!(sexpr.unwrap(), Sexpr::string("foo"));
    }

    #[test]
    fn can_parse_pair() {
        let sexpr = parse_str("(1 . 2)").unwrap();
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.cdr().unwrap(), &Sexpr::int(2));
    }

    #[test]
    fn can_parse_list() {
        let sexpr = parse_str("(1 (2 3) 4)").unwrap();
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.caadr().unwrap(), &Sexpr::int(2));
        assert_eq!(sexpr.cadadr().unwrap(), &Sexpr::int(3));
        assert_eq!(sexpr.cddadr().unwrap(), &Sexpr::nil());
        assert_eq!(sexpr.caddr().unwrap(), &Sexpr::int(4));
        assert_eq!(sexpr.cdddr().unwrap(), &Sexpr::nil());
    }
}
