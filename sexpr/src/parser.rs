lalrpop_mod!(pub sexpr_grammar); // synthesized by LALRPOP

use crate::{Sexpr, SourceLocation};

pub type Result<T> = std::result::Result<T, SourceLocation<Error>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidToken,
    UnexpectedEof {
        expected: Vec<String>,
    },
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
    },
    ExtraToken,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::InvalidToken => write!(f, "Invalid token."),
            Error::UnexpectedEof { expected } => write!(
                f,
                "Unexpected end of file. Expected one of {}",
                expected.join(", ")
            ),
            Error::UnrecognizedToken { token, expected } => write!(
                f,
                "Unrecognized token `{}`. Expected one of {}",
                token,
                expected.join(", ")
            ),
            Error::ExtraToken => write!(f, "Extra token."),
        }
    }
}

impl From<lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>>
    for SourceLocation<Error>
{
    fn from(
        pe: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &'static str>,
    ) -> Self {
        match pe {
            lalrpop_util::ParseError::InvalidToken { location } => {
                SourceLocation::new(Error::InvalidToken).with_span(location..location + 1)
            }
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                SourceLocation::new(Error::UnexpectedEof { expected }).with_span(location..location)
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (l, t, r),
                expected,
            } => SourceLocation::new(Error::UnrecognizedToken {
                token: t.to_string(),
                expected,
            })
            .with_span(l..r),
            lalrpop_util::ParseError::ExtraToken { token: (l, _, r) } => {
                SourceLocation::new(Error::ExtraToken).with_span(l..r)
            }
            lalrpop_util::ParseError::User { .. } => unimplemented!(),
        }
    }
}

pub fn parse_str(s: &str) -> Result<SourceLocation<Sexpr>> {
    let context = SourceLocation::new(()).in_string(s);
    Ok(sexpr_grammar::DatumParser::new().parse(&context, s)?)
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_str;
    use crate::*;

    #[test]
    fn can_parse_true() {
        let sexpr = parse_str("#t");
        assert_eq!(sexpr.unwrap(), Sexpr::bool(true));
    }

    #[test]
    fn can_parse_false() {
        let sexpr = parse_str("#f");
        assert_eq!(sexpr.unwrap(), Sexpr::bool(false));
    }

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

    #[test]
    fn can_parse_improper_list() {
        let sexpr = parse_str("(1 2 . 3)").unwrap();
        assert_eq!(sexpr.car().unwrap(), &Sexpr::int(1));
        assert_eq!(sexpr.cadr().unwrap(), &Sexpr::int(2));
        assert_eq!(sexpr.cddr().unwrap(), &Sexpr::int(3));
    }

    #[test]
    fn can_parse_special_character_symbols() {
        // note that a single '.' can't be parsed as an identifier
        for ch in "!$%&*+-/:<=>?@^_~".chars().map(|ch| ch.to_string()) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), Sexpr::symbol(&ch));
        }
    }

    #[test]
    fn can_parse_special_character_symbols_infix() {
        for ch in "!$%&*+-./:<=>?@^_~".chars().map(|ch| format!("x{}y", ch)) {
            let sexpr = parse_str(&ch).map_err(|e| e.in_string(&ch));
            assert_eq!(sexpr.unwrap(), Sexpr::symbol(&ch));
        }
    }
}
