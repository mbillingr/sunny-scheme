mod parser;
mod scanner;
mod sexpr;

pub use parser::parse_str;
pub use sexpr::{Context, Sexpr};

type Int = i64;

pub type Result<T> = std::result::Result<T, Context<Error>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEof,
    InvalidNumber,
    Utf8Error,
    MissingDelimiter,
    Expected(String),
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
}
