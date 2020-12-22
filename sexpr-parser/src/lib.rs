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
}

#[cfg(test)]
mod acceptance_tests {
    use crate::*;

    #[test]
    fn can_parse_integer() {
        let sexpr = parse_str("0");
        assert_eq!(sexpr.unwrap(), Sexpr::int(0));
    }
}
