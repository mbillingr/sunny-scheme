use lexpr::{Number, Value, Parser};
use sunny_core::Scm;
use std::io::Read;

pub use lexpr::parse::{Error, Result};

pub fn from_str(s: &str) -> Result<Scm> {
    let mut parser = Parser::from_str(s);
    let value = parser.expect_value()?;
    parser.expect_end()?;
    from_lexpr(value)
}

pub fn from_reader(r: impl Read) -> Result<Scm> {
    let mut parser = Parser::from_reader(r);
    let value = parser.expect_value()?;
    from_lexpr(value)
}

pub fn from_lexpr(value: Value) -> Result<Scm> {
    Ok(match value {
        Value::Null => Scm::nil(),
        Value::Bool(b) => Scm::bool(b),
        Value::Number(n) if n.is_i64() => Scm::int(n.as_i64().unwrap()),
        Value::Char(ch) => Scm::char(ch),
        Value::Symbol(s) => Scm::symbol(&s),
        _ => unimplemented!("{:?}", value),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nil() {
        assert_eq!(from_str("()").unwrap(), Scm::nil());
    }

    #[test]
    fn parse_int() {
        assert_eq!(from_str("42").unwrap(), Scm::int(42));
        assert_eq!(from_str("-123").unwrap(), Scm::int(-123));
    }

    #[test]
    fn read_multiple_values() {
        let mut stream = std::io::Cursor::new(b"1 2 3");

        assert_eq!(from_reader(&mut stream).unwrap(), Scm::int(1));
        assert_eq!(from_reader(&mut stream).unwrap(), Scm::int(2));
        assert_eq!(from_reader(&mut stream).unwrap(), Scm::int(3));
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(from_str("foo").unwrap(), Scm::symbol("foo"));
    }

    #[test]
    fn parse_bool() {
        assert_eq!(from_str("#t").unwrap(), Scm::True);
        assert_eq!(from_str("#f").unwrap(), Scm::False);
    }

    #[test]
    fn parse_char() {
        assert_eq!(from_str(r"#\x").unwrap(), Scm::char('x'));
    }
}
