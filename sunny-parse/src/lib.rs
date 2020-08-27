use lexpr::from_str as lexpr_from_str;
use lexpr::{Number, Value, Parser};
use sunny_core::Scm;
use std::io::Read;

pub fn from_str(s: &str) -> Scm {
    let mut parser = Parser::from_str(s);
    let value = parser.expect_value().unwrap();
    parser.expect_end().unwrap();
    from_lexpr(value)
}

pub fn from_reader(r: impl Read) -> Scm {
    let mut parser = Parser::from_reader(r);
    let value = parser.expect_value().unwrap();
    from_lexpr(value)
}

pub fn from_lexpr(value: Value) -> Scm {
    match value {
        Value::Number(n) if n.is_i64() => Scm::int(n.as_i64().unwrap()),
        Value::Cons(c) if c.car().as_symbol() == Some("quote") && c.cdr().as_pair() == Some((&Value::Null, &Value::Null)) => Scm::nil(),
        _ => unimplemented!("{:?}", value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_nil() {
        assert_eq!(from_str("'()"), Scm::nil());
    }

    #[test]
    fn parse_int() {
        assert_eq!(from_str("42"), Scm::int(42));
        assert_eq!(from_str("-123"), Scm::int(-123));
    }

    #[test]
    fn read_multiple_values() {
        let mut stream = std::io::Cursor::new(b"1 2 3");

        assert_eq!(from_reader(&mut stream), Scm::int(1));
        assert_eq!(from_reader(&mut stream), Scm::int(2));
        assert_eq!(from_reader(&mut stream), Scm::int(3));
    }
}
