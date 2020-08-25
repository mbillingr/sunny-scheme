use lexpr::from_str as lexpr_from_str;
use lexpr::{Number, Value};
use sunny_core::Scm;

pub fn from_str(s: &str) -> Scm {
    from_lexpr(lexpr_from_str(s).unwrap())
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
}
