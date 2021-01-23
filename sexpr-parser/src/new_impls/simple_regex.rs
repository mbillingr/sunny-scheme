#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    Empty,
    Character(u8),
    Concatenation(Box<Regex>, Box<Regex>),
    Alternation(Box<Regex>, Box<Regex>),
    Closure(Box<Regex>),
}

impl std::fmt::Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            return write!(f, "({})", self);
        }

        match self {
            Regex::Empty => Ok(()),
            Regex::Character(ch) => char::from(*ch).fmt(f),
            Regex::Concatenation(a, b) => {
                self.write_with_precedence(f, a)?;
                self.write_with_precedence(f, b)
            }
            Regex::Alternation(a, b) => {
                self.write_with_precedence(f, a)?;
                write!(f, "|")?;
                self.write_with_precedence(f, b)
            }
            Regex::Closure(a) => {
                self.write_with_precedence(f, a)?;
                write!(f, "*")
            }
        }
    }
}

impl Regex {
    pub fn new(s: &str) -> Self {
        Self::parse(s.as_bytes())
    }

    pub fn char(ch: u8) -> Self {
        Regex::Character(ch)
    }

    pub fn concatenate(a: impl Into<Regex>, b: impl Into<Regex>) -> Self {
        Regex::Concatenation(Box::new(a.into()), Box::new(b.into()))
    }

    pub fn alternate(a: impl Into<Regex>, b: impl Into<Regex>) -> Self {
        Regex::Alternation(Box::new(a.into()), Box::new(b.into()))
    }

    pub fn closure(a: impl Into<Regex>) -> Self {
        Regex::Closure(Box::new(a.into()))
    }

    pub fn precedes(&self, other: &Self) -> bool {
        use Regex::*;
        match (self, other) {
            (_, Closure(_)) => false,
            (Closure(_), _) => true,

            (_, Character(_)) => false,
            (Character(_), _) => true,

            (_, Concatenation(_, _)) => false,
            (Concatenation(_, _), _) => true,

            (_, Alternation(_, _)) => false,
            (Alternation(_, _), _) => true,

            (_, Empty) => false,
            //(Empty, _) => true,
        }
    }

    pub fn postorder_iter(&self) -> PostOrder {
        PostOrder::new(self)
    }
}

impl Regex {
    fn write_with_precedence(
        &self,
        f: &mut std::fmt::Formatter,
        a: &Box<Regex>,
    ) -> std::fmt::Result {
        if self.precedes(a) {
            write!(f, "{:#}", a)
        } else {
            write!(f, "{}", a)
        }
    }

    fn parse(s: &[u8]) -> Self {
        let (expr, rest) = Self::parse_subregex(s);
        assert!(rest.is_empty());
        expr
    }

    fn parse_subregex(s: &[u8]) -> (Self, &[u8]) {
        Self::parse_alternations(s)
    }

    fn parse_alternations(mut s: &[u8]) -> (Self, &[u8]) {
        let (mut lhs, rest) = Self::parse_concatenations(s);
        s = rest;
        loop {
            match s {
                [] => break,
                [b')', ..] => break,
                [b'|', rest @ ..] => {
                    let (rhs, rest) = Self::parse_concatenations(rest);
                    s = rest;
                    lhs = Self::alternate(lhs, rhs);
                }
                _ => break,
            }
        }
        (lhs, s)
    }

    fn parse_concatenations(mut s: &[u8]) -> (Self, &[u8]) {
        let (mut lhs, rest) = Self::parse_closure(s);
        s = rest;
        loop {
            match s {
                [] => break,
                [b'|', ..] => break,
                [b')', ..] => break,
                _ => {
                    let (rhs, rest) = Self::parse_concatenations(s);
                    s = rest;
                    lhs = Self::concatenate(lhs, rhs);
                }
            }
        }
        (lhs, s)
    }

    fn parse_closure(s: &[u8]) -> (Self, &[u8]) {
        let (lhs, rest) = Self::parse_term(s);
        match rest {
            [b'*', rest @ ..] => (Self::closure(lhs), rest),
            [b'+', rest @ ..] => (Self::concatenate(lhs.clone(), Self::closure(lhs)), rest),
            _ => (lhs, rest),
        }
    }

    fn parse_term(s: &[u8]) -> (Self, &[u8]) {
        match s {
            [b'(', inner @ ..] => {
                let (term, rest) = Self::parse_subregex(inner);
                assert_eq!(rest[0], b')');
                (term, &rest[1..])
            }
            [ch, rest @ ..] => (Self::char(*ch), rest),
            [] => (Self::Empty, &[]),
        }
    }
}

impl From<u8> for Regex {
    fn from(ch: u8) -> Self {
        Self::char(ch)
    }
}

impl From<&str> for Regex {
    fn from(s: &str) -> Self {
        Self::from(s.as_bytes())
    }
}

impl From<&[u8]> for Regex {
    fn from(s: &[u8]) -> Self {
        match s {
            [] => Self::Empty,
            [ch] => Self::Character(*ch),
            [ch, rest @ ..] => Self::concatenate(*ch, rest),
        }
    }
}

pub struct PostOrder<'a> {
    queue: Vec<TraversalNode<&'a Regex>>
}

impl<'a> PostOrder<'a> {
    fn new(root: &'a Regex) -> Self {
        PostOrder {
            queue: vec![TraversalNode::Fresh(root)]
        }
    }
}

impl<'a> Iterator for PostOrder<'a> {
    type Item = &'a Regex;
    fn next(&mut self) -> Option<Self::Item> {
        use TraversalNode::*;
        loop {
            match self.queue.pop() {
                None => return None,
                Some(Expanded(re)) => return Some(re),
                Some(Fresh(re)) => {
                    self.queue.push(Expanded(re));
                    match re {
                        Regex::Empty | Regex::Character(_) => {}
                        Regex::Closure(inner) => self.queue.push(Fresh(inner)),
                        Regex::Alternation(a, b) | Regex::Concatenation(a, b) => {
                            self.queue.push(Fresh(b));
                            self.queue.push(Fresh(a));
                        },
                    }
                }
            }
        }
    }
}

enum TraversalNode<T> {
    Fresh(T),
    Expanded(T),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_representation() {
        let re = Regex::char(b'x');
        assert_eq!(format!("{}", re), "x")
    }

    #[test]
    fn concatenate_representation() {
        let re = Regex::concatenate("foo", "bar");
        assert_eq!(format!("{}", re), "foobar")
    }

    #[test]
    fn alternate_representation() {
        let re = Regex::alternate("foo", "bar");
        assert_eq!(format!("{}", re), "foo|bar")
    }

    #[test]
    fn closure_representation() {
        let re = Regex::closure("foo");
        assert_eq!(format!("{}", re), "(foo)*")
    }

    #[test]
    fn concatenate_precedes_alternate_representation() {
        let re = Regex::concatenate(Regex::alternate("a", "b"), Regex::alternate("c", "d"));
        assert_eq!(format!("{}", re), "(a|b)(c|d)")
    }

    #[test]
    fn alternate_does_not_precede_concatenate_representation() {
        let re = Regex::alternate(Regex::concatenate("a", "b"), Regex::concatenate("c", "d"));
        assert_eq!(format!("{}", re), "ab|cd")
    }

    #[test]
    fn concatenate_does_not_selfprecede_representation() {
        let re = Regex::concatenate(Regex::concatenate("a", "b"), Regex::concatenate("c", "d"));
        assert_eq!(format!("{}", re), "abcd")
    }

    #[test]
    fn alternate_does_not_selfprecede_representation() {
        let re = Regex::alternate(Regex::alternate("a", "b"), Regex::alternate("c", "d"));
        assert_eq!(format!("{}", re), "a|b|c|d")
    }

    #[test]
    fn parse_char() {
        let re = Regex::new("x");
        assert_eq!(re, Regex::char(b'x'));
    }

    #[test]
    fn parse_concatenation() {
        let re = Regex::new("foo");
        assert_eq!(re, Regex::concatenate(b'f', Regex::concatenate(b'o', b'o')));
    }

    #[test]
    fn parse_alternate() {
        let re = Regex::new("x|y");
        assert_eq!(re, Regex::alternate("x", "y"))
    }

    #[test]
    fn parse_multiple_alternates() {
        let re = Regex::new("x|y|z");
        assert_eq!(re, Regex::alternate(Regex::alternate("x", "y"), "z"));
    }

    #[test]
    fn parse_alternating_concatenations() {
        let re = Regex::new("foo|bar|baz");
        assert_eq!(re, Regex::alternate(Regex::alternate("foo", "bar"), "baz"));
    }

    #[test]
    fn parse_closure() {
        let re = Regex::new("x*");
        assert_eq!(re, Regex::closure("x"))
    }

    #[test]
    fn parse_concatenated_alternations() {
        let re = Regex::new("(a|b)(c|d)");
        assert_eq!(
            re,
            Regex::concatenate(Regex::alternate("a", "b"), Regex::alternate("c", "d"))
        );
    }

    #[test]
    fn parse_closure_binds_only_last_char() {
        let re = Regex::new("xy*");
        assert_eq!(re, Regex::concatenate("x", Regex::closure("y")));
    }

    #[test]
    fn parse_closure_binds_parentheses() {
        let re = Regex::new("(xy)*");
        assert_eq!(re, Regex::closure(Regex::concatenate("x", "y")));
    }

    #[test]
    fn parse_positive_closure() {
        let re = Regex::new("x+");
        assert_eq!(re, Regex::new("xx*"))
    }

    #[test]
    fn iterate_postorder_empty() {
        let re = Regex::Empty;
        let postorder: Vec<&Regex> = re.postorder_iter().collect();
        assert_eq!(postorder, vec![&re]);
    }

    #[test]
    fn iterate_postorder_char() {
        let re = Regex::Character(b'x');
        let postorder: Vec<&Regex> = re.postorder_iter().collect();
        assert_eq!(postorder, vec![&re]);
    }

    #[test]
    fn iterate_postorder_closure() {
        let x = Regex::Character(b'x');
        let x_star = Regex::Closure(Box::new(x.clone()));
        let postorder: Vec<&Regex> = x_star.postorder_iter().collect();
        assert_eq!(postorder, vec![&x, &x_star]);
    }

    #[test]
    fn iterate_postorder_alternative() {
        let x = Regex::Character(b'x');
        let y = Regex::Character(b'y');
        let x_or_y = Regex::Alternation(Box::new(x.clone()), Box::new(y.clone()));
        let postorder: Vec<&Regex> = x_or_y.postorder_iter().collect();
        assert_eq!(postorder, vec![&x, &y, &x_or_y]);
    }

    #[test]
    fn iterate_postorder_concatenation() {
        let x = Regex::Character(b'x');
        let y = Regex::Character(b'y');
        let x_y = Regex::Concatenation(Box::new(x.clone()), Box::new(y.clone()));
        let postorder: Vec<&Regex> = x_y.postorder_iter().collect();
        assert_eq!(postorder, vec![&x, &y, &x_y]);
    }

    #[test]
    fn iterate_postorder_multiple_nodes() {
        let re = Regex::new("a(b|c)*");
        let postorder: Vec<&Regex> = re.postorder_iter().collect();

        let a = Regex::new("a");
        let b = Regex::new("b");
        let c = Regex::new("c");
        let b_or_c = Regex::new("b|c");
        let b_or_c_star = Regex::new("(b|c)*");

        assert_eq!(postorder, vec![
            &a, &b, &c, &b_or_c, &b_or_c_star, &re
        ]);
    }
}
