use std::collections::BTreeSet;
use std::fmt::Debug;
use std::ops::{BitOr, Mul};

#[macro_export]
macro_rules! regex {
    (.) => {Regex::Any};
    (^) => {Regex::BoL};
    ($) => {Regex::EoL};
    ((seq $($x:tt)*)) => {Regex::seq(vec![$(regex!($x)),*])};
    ((alt $($x:tt)*)) => {Regex::alt(vec![$(regex!($x)),*])};
    ((repeat $n:tt $x:tt)) => {Regex::Repeat($n, Some($n), Box::new(regex!($x)))};
    ((repeat $min:tt .. $max:tt $x:tt)) => {Regex::Repeat($min, Some($max), Box::new(regex!($x)))};
    ((repeat $min:tt .. $x:tt)) => {Regex::Repeat($min, None, Box::new(regex!($x)))};
    ((opt $x:tt)) => {Regex::Repeat(0, Some(1), Box::new(regex!($x)))};
    ((from $chars:expr)) => {Regex::char_from($chars)};
    ((not-from $chars:expr)) => {Regex::complement($chars)};
    ($quote:expr) => {Regex::from($quote.clone())}
}

fn main() {
    let boolean = regex!((alt "#t" "#f" "#true" "#false"));
    println!("<boolean> = {}", boolean.simplify().build());
    let boolean_true = regex!((alt "#t" "#true"));
    println!("<boolean true> = {}", boolean_true.simplify().build());
    let boolean_false = regex!((alt "#f" "#false"));
    println!("<boolean false> = {}", boolean_false.simplify().build());

    let letter = regex! {(from r"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")};
    let digit = regex! {(from r"0123456789")};
    let hex_digit = regex! {(alt digit (from r"abcdef"))};
    let explicit_sign = regex! {(from r"+-")};

    let hex_scalar_value = regex! {(repeat 1.. hex_digit)};

    let special_init = regex! {(from r"!$%&*/:<=>?@^_~")};
    let initial = regex! {(alt letter special_init)};
    let special_subsequent = regex! {(alt explicit_sign "." "@")};
    let subsequent = regex! {(alt initial digit special_subsequent)};

    let inline_hex_escape = regex! {(seq r"\x" hex_scalar_value ";")};
    let mnemonic_escape = regex! {(alt r"\a" r"\b" r"\t" r"\n" r"\r")};
    let symbol_element = regex! {
        (alt (not-from r"|\")
             inline_hex_escape
             mnemonic_escape
             r"\|")
    };

    let normal_identifier = regex! {(seq initial (repeat 0.. subsequent))};
    let verbatim_identifier = regex! {(seq "|" (repeat 0.. symbol_element) "|")};

    let sign_subsequent = regex! {(alt initial explicit_sign "@")};
    let dot_subsequent = regex! {(alt sign_subsequent ".")};
    let peculiar_identifier = regex! {
        (alt (seq explicit_sign sign_subsequent (repeat 0.. subsequent))
             (seq explicit_sign "." dot_subsequent (repeat 0.. subsequent))
             (seq "." dot_subsequent (repeat 0.. subsequent))
             explicit_sign)
    };
    println!(
        "<normal identifier> = {}",
        normal_identifier.simplify().build()
    );
    println!(
        "<verbatim identifier> = {}",
        verbatim_identifier.simplify().build()
    );
    println!(
        "<peculiar identifier> = {}",
        peculiar_identifier.simplify().build()
    );

    let exponent_marker = "e";
    let sign = regex! {(opt (alt "+" "-"))};
    let exactness = regex! {(opt (alt "#i" "#e"))};
    let infnan = regex! {(alt "+inf.0" "-inf.0" "+nan.0" "-nan.0")};
    for &r in &[2, 8, 10, 16] {
        let prefix = regex! {(alt (seq (radix(r)) exactness) (seq exactness (radix(r))))};
        let uinteger = regex! {(repeat 1.. (num_digit(r)))};
        let suffix = regex! {(opt(seq exponent_marker sign uinteger))};
        let decimal = regex! {
            (alt (seq uinteger "." (repeat 0.. (num_digit(r))) suffix)
                 (seq "." uinteger suffix)
                 (seq uinteger suffix))
        };
        let ureal = regex! {
            (alt (seq uinteger "/" uinteger)
                 decimal
                 uinteger)  // note that the <uinteger> alternative is also matched by <decimal> because <suffix> is optional
        };
        let real = regex! {(alt (seq sign ureal) infnan)};
        let complex = regex! {
            (alt (seq real "@" real)
                 (seq real "+" ureal "i")
                 (seq real "-" ureal "i")
                 (seq real "+i")
                 (seq real "-i")
                 (seq real infnan "i")
                 (seq "+" ureal "i")
                 (seq "-" ureal "i")
                 (seq infnan "i")
                 (seq "+i")
                 (seq "-i")
                 real)
        };
        let num = regex! {(seq prefix complex)};
        println!("<num {}> = {}", r, num.simplify().build());
    }
}

fn radix(r: u8) -> Regex {
    match r {
        2 => regex! {"#b"},
        8 => regex! {"#o"},
        10 => regex! {(opt "#d")},
        16 => regex! {"#x"},
        _ => panic!("Invalid number base: {}", r),
    }
}

fn num_digit(r: u8) -> Regex {
    match r {
        2 => regex! {(from "01")},
        8 => regex! {(from "01234567")},
        10 => regex! {(from "0123456789")},
        16 => regex! {(from "0123456789abcdef")},
        _ => panic!("Invalid number base: {}", r),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Regex {
    Any,
    BoL,
    EoL,
    Quote(String),
    Seq(Box<Regex>, Box<Regex>),
    Alt(Box<Regex>, Box<Regex>),
    Repeat(usize, Option<usize>, Box<Regex>),
    CharSet(String),
    Complement(String),
}

impl Regex {
    fn is_set(&self) -> bool {
        match self {
            Regex::CharSet(_) => true,
            _ => false,
        }
    }

    fn is_alt(&self) -> bool {
        match self {
            Regex::Alt(_, _) => true,
            _ => false,
        }
    }

    fn is_seq(&self) -> bool {
        match self {
            Regex::Seq(_, _) => true,
            _ => false,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Regex::Quote(s) => s.is_empty(),
            Regex::Repeat(_, _, re) => re.is_empty(),
            _ => false,
        }
    }

    fn is_quote(&self) -> bool {
        self.as_quote_str().is_some()
    }

    pub fn as_quote_str(&self) -> Option<&str> {
        match self {
            Self::Quote(s) => Some(s),
            _ => None,
        }
    }

    pub fn empty() -> Self {
        Self::Quote("".to_string())
    }

    pub fn quote(s: impl ToString) -> Self {
        Self::Quote(s.to_string())
    }

    pub fn seq(x: Vec<Self>) -> Self {
        let mut x: Vec<_> = x.into_iter().filter(|r| !r.is_empty()).collect();
        match x.len() {
            0 => Self::empty(),
            1 => x.into_iter().next().unwrap(),
            _ => {
                x.reverse();
                let first = x.pop().unwrap();
                x.reverse();
                Self::Seq(Box::new(first), Box::new(Self::seq(x)))
            }
        }
    }

    pub fn alt(exprs: Vec<Self>) -> Self {
        let mut x = vec![];
        for expr in exprs {
            if !x.contains(&expr) {
                x.push(expr);
            }
        }
        match x.len() {
            0 => Self::empty(),
            1 => x.into_iter().next().unwrap(),
            _ => {
                x.reverse();
                let first = x.pop().unwrap();
                x.reverse();
                Self::Alt(Box::new(first), Box::new(Self::alt(x)))
            }
        }
    }

    pub fn star(expr: Self) -> Self {
        if expr.is_empty() {
            return Self::empty();
        }
        Regex::Repeat(0, None, Box::new(expr))
    }

    pub fn char_from(chars: &str) -> Self {
        Self::CharSet(chars.to_string())
    }

    pub fn complement(chars: &str) -> Self {
        if chars.is_empty() {
            Self::Any
        } else {
            Self::Complement(chars.to_string())
        }
    }

    pub fn build(self) -> String {
        self.recursive_build(0)
    }

    fn recursive_build(&self, current_precedence_level: usize) -> String {
        let expr = match self {
            Regex::Any => format!("."),
            Regex::BoL => format!("^"),
            Regex::EoL => format!("$"),
            Regex::Quote(s) => self.escape_quote(s),
            Regex::Seq(a, b) => format!(
                "{}{}",
                a.recursive_build(self.precedence_level()),
                b.recursive_build(self.precedence_level())
            ),
            Regex::Alt(a, b) => format!(
                "{}|{}",
                a.recursive_build(self.precedence_level()),
                b.recursive_build(self.precedence_level())
            ),
            Regex::Repeat(0, None, re) => {
                format!("{}*", re.recursive_build(self.precedence_level()),)
            }
            Regex::Repeat(1, None, re) => {
                format!("{}+", re.recursive_build(self.precedence_level()),)
            }
            Regex::Repeat(0, Some(1), re) => {
                format!("{}?", re.recursive_build(self.precedence_level()),)
            }
            Regex::Repeat(min, Some(max), re) if min == max => {
                format!("{}{{{}}}", re.recursive_build(self.precedence_level()), min)
            }
            Regex::Repeat(min, Some(max), re) => format!(
                "{}{{{},{}}}",
                re.recursive_build(self.precedence_level()),
                min,
                max
            ),
            Regex::Repeat(min, None, re) => format!(
                "{}{{{},}}",
                re.recursive_build(self.precedence_level()),
                min
            ),
            Regex::CharSet(chars) => format!("[{}]", self.escape_bracketed(chars)),
            Regex::Complement(chars) => format!("[^{}]", self.escape_bracketed(chars)),
        };
        if self.precedence_level() < current_precedence_level {
            format!("(?:{})", expr)
        } else {
            expr
        }
    }

    fn precedence_level(&self) -> usize {
        match self {
            Regex::Alt(_, _) => 1,
            Regex::BoL | Regex::EoL => 2,
            Regex::Seq(_, _) | Regex::Quote(_) => 3,
            Regex::Repeat(_, _, _) => 4,
            Regex::CharSet(_) | Regex::Complement(_) => 5,
            Regex::Any => 99,
        }
    }

    fn escape_quote(&self, s: &str) -> String {
        let mut escaped_string = String::with_capacity(s.len());
        for ch in s.chars() {
            if CHARS_NEEDING_ESCAPE.contains(ch) {
                escaped_string.push('\\');
            }
            escaped_string.push(ch);
        }
        escaped_string
    }

    fn escape_bracketed(&self, s: &str) -> String {
        let mut chars: Vec<char> = s.chars().collect();
        chars.sort();

        let mut escaped_string = String::with_capacity(s.len());

        if s.contains(']') {
            escaped_string.push(']');
        };

        for ch in chars {
            if "]^-".contains(ch) {
                continue;
            }
            let b = escaped_string.pop();
            let a = escaped_string.pop();
            match (a, b) {
                (None, None) => {}
                (None, Some(b)) => escaped_string.push(b),
                (Some('-'), Some(b)) if u32::wrapping_sub(ch as u32, b as u32) == 1 => {
                    escaped_string.push('-');
                }
                (Some(a), Some(b))
                    if u32::wrapping_sub(b as u32, a as u32) == 1
                        && u32::wrapping_sub(ch as u32, b as u32) == 1 =>
                {
                    escaped_string.push(a);
                    escaped_string.push('-');
                }
                (Some(a), Some(b)) => {
                    escaped_string.push(a);
                    escaped_string.push(b);
                }
                (Some(_), None) => unreachable!(),
            }
            escaped_string.push(ch);
        }

        if s.contains('^') {
            escaped_string.push('^');
        };

        if s.contains('-') {
            escaped_string.push('-');
        };

        escaped_string
    }

    fn simplify(self) -> Self {
        self.expand_quotes()
            .flatten()
            .extract_common_prefix()
            .unwrap()
            .reverse()
            .flatten()
            .extract_common_prefix()
            .unwrap()
            .reverse()
            .flatten()
            .combine_alt_of_sets()
            .unwrap()
            .replace_trivial_sets_with_quote()
            .contract_quotes()
            .unwrap()
    }

    fn combine_alt_of_sets(self) -> Value<Self> {
        match self {
            Regex::Quote(s) if s.len() == 1 => Value::New(Regex::CharSet(s)),
            Regex::Alt(a, b) if a.is_set() && b.is_set() => {
                if let (Regex::CharSet(a), Regex::CharSet(b)) = (*a, *b) {
                    let chars: BTreeSet<_> = a.chars().chain(b.chars()).collect();
                    Value::New(Regex::CharSet(chars.into_iter().collect()))
                } else {
                    unreachable!()
                }
            }
            Regex::Alt(a, b) => (a.combine_alt_of_sets() | b.combine_alt_of_sets())
                .map_new(Self::combine_alt_of_sets),
            Regex::Seq(a, b) => (a.combine_alt_of_sets() * b.combine_alt_of_sets())
                .map_new(Self::combine_alt_of_sets),
            Regex::Repeat(min, max, expr) => expr
                .combine_alt_of_sets()
                .map(|x| Regex::Repeat(min, max, Box::new(x))),
            _ => Value::Old(self),
        }
    }

    fn replace_trivial_sets_with_quote(self) -> Self {
        match self {
            Self::CharSet(s) if s.len() == 1 => Self::Quote(s),
            Self::Seq(x, y) => {
                x.replace_trivial_sets_with_quote() * y.replace_trivial_sets_with_quote()
            }
            Self::Alt(a, b) => {
                a.replace_trivial_sets_with_quote() | b.replace_trivial_sets_with_quote()
            }
            Self::Repeat(min, max, expr) => {
                Self::Repeat(min, max, Box::new(expr.replace_trivial_sets_with_quote()))
            }
            other => other,
        }
    }

    fn expand_quotes(self) -> Self {
        match self {
            Self::Quote(s) => Self::seq(s.chars().map(Self::quote).collect()),
            Self::Seq(a, b) => {
                Self::seq(vec![*a, *b].into_iter().map(Self::expand_quotes).collect())
            }
            Self::Alt(a, b) => {
                Self::alt(vec![*a, *b].into_iter().map(Self::expand_quotes).collect())
            }
            Self::Repeat(min, max, expr) => Self::Repeat(min, max, Box::new(expr.expand_quotes())),
            _ => self,
        }
    }

    fn contract_quotes(self) -> Value<Self> {
        use Value::*;
        match self {
            Self::Seq(a, b) => match (*a, *b) {
                (Self::Quote(a), Self::Seq(c, d)) if c.is_quote() => {
                    (Self::Quote(a + c.as_quote_str().unwrap()) * *d).contract_quotes()
                }
                (Self::Quote(a), Self::Quote(b)) => New(Self::Quote(a + &b)),
                (a, b) => {
                    (a.contract_quotes() * b.contract_quotes()).map_new(Self::contract_quotes)
                }
            },
            Self::Alt(a, b) => {
                (a.contract_quotes() | b.contract_quotes()).map_new(Self::contract_quotes)
            }
            Self::Repeat(min, max, expr) => expr
                .contract_quotes()
                .map(|x| Regex::Repeat(min, max, Box::new(x))),
            _ => Old(self),
        }
    }

    fn flatten(self) -> Self {
        match self {
            Self::Seq(ab, c) if ab.is_seq() => {
                if let Self::Seq(a, b) = *ab {
                    Self::Seq(a, Box::new(Self::Seq(b, c))).flatten()
                } else {
                    unreachable!()
                }
            }
            Self::Alt(ab, c) if ab.is_alt() => {
                if let Self::Alt(a, b) = *ab {
                    Self::Alt(a, Box::new(Self::Alt(b, c))).flatten()
                } else {
                    unreachable!()
                }
            }
            Self::Seq(x, y) => x.flatten() * y.flatten(),
            Self::Alt(a, b) => a.flatten() | b.flatten(),
            Self::Repeat(min, max, expr) => Self::Repeat(min, max, Box::new(expr.flatten())),
            _ => self,
        }
    }

    fn extract_common_prefix(self) -> Value<Self> {
        use Value::*;
        match self {
            Regex::Alt(a, b) => {
                if a.prefix() == b.prefix() {
                    let (pa, ra) = a.extract_prefix();
                    let (pb, rb) = b.extract_prefix();
                    assert_eq!(pa, pb);
                    New((pa * (ra | rb)).extract_common_prefix().unwrap())
                } else {
                    (a.extract_common_prefix() | b.extract_common_prefix())
                        .map_new(Self::extract_common_prefix)
                }
            }
            Regex::Seq(a, b) => (a.extract_common_prefix() * b.extract_common_prefix())
                .map_new(Self::extract_common_prefix),
            Regex::Repeat(min, max, expr) => expr
                .extract_common_prefix()
                .map(|r| Regex::Repeat(min, max, Box::new(r))),
            _ => Value::Old(self),
        }
    }

    fn prefix(&self) -> &Self {
        match self {
            Self::Seq(a, _) => a,
            Self::Repeat(0, _, _) => self,
            Self::Repeat(_, _, r) => r,
            _ => self,
        }
    }

    fn extract_prefix(self) -> (Self, Self) {
        match self {
            Self::Seq(a, b) => (*a, *b),
            Self::Repeat(0, _, _) => unimplemented!(),
            Self::Repeat(min, None, r) => (*r.clone(), Self::Repeat(min - 1, None, r)),
            Self::Repeat(min, Some(max), r) => {
                (*r.clone(), Self::Repeat(min - 1, Some(max - 1), r))
            }
            _ => (self, Self::Quote("".to_string())),
        }
    }

    fn reverse(self) -> Self {
        match self {
            Self::Quote(s) => Self::Quote(s.chars().rev().collect()),
            Self::Seq(a, b) => b.reverse() * a.reverse(),
            Self::Alt(a, b) => a.reverse() | b.reverse(),
            Self::Repeat(min, max, r) => Self::Repeat(min, max, Box::new(r.reverse())),
            _ => self,
        }
    }
}

/*fn debug_inspect<T: Debug+Clone, U: Debug>(prefix: &'static str, func: impl Fn(T)->U) -> impl Fn(T)->U {
    |input: T| {
        let output = func(input.clone());
        println!("{} {:?} => {:?}", prefix, input, output);
        output
    }
}*/

#[derive(Debug)]
enum Value<T> {
    New(T),
    Old(T),
}

impl<T> Value<T> {
    pub fn unwrap(self) -> T {
        match self {
            Value::New(v) | Value::Old(v) => v,
        }
    }

    pub fn map_new(self, f: impl Fn(T) -> Value<T>) -> Value<T> {
        match self {
            Value::New(v) => f(v),
            Value::Old(v) => Value::Old(v),
        }
    }

    pub fn map(self, f: impl Fn(T) -> T) -> Value<T> {
        match self {
            Value::New(v) => Value::New(f(v)),
            Value::Old(v) => Value::Old(f(v)),
        }
    }
}

impl From<&str> for Regex {
    fn from(s: &str) -> Self {
        Self::quote(s)
    }
}

const CHARS_NEEDING_ESCAPE: &str = r".\^$*+?|()[]";

impl Mul for Regex {
    type Output = Regex;
    fn mul(self, rhs: Self) -> Regex {
        Regex::Seq(Box::new(self), Box::new(rhs))
    }
}

impl Mul for Box<Regex> {
    type Output = Regex;
    fn mul(self, rhs: Self) -> Regex {
        Regex::Seq(self, rhs)
    }
}

impl BitOr for Regex {
    type Output = Regex;
    fn bitor(self, rhs: Self) -> Self {
        Regex::Alt(Box::new(self), Box::new(rhs))
    }
}

impl BitOr for Box<Regex> {
    type Output = Regex;
    fn bitor(self, rhs: Self) -> Regex {
        Regex::Alt(self, rhs)
    }
}

impl<T: BitOr<Output = T>> BitOr for Value<T> {
    type Output = Value<T>;
    fn bitor(self, rhs: Self) -> Self {
        use Value::*;
        match (self, rhs) {
            (New(a), New(b)) => New(a | b),
            (New(a), Old(b)) => New(a | b),
            (Old(a), New(b)) => New(a | b),
            (Old(a), Old(b)) => Old(a | b),
        }
    }
}

impl<T: Mul<Output = T>> Mul for Value<T> {
    type Output = Value<T>;
    fn mul(self, rhs: Self) -> Self {
        use Value::*;
        match (self, rhs) {
            (New(a), New(b)) => New(a * b),
            (New(a), Old(b)) => New(a * b),
            (Old(a), New(b)) => New(a * b),
            (Old(a), Old(b)) => Old(a * b),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Regex::*;

    #[test]
    fn construct_primitives() {
        assert_eq!(regex!(.), Any);
        assert_eq!(regex!(^), BoL);
        assert_eq!(regex!($), EoL);
    }

    #[test]
    fn construct_quotes() {
        assert_eq!(regex! {"abc"}, Quote("abc".to_string()));
        assert_eq!(regex! {r".\^$*+?|()[]"}, Quote(r".\^$*+?|()[]".to_string()));
    }

    #[test]
    fn construct_sequences() {
        assert_eq!(regex! {(seq)}, Regex::empty());
        assert_eq!(regex! {(seq .)}, Any);
        assert_eq!(regex! {(seq . .)}, Seq(Box::new(Any), Box::new(Any)));
    }

    #[test]
    fn construct_alternatives() {
        assert_eq!(regex! {(alt)}, Regex::empty());
        assert_eq!(regex! {(alt .)}, Any);
        assert_eq!(regex! {(alt . .)}, Any);
        assert_eq!(
            regex! {(alt "a" "b")},
            Alt(Box::new(Regex::quote("a")), Box::new(Regex::quote("b")))
        );
    }

    #[test]
    fn construct_repetition() {
        assert_eq!(regex! {(repeat 2 .)}, Repeat(2, Some(2), Box::new(Any)));
        assert_eq!(regex! {(repeat 2..3 .)}, Repeat(2, Some(3), Box::new(Any)));
        assert_eq!(regex! {(repeat 2.. .)}, Repeat(2, None, Box::new(Any)));
        assert_eq!(regex! {(opt .)}, Repeat(0, Some(1), Box::new(Any)));
    }

    #[test]
    fn construct_charset() {
        assert_eq!(regex! {(from "")}, CharSet("".to_string()));
        assert_eq!(regex! {(from "[-](")}, CharSet("[-](".to_string()));
    }

    #[test]
    fn construct_complement() {
        assert_eq!(regex! {(not-from "")}, Any);
        assert_eq!(regex! {(not-from "abc")}, Complement("abc".to_string()));
        assert_eq!(regex! {(not-from "[-](")}, Complement("[-](".to_string()));
    }

    #[test]
    fn build_primitives() {
        assert_eq!(Any.build(), ".");
        assert_eq!(BoL.build(), "^");
        assert_eq!(EoL.build(), "$");
    }

    #[test]
    fn build_quote() {
        assert_eq!(Quote("foo-bar".to_string()).build(), "foo-bar");
        assert_eq!(
            Quote(r".\^$*+?|()[]".to_string()).build(),
            r"\.\\\^\$\*\+\?\|\(\)\[\]"
        );
    }

    #[test]
    fn build_sequences() {
        assert_eq!(Regex::seq(vec![]).build(), "");
        assert_eq!(Regex::seq(vec![Any]).build(), ".");
        assert_eq!(Regex::seq(vec![Any, Any]).build(), "..");
    }

    #[test]
    fn build_alternatives() {
        assert_eq!(Regex::alt(vec![]).build(), "");
        assert_eq!(Regex::alt(vec![Any]).build(), ".");
        assert_eq!(
            Regex::alt(vec![Regex::quote("a"), Regex::quote("b")]).build(),
            "a|b"
        );
    }

    #[test]
    fn build_repetition() {
        assert_eq!(Repeat(2, Some(2), Box::new(Any)).build(), ".{2}");
        assert_eq!(Repeat(2, Some(5), Box::new(Any)).build(), ".{2,5}");
        assert_eq!(Repeat(2, None, Box::new(Any)).build(), ".{2,}");
        assert_eq!(Repeat(0, None, Box::new(Any)).build(), ".*");
        assert_eq!(Repeat(1, None, Box::new(Any)).build(), ".+");
        assert_eq!(Repeat(0, Some(1), Box::new(Any)).build(), ".?");
    }

    #[test]
    fn build_charset() {
        assert_eq!(CharSet("".to_string()).build(), "[]");
        assert_eq!(CharSet("abc".to_string()).build(), "[a-c]");
        assert_eq!(CharSet("[-^]".to_string()).build(), "[][^-]");
    }

    #[test]
    fn build_complement() {
        assert_eq!(Complement("abc".to_string()).build(), "[^a-c]");
        assert_eq!(Complement("[-^]".to_string()).build(), "[^][^-]");
    }

    #[test]
    fn quote_precedes_alternative() {
        assert_eq!(
            Regex::alt(vec![Regex::quote("foo"), Regex::quote("bar")]).build(),
            "foo|bar"
        );
    }

    #[test]
    fn sequence_precedes_alternative() {
        assert_eq!(
            Regex::alt(vec![
                Regex::seq(vec![Regex::quote("a"), Regex::quote("b")]),
                Regex::seq(vec![Regex::quote("c"), Regex::quote("d")])
            ])
            .build(),
            "ab|cd"
        );
        assert_eq!(
            Regex::seq(vec![
                Regex::alt(vec![Regex::quote("a"), Regex::quote("b")]),
                Regex::alt(vec![Regex::quote("c"), Regex::quote("d")])
            ])
            .build(),
            "(?:a|b)(?:c|d)"
        );
    }

    #[test]
    fn repetition_precedes_sequence() {
        assert_eq!(
            Repeat(
                2,
                None,
                Box::new(Regex::seq(vec![Regex::quote("a"), Regex::quote("b")]))
            )
            .build(),
            "(?:ab){2,}"
        );
    }

    #[test]
    fn repetition_precedes_alternative() {
        assert_eq!(
            Repeat(
                2,
                None,
                Box::new(Regex::alt(vec![Regex::quote("a"), Regex::quote("b")]))
            )
            .build(),
            "(?:a|b){2,}"
        );
    }

    #[test]
    fn repetition_precedes_quotation() {
        assert_eq!(
            Repeat(2, None, Box::new(Regex::quote("foo"))).build(),
            "(?:foo){2,}"
        );
    }

    #[test]
    fn no_redundant_parentheses() {
        assert_eq!(
            Regex::seq(vec![Regex::alt(vec![Regex::seq(vec![Regex::alt(vec![
                Regex::quote("foo")
            ])])])]),
            Regex::quote("foo")
        );
    }

    #[test]
    fn can_compose_regexes() {
        let digit = regex!((from "1234567890"));
        let number = regex!((repeat 1.. digit));
        assert_eq!(number.clone().build(), "[0-9]+");

        let register = regex!((seq "r" (number.clone())));
        assert_eq!(register.clone().build(), "r[0-9]+");

        let operand = regex!((alt register number));
        assert_eq!(operand.build(), "r[0-9]+|[0-9]+");
    }

    #[test]
    fn shrink_alternative_of_sets() {
        let re = regex!((alt (from "ac") (from "cb") (from "cd")));
        let ex = regex!((from "abcd"));
        assert_eq!(re.simplify(), ex);
    }

    #[test]
    fn simplify_extracts_common_prefix_and_suffix() {
        let re = regex!((alt "foomehbar" "foolabar"));
        let ex = regex!((seq "foo" (alt "meh" "la") "bar"));
        assert_eq!(re.simplify(), ex);
    }

    #[test]
    fn contract_quotes() {
        let re = regex!((seq "f" (seq "o" "o")));
        let ex = regex!("foo");
        assert_eq!(re.contract_quotes().unwrap(), ex);
    }
}
