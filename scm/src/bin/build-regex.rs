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
    println!("<boolean> = {}", boolean.build());

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
             mnemonic_escape r"\|")
    };

    let normal_identifier = regex! {(seq initial (repeat 0.. subsequent))};
    let verbatim_identifier = regex! {(seq "|" (repeat 0.. symbol_element) "|")};

    let sign_subsequent = regex! {(alt initial explicit_sign "@")};
    let dot_subsequent = regex! {(alt sign_subsequent ".")};
    let peculiar_identifier = regex! {
        (alt explicit_sign
             (seq explicit_sign sign_subsequent (repeat 0.. subsequent))
             (seq explicit_sign "." dot_subsequent (repeat 0.. subsequent))
             (seq "." dot_subsequent (repeat 0.. subsequent)))
    };
    println!("<normal identifier> = {}", normal_identifier.build());
    println!("<verbatim identifier> = {}", verbatim_identifier.build());
    println!("<peculiar identifier> = {}", peculiar_identifier.build());

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
        println!("<num {}> = {}", r, num.build());
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

#[derive(Debug, Clone, PartialEq)]
pub enum Regex {
    Any,
    BoL,
    EoL,
    Quote(String),
    Seq(Vec<Regex>),
    Alt(Vec<Regex>),
    Repeat(usize, Option<usize>, Box<Regex>),
    CharSet(String),
    Complement(String),
}

impl Regex {
    pub fn quote(s: impl ToString) -> Self {
        Self::Quote(s.to_string())
    }

    pub fn seq(x: impl Into<Vec<Self>>) -> Self {
        let x = x.into();
        match x.len() {
            1 => x.into_iter().next().unwrap(),
            _ => Self::Seq(x),
        }
    }

    pub fn alt(x: impl Into<Vec<Self>>) -> Self {
        let x = x.into();
        match x.len() {
            1 => x.into_iter().next().unwrap(),
            _ => Self::Alt(x),
        }
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

    pub fn build(&self) -> String {
        self.recursive_build(0)
    }

    fn recursive_build(&self, current_precedence_level: usize) -> String {
        let expr = match self {
            Regex::Any => format!("."),
            Regex::BoL => format!("^"),
            Regex::EoL => format!("$"),
            Regex::Quote(s) => self.escape_quote(s),
            Regex::Seq(res) => res
                .iter()
                .map(|re| re.recursive_build(self.precedence_level()))
                .collect(),
            Regex::Alt(res) => res
                .iter()
                .map(|re| re.recursive_build(self.precedence_level()))
                .collect::<Vec<_>>()
                .join("|"),
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
            Regex::Alt(_) => 1,
            Regex::BoL | Regex::EoL => 2,
            Regex::Seq(_) | Regex::Quote(_) => 3,
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
}

impl From<&str> for Regex {
    fn from(s: &str) -> Self {
        Self::quote(s)
    }
}

const CHARS_NEEDING_ESCAPE: &str = r".\^$*+?|()[]";

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
        assert_eq!(regex! {(seq)}, Seq(vec![]));
        assert_eq!(regex! {(seq .)}, Any);
        assert_eq!(regex! {(seq . .)}, Seq(vec![Any, Any]));
    }

    #[test]
    fn construct_alternatives() {
        assert_eq!(regex! {(alt)}, Alt(vec![]));
        assert_eq!(regex! {(alt .)}, Any);
        assert_eq!(regex! {(alt . .)}, Alt(vec![Any, Any]));
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
        assert_eq!(Seq(vec![]).build(), "");
        assert_eq!(Seq(vec![Any]).build(), ".");
        assert_eq!(Seq(vec![Any, Any]).build(), "..");
    }

    #[test]
    fn build_alternatives() {
        assert_eq!(Alt(vec![]).build(), "");
        assert_eq!(Alt(vec![Any]).build(), ".");
        assert_eq!(Alt(vec![Any, Any]).build(), ".|.");
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
            Alt(vec![Regex::quote("foo"), Regex::quote("bar")]).build(),
            "foo|bar"
        );
    }

    #[test]
    fn sequence_precedes_alternative() {
        assert_eq!(
            Alt(vec![
                Seq(vec![Regex::quote("a"), Regex::quote("b")]),
                Seq(vec![Regex::quote("c"), Regex::quote("d")])
            ])
            .build(),
            "ab|cd"
        );
        assert_eq!(
            Seq(vec![
                Alt(vec![Regex::quote("a"), Regex::quote("b")]),
                Alt(vec![Regex::quote("c"), Regex::quote("d")])
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
                Box::new(Seq(vec![Regex::quote("a"), Regex::quote("b")]))
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
                Box::new(Alt(vec![Regex::quote("a"), Regex::quote("b")]))
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
        assert_eq!(number.build(), "[0-9]+");

        let register = regex!((seq "r" (number.clone())));
        assert_eq!(register.build(), "r[0-9]+");

        let operand = regex!((alt register number));
        assert_eq!(operand.build(), "r[0-9]+|[0-9]+");
    }
}
