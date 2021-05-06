use crate::scm::interner::{interned_string, Internable, Strong};
use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

#[derive(Debug, Hash)]
#[repr(transparent)]
pub struct ConstantString(Box<str>);

impl ConstantString {
    pub fn interned(name: impl Internable<Box<str>>) -> Strong<ConstantString> {
        let string = interned_string(name);
        unsafe {
            // # Safety: converting to a repr(transparent) wrapper
            // type is safe.
            std::mem::transmute(string)
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl ScmObject for ConstantString {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self.0 == other.0)
            .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        // TODO: I'm sure this is very inefficient.
        //       It looks up the string in the interner...
        //       Would be better if we had the original Scm available to return.
        //       Actually, substitution should probably be not a member of ScmObject anyway.
        Scm::string(&self.0)
    }
}

impl Display for ConstantString {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

pub fn escape(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.char_indices();
    while let Some((_, ch)) = chars.next() {
        if ch == '\\' {
            match chars.next().map(|(_, ch)| ch) {
                Some('a') => result.push(0x07 as char),
                Some('b') => result.push(0x08 as char),
                Some('t') => result.push('\t'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some(ch) if ch.is_whitespace() => {
                    while let Some((_, ch)) = chars.next() {
                        if ch.is_whitespace() {
                            continue;
                        }
                        result.push(ch);
                        break;
                    }
                }
                Some('x') => {
                    if let Some((start, _)) = chars.next() {
                        while let Some((i, ch)) = chars.next() {
                            if ch == ';' {
                                if let Ok(n) = u32::from_str_radix(&s[start..i], 16) {
                                    if let Some(ch) = std::char::from_u32(n) {
                                        result.push(ch);
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
                Some(other) => result.push(other),
                None => break,
            }
        } else {
            result.push(ch);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_does_not_change_unescaped_strings() {
        let original = "foo-bar";
        assert_eq!(escape(original), original);
    }

    #[test]
    fn escape_alarm() {
        let original = r"\a";
        let expected = "\u{0007}";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_backspace() {
        let original = r"foo\bbar";
        let expected = "foo\u{0008}bar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_tabulator() {
        let original = r"foo\tbar";
        let expected = "foo\tbar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_linefeed() {
        let original = r"foo\nbar";
        let expected = "foo\nbar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_return() {
        let original = r"foo\rbar";
        let expected = "foo\rbar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_backslash() {
        let original = r"foo\\bar";
        let expected = "foo\u{005c}bar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_pipe() {
        let original = r"foo\|bar";
        let expected = "foo\u{007c}bar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_line_ending() {
        let original = "foo\\    \n   bar";
        let expected = "foobar";
        assert_eq!(escape(original), expected);
    }

    #[test]
    fn escape_unicode_literal() {
        let original = r"foo \x03bb; bar";
        let expected = "foo λ bar";
        assert_eq!(escape(original), expected);
    }
}
