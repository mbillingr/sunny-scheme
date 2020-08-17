//! Interned Symbols

use lazy_static::lazy_static;
use std::collections::HashSet;
use std::sync::RwLock;

lazy_static! {
    static ref STRINGS: RwLock<HashSet<&'static str>> = RwLock::new(HashSet::new());
}

#[derive(Debug, Copy, Clone, Eq)]
pub struct Symbol(&'static str);

impl Symbol {
    pub fn new(name: impl ToString + AsRef<str>) -> Self {
        if let Some(s) = STRINGS.read().unwrap().get(name.as_ref()) {
            return Symbol(*s);
        }

        let static_name = make_static(name);
        STRINGS.write().unwrap().insert(static_name);
        Symbol(static_name)
    }

    pub fn new_uninterned(name: impl ToString) -> Self {
        Symbol(make_static(name))
    }

    pub fn name(&self) -> &str {
        self.0
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn make_static(s: impl ToString) -> &'static str {
    Box::leak(s.to_string().into_boxed_str())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_symbol() {
        let sym = Symbol::new("foo");
        assert_eq!(sym.name(), "foo");
    }

    #[test]
    fn symbols_created_with_the_same_name_are_equal() {
        let a = Symbol::new("bar");
        let b = Symbol::new("bar");
        assert_eq!(a, b);
    }

    #[test]
    fn symbols_created_with_different_names_are_not_equal() {
        let a = Symbol::new("foo");
        let b = Symbol::new("bar");
        assert_ne!(a, b);
    }

    #[test]
    fn uninterned_symbols_with_same_name_are_not_equal() {
        let a = Symbol::new_uninterned("bar");
        let b = Symbol::new_uninterned("bar");
        assert_eq!(a, a);
        assert_eq!(b, b);
        assert_ne!(a, b);
    }

    #[test]
    fn symbols_are_copy() {
        let a = Symbol::new("original");
        let b = a;
        assert_eq!(a, b);
    }

    #[test]
    fn symbols_are_display() {
        let a = Symbol::new("xylz");
        assert_eq!(format!("{}", a), "xylz")
    }
}
