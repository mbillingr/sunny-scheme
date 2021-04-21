use maplit::hashmap;
use sexpr_generics::prelude::PointerKey;
use sexpr_generics::prelude::*;
use std::collections::HashMap;
use sunny_scm::Scm;

#[derive(Debug, PartialEq)]
pub struct MatchResult {
    bindings: HashMap<PointerKey<Scm>, Scm>,
}

impl MatchResult {
    pub fn empty() -> Self {
        MatchResult {
            bindings: HashMap::new(),
        }
    }

    pub fn new(binding: Scm, value: Scm) -> Self {
        MatchResult {
            bindings: hashmap![binding.into() => value],
        }
    }

    pub fn from_sequence(bindings: impl IntoIterator<Item = (Scm, Scm)>) -> Self {
        MatchResult {
            bindings: bindings
                .into_iter()
                .map(|(key, value)| (key.into(), value))
                .collect(),
        }
    }
}

pub fn match_pattern(value: &Scm, pattern: &Scm) -> Option<MatchResult> {
    if pattern.is_null() && value.is_null() {
        return Some(MatchResult::empty());
    }

    match pattern.to_symbol() {
        Some("_") => return Some(MatchResult::empty()),
        Some(_) => return Some(MatchResult::new(pattern.clone(), value.clone())),
        None => {}
    }

    if pattern.is_pair() && value.is_pair() {
        let left_match = match_pattern(pattern.left().unwrap(), value.left().unwrap());
        let right_match = match_pattern(pattern.right().unwrap(), value.right().unwrap());
        unimplemented!()
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use sexpr_generics::sexpr;

    #[test]
    fn match_any() {
        let pattern = sexpr![_];
        let value = sexpr![foo];

        let result = match_pattern(&value, &pattern);

        assert_eq!(result, Some(MatchResult::empty()));
    }

    #[test]
    fn mismatch() {
        let pattern = sexpr![()];
        let mismatch_value = sexpr![foo];

        assert_eq!(match_pattern(&mismatch_value, &pattern), None);
    }

    #[test]
    fn match_empty_list() {
        let pattern = sexpr![()];
        let matching_value = sexpr![()];

        assert_eq!(
            match_pattern(&matching_value, &pattern),
            Some(MatchResult::empty())
        );
    }

    #[test]
    fn match_pattern_variable() {
        let pattern = sexpr![foo];
        let value = sexpr![42];

        let result = match_pattern(&value, &pattern);

        assert_eq!(
            result,
            Some(MatchResult::from_sequence(vec![(
                Scm::symbol("foo"),
                Scm::int(42)
            )]))
        );
    }

    #[test]
    fn match_pair() {
        let pattern = sexpr![(foo.bar)];
        let value = sexpr![(1/*car*/./*cdr*/2)]; // workaround to keep rustfmt changing this

        println!("{}", value);

        let result = match_pattern(&value, &pattern);

        assert_eq!(
            result,
            Some(MatchResult::from_sequence(vec![(
                Scm::symbol("foo"),
                Scm::int(42)
            )]))
        );
    }
}
