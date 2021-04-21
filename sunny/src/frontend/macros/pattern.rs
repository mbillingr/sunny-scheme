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

    pub fn join(self, other: Self) -> Self {
        let mut bindings = self.bindings;
        bindings.extend(other.bindings);
        MatchResult { bindings }
    }
}

pub struct PatternMatcher {
    pattern: Scm,
}

impl PatternMatcher {
    pub fn new(pattern: Scm) -> Self {
        PatternMatcher { pattern }
    }

    pub fn match_value(&self, value: &Scm) -> Option<MatchResult> {
        recursive_matcher(&self.pattern, value)
    }
}

fn recursive_matcher(pattern: &Scm, value: &Scm) -> Option<MatchResult> {
    if pattern.is_null() && value.is_null() {
        return Some(MatchResult::empty());
    }

    match pattern.to_symbol() {
        Some("_") => return Some(MatchResult::empty()),
        Some(_) => return Some(MatchResult::new(pattern.clone(), value.clone())),
        None => {}
    }

    if pattern.is_pair() && value.is_pair() {
        let left_match = recursive_matcher(pattern.left().unwrap(), value.left().unwrap())?;
        let right_match = recursive_matcher(pattern.right().unwrap(), value.right().unwrap())?;
        return Some(left_match.join(right_match));
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

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(result, Some(MatchResult::empty()));
    }

    #[test]
    fn mismatch() {
        let pattern = sexpr![()];
        let mismatch_value = sexpr![foo];

        assert_eq!(
            PatternMatcher::new(pattern).match_value(&mismatch_value),
            None
        );
    }

    #[test]
    fn match_empty_list() {
        let pattern = sexpr![()];
        let matching_value = sexpr![()];

        assert_eq!(
            PatternMatcher::new(pattern).match_value(&matching_value),
            Some(MatchResult::empty())
        );
    }

    #[test]
    fn match_pattern_variable() {
        let pattern = sexpr![foo];
        let value = sexpr![42];

        let result = PatternMatcher::new(pattern).match_value(&value);

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
        let pattern = sexpr![(foo . bar)];
        let value = sexpr![(1/*car*/./*cdr*/2)]; // workaround to keep rustfmt changing this

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::from_sequence(vec![
                (Scm::symbol("foo"), Scm::int(1)),
                (Scm::symbol("bar"), Scm::int(2))
            ]))
        );
    }

    #[test]
    fn match_list() {
        let pattern = sexpr![(foo bar . baz)];
        let value = sexpr![(1 2 3 4)]; // workaround to keep rustfmt changing this

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::from_sequence(vec![
                (sexpr![foo], sexpr![1]),
                (sexpr![bar], sexpr![2]),
                (sexpr![baz], sexpr![(3 4)]),
            ]))
        );
    }
}
