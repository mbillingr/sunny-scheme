use maplit::{hashmap, hashset};
use sexpr_generics::lists::length;
use sexpr_generics::prelude::PointerKey;
use sexpr_generics::prelude::*;
use sexpr_generics::with_sexpr_matcher;
use std::collections::{HashMap, HashSet};
use sunny_scm::Scm;

#[derive(Debug, PartialEq)]
pub struct MatchResult {
    bindings: HashMap<PointerKey<Scm>, Binding>,
}

#[derive(Debug, PartialEq)]
enum Binding {
    Simple(Scm),
    Repeated(Vec<Binding>),
}

impl MatchResult {
    fn empty() -> Self {
        MatchResult {
            bindings: HashMap::new(),
        }
    }

    fn new(binding: Scm, value: Scm) -> Self {
        MatchResult {
            bindings: hashmap![binding.into() => Binding::Simple(value)],
        }
    }

    fn empty_repetition(identifiers: impl IntoIterator<Item = PointerKey<Scm>>) -> Self {
        MatchResult {
            bindings: identifiers
                .into_iter()
                .map(|k| (k, Binding::Repeated(vec![])))
                .collect(),
        }
    }

    fn repeated(binding: Scm, values: impl IntoIterator<Item = impl Into<Binding>>) -> Self {
        let bindings = values.into_iter().map(Into::into).collect();
        MatchResult {
            bindings: hashmap![binding.into() => Binding::Repeated(bindings)],
        }
    }

    fn from_sequence(bindings: impl IntoIterator<Item = (Scm, Scm)>) -> Self {
        MatchResult {
            bindings: bindings
                .into_iter()
                .map(|(key, value)| (key.into(), Binding::Simple(value)))
                .collect(),
        }
    }

    /// Add a new binding to the match result
    fn join(self, other: Self) -> Self {
        let mut bindings = self.bindings;
        bindings.extend(other.bindings);
        MatchResult { bindings }
    }

    /// Attach a new repetition to existing bindings
    fn attach(&mut self, repetition: Self) {
        for (name, value) in repetition.bindings {
            self.bindings
                .entry(name)
                .or_insert(Binding::Repeated(vec![]))
                .attach(value)
        }
    }
}

impl Binding {
    fn attach(&mut self, value: Self) {
        match self {
            Binding::Simple(_) => panic!("can't attach repetition to simple binding"),
            Binding::Repeated(reps) => reps.push(value),
        }
    }
}

impl<T> From<Vec<T>> for Binding
where
    T: Into<Binding>,
{
    fn from(v: Vec<T>) -> Binding {
        Binding::Repeated(v.into_iter().map(Into::into).collect())
    }
}

impl From<i64> for Binding {
    fn from(x: i64) -> Binding {
        Binding::Simple(x.into())
    }
}

pub struct PatternMatcher {
    pattern: Scm,
    ellipsis: Scm,
}

impl PatternMatcher {
    pub fn new(pattern: Scm) -> Self {
        PatternMatcher {
            pattern,
            ellipsis: Scm::symbol("..."),
        }
    }

    pub fn match_value(&self, value: &Scm) -> Option<MatchResult> {
        self.match_pattern(&self.pattern, value)
    }

    fn match_pattern(&self, pattern: &Scm, value: &Scm) -> Option<MatchResult> {
        if pattern.is_null() && value.is_null() {
            return Some(MatchResult::empty());
        }

        match pattern.to_symbol() {
            Some("_") => return Some(MatchResult::empty()),
            Some(_) => return Some(MatchResult::new(pattern.clone(), value.clone())),
            None => {}
        }

        with_sexpr_matcher! {
            match pattern, {
                // ellipsis in last position
                (p {self.ellipsis}) => {
                    self.match_simple_ellipsis(p, value)
                }

                // ellipsis followed by more elements
                (p {self.ellipsis} . pattern_tail) => {
                    let tail_length = length(pattern_tail);
                    let (res, value_tail) = self.match_general_ellipsis(p, value, tail_length)?;
                    Some(res.join(self.match_pattern(pattern_tail, value_tail)?))
                }

                // default case: just match the car and the cdr
                _ => { self.match_pair(pattern, value) }
            }
        }
    }

    fn match_pair(&self, pattern: &Scm, value: &Scm) -> Option<MatchResult> {
        let left_match = self.match_pattern(pattern.left()?, value.left()?)?;
        let right_match = self.match_pattern(pattern.right()?, value.right()?)?;
        return Some(left_match.join(right_match));
    }

    fn match_simple_ellipsis(&self, pattern: &Scm, mut value: &Scm) -> Option<MatchResult> {
        let identifiers = self.identifiers(pattern);
        let mut result = MatchResult::empty_repetition(identifiers);
        while !value.is_null() {
            let res = self.match_pattern(pattern, value.left()?)?;
            result.attach(res);
            value = value.right()?;
        }
        Some(result)
    }

    fn match_general_ellipsis<'a>(
        &self,
        pattern: &Scm,
        mut value: &'a Scm,
        tail_length: usize,
    ) -> Option<(MatchResult, &'a Scm)> {
        let mut remaining_length = length(value);
        if remaining_length < tail_length {
            return None;
        }

        let identifiers = self.identifiers(pattern);
        let mut result = MatchResult::empty_repetition(identifiers);
        while remaining_length > tail_length {
            let res = self.match_pattern(pattern, value.left()?)?;
            result.attach(res);
            value = value.right()?;
            remaining_length -= 1;
        }

        Some((result, value))
    }

    fn identifiers(&self, pattern: &Scm) -> HashSet<PointerKey<Scm>> {
        with_sexpr_matcher! {
            match pattern, {
                {self.ellipsis} => { hashset![] }
                {_: Symbol} => { hashset![pattern.clone().into()] }
                (car . cdr) => {
                    let mut idents = self.identifiers(car);
                    idents.extend(self.identifiers(cdr));
                    idents
                }
                _ => { hashset![] }
            }
        }
    }
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
        let pattern = sexpr![(foo.bar)];
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
        let value = sexpr![(1 2 3 4)];

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

    #[test]
    fn match_list_with_ellipsis() {
        let pattern = sexpr![(p ...)];
        let value = sexpr![(1 2 3 4)];

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::repeated(sexpr![p], vec![1, 2, 3, 4])),
        );
    }

    #[test]
    fn match_empty_list_with_ellipsis() {
        let pattern = sexpr![(p ...)];
        let value = sexpr![()];

        let result = PatternMatcher::new(pattern).match_value(&value);

        let expected: Vec<i64> = vec![];
        assert_eq!(result, Some(MatchResult::repeated(sexpr![p], expected)),);
    }

    #[test]
    fn match_ellipsis_with_multiple_bindings() {
        let pattern = sexpr![((p q) ...)];
        let value = sexpr![((1 2) (3 4))];

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::join(
                MatchResult::repeated(sexpr![p], vec![1, 3]),
                MatchResult::repeated(sexpr![q], vec![2, 4])
            )),
        );
    }

    #[test]
    fn match_nested_ellipsis() {
        let pattern = sexpr![((p ...) ...)];
        let value = sexpr![((1 2) (3 4))];

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::repeated(
                sexpr![p],
                vec![vec![1, 2], vec![3, 4]]
            )),
        );
    }

    #[test]
    fn match_list_with_ellipsis_followed_by_more_items() {
        let pattern = sexpr![(p ... x y)];
        let value = sexpr![(1 2 3 4)];

        let result = PatternMatcher::new(pattern).match_value(&value);

        assert_eq!(
            result,
            Some(MatchResult::join(
                MatchResult::repeated(sexpr![p], vec![1, 2]),
                MatchResult::from_sequence(vec![(sexpr![x], sexpr![3]), (sexpr![y], sexpr![4])])
            ))
        );
    }
}
