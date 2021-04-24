use maplit::hashset;
use sexpr_generics::lists::{length, memq};
use sexpr_generics::prelude::PointerKey;
use sexpr_generics::prelude::*;
use sexpr_generics::with_sexpr_matcher;
use std::collections::HashSet;
use sunny_scm::Scm;

use crate::frontend::environment::Env;
use crate::frontend::macros::bindings::MatchBindings;
use crate::frontend::syntactic_closure::SyntacticClosure;

#[derive(Debug)]
pub struct PatternMatcher {
    pattern: Scm,
    ellipsis: Scm,
    literals: Scm,
}

impl PatternMatcher {
    pub fn new(pattern: Scm, ellipsis: Scm, literals: Scm) -> Self {
        PatternMatcher {
            pattern,
            ellipsis,
            literals,
        }
    }

    pub fn default(pattern: Scm) -> Self {
        PatternMatcher::new(pattern, Scm::symbol("..."), Scm::null())
    }

    pub fn match_value(&self, value: &Scm, env: &Env) -> Option<MatchBindings> {
        self.match_pattern(&self.pattern, value, env)
    }

    fn match_pattern(&self, pattern: &Scm, value: &Scm, env: &Env) -> Option<MatchBindings> {
        if pattern.is_null() && value.is_null() {
            return Some(MatchBindings::empty());
        }

        match pattern.to_symbol() {
            Some("_") => return Some(MatchBindings::empty()),
            Some(_) if memq(&self.literals, pattern).is_some() => {
                if value.ptr_eq(pattern) {
                    return Some(MatchBindings::empty());
                } else {
                    return None;
                }
            }

            Some(_) => {
                if value.is_symbol() {
                    return Some(MatchBindings::new(
                        pattern.clone(),
                        SyntacticClosure::new_scm(value.clone(), env.clone()),
                    ));
                } else {
                    return Some(MatchBindings::new(pattern.clone(), value.clone()));
                }
            }
            None => {}
        }

        with_sexpr_matcher! {
            match pattern, {
                // ellipsis in last position
                (p {self.ellipsis}) => {
                    self.match_simple_ellipsis(p, value, env)
                }

                // ellipsis followed by more elements
                (p {self.ellipsis} . pattern_tail) => {
                    let tail_length = length(pattern_tail);
                    let (res, value_tail) = self.match_general_ellipsis(p, value, tail_length, env)?;
                    Some(res.join(self.match_pattern(pattern_tail, value_tail, env)?))
                }

                // default case: just match the car and the cdr
                _ => { self.match_pair(pattern, value, env) }
            }
        }
    }

    fn match_pair(&self, pattern: &Scm, value: &Scm, env: &Env) -> Option<MatchBindings> {
        let left_match = self.match_pattern(pattern.left()?, value.left()?, env)?;
        let right_match = self.match_pattern(pattern.right()?, value.right()?, env)?;
        return Some(left_match.join(right_match));
    }

    fn match_simple_ellipsis(
        &self,
        pattern: &Scm,
        mut value: &Scm,
        env: &Env,
    ) -> Option<MatchBindings> {
        let identifiers = self.identifiers(pattern);
        let mut result = MatchBindings::empty_repetition(identifiers);
        while !value.is_null() {
            let res = self.match_pattern(pattern, value.left()?, env)?;
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
        env: &Env,
    ) -> Option<(MatchBindings, &'a Scm)> {
        let mut remaining_length = length(value);
        if remaining_length < tail_length {
            return None;
        }

        let identifiers = self.identifiers(pattern);
        let mut result = MatchBindings::empty_repetition(identifiers);
        while remaining_length > tail_length {
            let res = self.match_pattern(pattern, value.left()?, env)?;
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
    use sexpr_generics::sexpr;

    use super::*;

    #[test]
    fn match_any() {
        let pattern = sexpr![_];
        let value = sexpr![foo];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(result, Some(MatchBindings::empty()));
    }

    #[test]
    fn mismatch() {
        let pattern = sexpr![()];
        let mismatch_value = sexpr![foo];

        assert_eq!(
            PatternMatcher::default(pattern).match_value(&mismatch_value, &Env::empty("env")),
            None
        );
    }

    #[test]
    fn match_empty_list() {
        let pattern = sexpr![()];
        let matching_value = sexpr![()];

        assert_eq!(
            PatternMatcher::default(pattern).match_value(&matching_value, &Env::empty("env")),
            Some(MatchBindings::empty())
        );
    }

    #[test]
    fn match_pattern_variable() {
        let pattern = sexpr![foo];
        let value = sexpr![42];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::from_sequence(vec![(
                Scm::symbol("foo"),
                Scm::int(42)
            )]))
        );
    }

    #[test]
    fn match_pair() {
        let pattern = sexpr![(foo.bar)];
        let value = sexpr![(1/*car*/./*cdr*/2)]; // workaround to keep rustfmt changing this

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::from_sequence(vec![
                (Scm::symbol("foo"), Scm::int(1)),
                (Scm::symbol("bar"), Scm::int(2))
            ]))
        );
    }

    #[test]
    fn match_list() {
        let pattern = sexpr![(foo bar . baz)];
        let value = sexpr![(1 2 3 4)];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::from_sequence(vec![
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

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::repeated(sexpr![p], vec![1, 2, 3, 4])),
        );
    }

    #[test]
    fn match_empty_list_with_ellipsis() {
        let pattern = sexpr![(p ...)];
        let value = sexpr![()];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        let expected: Vec<i64> = vec![];
        assert_eq!(result, Some(MatchBindings::repeated(sexpr![p], expected)),);
    }

    #[test]
    fn match_ellipsis_with_multiple_bindings() {
        let pattern = sexpr![((p q) ...)];
        let value = sexpr![((1 2) (3 4))];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::join(
                MatchBindings::repeated(sexpr![p], vec![1, 3]),
                MatchBindings::repeated(sexpr![q], vec![2, 4])
            )),
        );
    }

    #[test]
    fn match_nested_ellipsis() {
        let pattern = sexpr![((p ...) ...)];
        let value = sexpr![((1 2) (3 4))];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::repeated(
                sexpr![p],
                vec![vec![1, 2], vec![3, 4]]
            )),
        );
    }

    #[test]
    fn match_list_with_ellipsis_followed_by_more_items() {
        let pattern = sexpr![(p ... x y)];
        let value = sexpr![(1 2 3 4)];

        let result = PatternMatcher::default(pattern).match_value(&value, &Env::empty("env"));

        assert_eq!(
            result,
            Some(MatchBindings::join(
                MatchBindings::repeated(sexpr![p], vec![1, 2]),
                MatchBindings::from_sequence(vec![(sexpr![x], sexpr![3]), (sexpr![y], sexpr![4])])
            ))
        );
    }

    #[test]
    fn match_pattern_literal_symbol() {
        let pattern = sexpr![foo];
        let value = sexpr![foo];

        let result = PatternMatcher::new(pattern, Scm::symbol("..."), sexpr![(foo)])
            .match_value(&value, &Env::empty("env"));

        assert_eq!(result, Some(MatchBindings::from_sequence(vec![])));
    }

    #[test]
    fn mismatch_pattern_literal_symbol() {
        let pattern = sexpr![foo];
        let value = sexpr![bar];

        let result = PatternMatcher::new(pattern, Scm::symbol("..."), sexpr![(foo)])
            .match_value(&value, &Env::empty("env"));

        assert_eq!(result, None);
    }
}
