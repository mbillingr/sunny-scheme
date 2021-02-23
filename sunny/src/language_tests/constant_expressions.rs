use crate::context::{Context, Error};
use hamcrest2::assert_that;
use hamcrest2::core::{MatchResult, Matcher};
use hamcrest2::matchers::equal_to::EqualTo;
use hamcrest2::matchers::err::IsErr;
use hamcrest2::prelude::*;
use sunny_vm::Value;

#[test]
fn empty_input_is_error() {
    assert_that!("", EvaluatesTo::an_error());
}

#[test]
fn zero_integer() {
    assert_that!("0", EvaluatesTo::the_integer(0));
}

#[test]
fn small_positive_integers() {
    assert_that!("1", EvaluatesTo::the_integer(1));
    assert_that!("23", EvaluatesTo::the_integer(23));
    assert_that!("456", EvaluatesTo::the_integer(456));
}

#[test]
fn large_positive_integers() {
    assert_that!(
        "9223372036854775807",
        EvaluatesTo::the_integer(i64::max_value())
    );
}

struct EvaluatesTo {}

impl EvaluatesTo {
    pub fn the_integer(i: i64) -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        EvaluationMatcher {
            result_matcher: equal_to(Ok(Value::Int(i))),
        }
    }

    pub fn an_error() -> EvaluationMatcher<IsErr<Value, Error>> {
        EvaluationMatcher {
            result_matcher: err(),
        }
    }
}

struct EvaluationMatcher<M: Matcher<Result<Value, Error>>> {
    result_matcher: M,
}

impl<M: Matcher<Result<Value, Error>>> std::fmt::Display for EvaluationMatcher<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.result_matcher.fmt(f)
    }
}

impl<M: Matcher<Result<Value, Error>>> Matcher<&str> for EvaluationMatcher<M> {
    fn matches(&self, actual: &str) -> MatchResult {
        let result = Context::new().eval(actual);
        self.result_matcher.matches(result)
    }
}
