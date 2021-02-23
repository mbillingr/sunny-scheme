use crate::context::{Context, Error};
use hamcrest2::assert_that;
use hamcrest2::core::{MatchResult, Matcher};
use hamcrest2::matchers::equal_to::EqualTo;
use hamcrest2::matchers::err::IsErr;
use hamcrest2::prelude::*;
use std::cell::RefCell;
use sunny_vm::Value;

#[test]
fn empty_input_is_error() {
    assert_that!("", EvaluatesTo::an_error());
}

#[test]
fn the_quoted_empty_list_is_nil() {
    assert_that!("'()", EvaluatesTo::nil());
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

#[test]
fn booleans() {
    assert_that!("#t", EvaluatesTo::the_boolean(true));
    assert_that!("#f", EvaluatesTo::the_boolean(false));
}

#[test]
fn quoted_symbols_evaluate_to_symbols() {
    assert_that!("'foo", EvaluatesTo::the_symbol("foo"));
}

struct EvaluatesTo;

impl EvaluatesTo {
    pub fn an_error() -> EvaluationMatcher<IsErr<Value, Error>> {
        EvaluationMatcher::new(err())
    }

    pub fn the_integer(i: i64) -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        EvaluationMatcher::new(equal_to(Ok(Value::Int(i))))
    }

    pub fn the_boolean(b: bool) -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        EvaluationMatcher::new(equal_to(Ok(Value::bool(b))))
    }

    pub fn the_symbol(name: &str) -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        let mut context = Context::new();
        let symbol = context.symbol(name);
        context.preserve(&symbol);
        EvaluationMatcher {
            context: RefCell::new(context),
            result_matcher: equal_to(Ok(symbol)),
        }
    }

    pub fn nil() -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        EvaluationMatcher::new(equal_to(Ok(Value::Nil)))
    }
}

struct EvaluationMatcher<M: Matcher<Result<Value, Error>>> {
    context: RefCell<Context>,
    result_matcher: M,
}

impl<M: Matcher<Result<Value, Error>>> EvaluationMatcher<M> {
    pub fn new(result_matcher: M) -> Self {
        EvaluationMatcher {
            context: RefCell::new(Context::new()),
            result_matcher,
        }
    }
}

impl<M: Matcher<Result<Value, Error>>> std::fmt::Display for EvaluationMatcher<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.result_matcher.fmt(f)
    }
}

impl<M: Matcher<Result<Value, Error>>> Matcher<&str> for EvaluationMatcher<M> {
    fn matches(&self, actual: &str) -> MatchResult {
        let result = self.context.borrow_mut().eval(actual);
        self.result_matcher.matches(result)
    }
}
