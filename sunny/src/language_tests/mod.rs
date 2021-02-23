mod constant_expressions;
mod control_flow;

use crate::context::{Context, Error};
use hamcrest2::core::{success, MatchResult, Matcher};
use hamcrest2::matchers::equal_to::EqualTo;
use hamcrest2::matchers::err::IsErr;
use hamcrest2::prelude::*;
use std::cell::RefCell;
use sunny_vm::Value;
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

    pub fn the_pair(a: impl Into<Value>, b: impl Into<Value>) -> EvaluationMatcher<ValueEqualTo> {
        let mut context = Context::new();
        let pair = context.cons(a, b);
        context.preserve(&pair);
        EvaluationMatcher {
            context: RefCell::new(context),
            result_matcher: equals(pair),
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

pub struct ValueEqualTo {
    expected: Value,
}

impl std::fmt::Display for ValueEqualTo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Ok({})", self.expected)
    }
}

impl<E: std::fmt::Debug> Matcher<Result<Value, E>> for ValueEqualTo {
    fn matches(&self, actual: Result<Value, E>) -> MatchResult {
        if actual.is_ok() && self.expected.equals(actual.as_ref().unwrap()) {
            success()
        } else {
            match actual {
                Ok(val) => Err(format!("was Ok({})", val)),
                Err(e) => Err(format!("was Err({:?})", e)),
            }
        }
    }
}

pub fn equals(expected: Value) -> ValueEqualTo {
    ValueEqualTo { expected }
}
