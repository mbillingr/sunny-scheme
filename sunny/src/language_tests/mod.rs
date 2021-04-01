mod constant_expressions;
mod control_features;
mod control_flow;
mod global_variables;
mod lexical_conventions;
mod libraries;
mod macros;
mod procedures;
mod program_structure;
mod scoping;

use crate::context::{Context, Error};
use crate::frontend::error::Error as FrontendError;
use crate::library_filesystem::LibraryFileSystem;
use crate::stdlib::define_standard_libraries;
use hamcrest2::core::{expect, success, MatchResult, Matcher};
use hamcrest2::matchers::equal_to::EqualTo;
use hamcrest2::matchers::err::IsErr;
use hamcrest2::matchers::has::Has;
use hamcrest2::prelude::*;
use std::cell::Cell;
use sunny_sexpr_parser::SourceLocation;
use sunny_vm::Value;

struct EvaluatesTo;

impl EvaluatesTo {
    pub fn an_error() -> EvaluationMatcher<IsErr<Value, Error>> {
        EvaluationMatcher::new(err())
    }

    pub fn a_procedure() -> EvaluationMatcher<ResultSatisfies> {
        EvaluationMatcher::new(satisfies(Value::is_procedure, "<a procedure>"))
    }

    pub fn void() -> EvaluationMatcher<Has<Value>> {
        EvaluationMatcher::new(has(Value::Void))
    }

    pub fn the_frontend_error(err: FrontendError) -> EvaluationMatcher<IsSpecificErr<Error>> {
        let err = SourceLocation::new(err);
        let err = Error::FrontendError(err);
        EvaluationMatcher::new(is_err(err))
    }

    pub fn the_integer(i: i64) -> EvaluationMatcher<Has<Value>> {
        EvaluationMatcher::new(has(Value::number(i)))
    }

    pub fn the_boolean(b: bool) -> EvaluationMatcher<Has<Value>> {
        EvaluationMatcher::new(has(Value::bool(b)))
    }

    pub fn the_symbol(name: &'static str) -> EvaluationMatcher<IsOk<EqualsSymbol>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_symbol(name)),
        }
    }

    pub fn the_pair(
        a: impl Into<Value>,
        b: impl Into<Value>,
    ) -> EvaluationMatcher<IsOk<EqualsPair>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_pair(a, b)),
        }
    }

    pub fn the_list(items: Vec<impl Into<Value>>) -> EvaluationMatcher<IsOk<EqualsList>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_list(items)),
        }
    }

    pub fn nil() -> EvaluationMatcher<EqualTo<Result<Value, Error>>> {
        EvaluationMatcher::new(equal_to(Ok(Value::Nil)))
    }
}

struct EvaluationMatcher<M: Matcher<Result<Value, Error>>> {
    result_matcher: M,
}

impl<M: Matcher<Result<Value, Error>>> EvaluationMatcher<M> {
    pub fn new(result_matcher: M) -> Self {
        EvaluationMatcher { result_matcher }
    }
}

impl<M: Matcher<Result<Value, Error>>> std::fmt::Display for EvaluationMatcher<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.result_matcher.fmt(f)
    }
}

impl<M: Matcher<Result<Value, Error>>, T: Eval> Matcher<T> for EvaluationMatcher<M> {
    fn matches(&self, testee: T) -> MatchResult {
        let mut context = testee.make_context();
        let result = testee.eval(&mut context);
        self.result_matcher.matches(result)
    }
}

pub trait Eval {
    fn eval(&self, context: &mut Context) -> Result<Value, Error>;

    fn make_context(&self) -> Context {
        let mut context = Context::new();
        define_standard_libraries(&mut context);
        context
    }
}

impl Eval for &str {
    fn eval(&self, context: &mut Context) -> Result<Value, Error> {
        context.eval(self)
    }
}

impl<T: Eval> Eval for Vec<T> {
    fn eval(&self, context: &mut Context) -> Result<Value, Error> {
        if self.len() >= 2 {
            for expr in &self[..self.len() - 1] {
                expr.eval(context)?;
            }
        }
        self.last().unwrap().eval(context)
    }
}

pub struct When {
    context: Context,
    libfs: LibraryFileSystem,
}

pub fn given() -> When {
    let mut context = Context::new();
    define_standard_libraries(&mut context);

    When {
        context,
        libfs: LibraryFileSystem::default(),
    }
}

impl When {
    pub fn then<T: Eval>(mut self, x: T) -> EvalIn<T> {
        self.context.set_libfs(self.libfs);
        EvalIn::new(x, self.context)
    }

    pub fn file_contains(self, path: &str, content: &[u8]) -> Self {
        self.libfs.add_virtual_file(path, content);
        self
    }
}

pub struct EvalIn<T> {
    context: Cell<Option<Context>>,
    evalee: T,
}
impl<T> EvalIn<T> {
    fn new(evalee: T, context: Context) -> Self {
        EvalIn {
            context: Cell::new(Some(context)),
            evalee,
        }
    }
}

impl<T: Eval> Eval for EvalIn<T> {
    fn eval(&self, context: &mut Context) -> Result<Value, Error> {
        self.evalee.eval(context)
    }

    fn make_context(&self) -> Context {
        self.context.take().expect("can only retrieve context once")
    }
}

#[derive(Debug)]
pub struct EqualsPair {
    car: Value,
    cdr: Value,
}

impl std::fmt::Display for EqualsPair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} . {})", self.car, self.cdr)
    }
}

impl Matcher<Value> for EqualsPair {
    fn matches(&self, actual: Value) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Value> for EqualsPair {
    fn eq(&self, x: &Value) -> bool {
        x.as_pair()
            .map(|p| p.0.equals(&self.car) && p.1.equals(&self.cdr))
            .unwrap_or(false)
    }
}

impl PartialEq<EqualsPair> for Value {
    fn eq(&self, x: &EqualsPair) -> bool {
        self.as_pair()
            .map(|p| p.0.equals(&x.car) && p.1.equals(&x.cdr))
            .unwrap_or(false)
    }
}

pub fn equals_pair(car: impl Into<Value>, cdr: impl Into<Value>) -> EqualsPair {
    EqualsPair {
        car: car.into(),
        cdr: cdr.into(),
    }
}

#[derive(Debug)]
pub struct EqualsSymbol {
    name: &'static str,
}

impl std::fmt::Display for EqualsSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'{}", self.name)
    }
}

impl Matcher<Value> for EqualsSymbol {
    fn matches(&self, actual: Value) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Value> for EqualsSymbol {
    fn eq(&self, x: &Value) -> bool {
        x.as_symbol().map(|s| **s == *self.name).unwrap_or(false)
    }
}

impl PartialEq<EqualsSymbol> for Value {
    fn eq(&self, x: &EqualsSymbol) -> bool {
        self.as_symbol().map(|s| **s == *x.name).unwrap_or(false)
    }
}

pub fn equals_symbol(name: &'static str) -> EqualsSymbol {
    EqualsSymbol { name }
}

#[derive(Debug)]
pub struct EqualsList {
    items: Vec<Value>,
    last_cdr: Value,
}

impl std::fmt::Display for EqualsList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(")?;
        write!(f, "{}", &self.items[0])?;
        for x in &self.items[1..] {
            write!(f, " {}", x)?;
        }
        if !self.last_cdr.is_nil() {
            write!(f, " . {}", self.last_cdr)?;
        }
        write!(f, ")")
    }
}

impl Matcher<Value> for EqualsList {
    fn matches(&self, actual: Value) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Value> for EqualsList {
    fn eq(&self, x: &Value) -> bool {
        is_list_eq(x, &self.items, &self.last_cdr)
    }
}

impl PartialEq<EqualsList> for Value {
    fn eq(&self, x: &EqualsList) -> bool {
        is_list_eq(self, &x.items, &x.last_cdr)
    }
}

fn is_list_eq(x: &Value, items: &[Value], last_cdr: &Value) -> bool {
    match (items, last_cdr, x.as_pair()) {
        ([], ld, _) => x.equals(ld),
        ([a, d @ ..], ld, Some(p)) => a.equals(&p.0) && is_list_eq(&p.1, d, ld),
        _ => false,
    }
}

pub fn equals_list(items: Vec<impl Into<Value>>) -> EqualsList {
    EqualsList {
        items: items.into_iter().map(|x| x.into()).collect(),
        last_cdr: Value::Nil,
    }
}

pub struct ResultSatisfies {
    description: &'static str,
    predicate: fn(&Value) -> bool,
}

impl std::fmt::Display for ResultSatisfies {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Ok(<{}>)", self.description)
    }
}

impl<E: std::fmt::Debug> Matcher<Result<Value, E>> for ResultSatisfies {
    fn matches(&self, actual: Result<Value, E>) -> MatchResult {
        match actual {
            Ok(val) if (self.predicate)(&val) => success(),
            Ok(val) => Err(format!("was Ok({})", val)),
            Err(e) => Err(format!("was Err({:?})", e)),
        }
    }
}

pub fn satisfies(predicate: fn(&Value) -> bool, description: &'static str) -> ResultSatisfies {
    ResultSatisfies {
        predicate,
        description,
    }
}

pub struct IsSpecificErr<E> {
    err: E,
}

impl<E: std::fmt::Debug> std::fmt::Display for IsSpecificErr<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Err({:?})", self.err)
    }
}

impl<T, E> Matcher<Result<T, E>> for IsSpecificErr<E>
where
    T: std::fmt::Debug,
    E: std::fmt::Debug + PartialEq,
{
    fn matches(&self, actual: Result<T, E>) -> MatchResult {
        match actual {
            Err(err) if err == self.err => success(),
            other => Err(format!("was {:?}", other)),
        }
    }
}

pub fn is_err<E>(err: E) -> IsSpecificErr<E> {
    IsSpecificErr { err }
}

#[derive(Debug)]
pub struct IsOk<T> {
    value: T,
}

impl<T: std::fmt::Debug> std::fmt::Display for IsOk<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Ok({:?})", self.value)
    }
}

impl<T, U, E> Matcher<Result<U, E>> for IsOk<T>
where
    T: std::fmt::Debug,
    U: std::fmt::Debug + PartialEq<T>,
    E: std::fmt::Debug,
{
    fn matches(&self, actual: Result<U, E>) -> MatchResult {
        match actual {
            Ok(x) if x == self.value => success(),
            other => Err(format!("was {:?}", other)),
        }
    }
}

pub fn is_ok<T>(value: T) -> IsOk<T> {
    IsOk { value }
}
