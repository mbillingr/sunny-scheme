mod constant_expressions;
mod control_features;
mod control_flow;
mod equivalence_predicates;
mod global_variables;
mod lexical_conventions;
mod libraries;
mod macros;
mod pattern_language;
mod procedures;
mod program_structure;
mod scoping;

use crate::builtin_libs::define_standard_libraries;
use crate::context::{Context, Error};
use crate::frontend::error::Error as FrontendError;
use crate::frontend::syntax_forms::Import;
use crate::library_filesystem::LibraryFileSystem;
use hamcrest2::core::{expect, success, MatchResult, Matcher};
use hamcrest2::matchers::equal_to::EqualTo;
use hamcrest2::matchers::err::IsErr;
use hamcrest2::matchers::has::Has;
use hamcrest2::prelude::*;
use std::cell::Cell;
use sunny_sexpr_parser::{Scm, SourceLocation};
use sunny_vm::scm_extension::ScmExt;

struct EvaluatesTo;

impl EvaluatesTo {
    pub fn an_error() -> EvaluationMatcher<IsErr<Scm, Error>> {
        EvaluationMatcher::new(err())
    }

    pub fn a_procedure() -> EvaluationMatcher<ResultSatisfies> {
        EvaluationMatcher::new(satisfies(Scm::is_procedure, "<a procedure>"))
    }

    pub fn void() -> EvaluationMatcher<Has<Scm>> {
        EvaluationMatcher::new(has(Scm::void()))
    }

    pub fn the_frontend_error(err: FrontendError) -> EvaluationMatcher<IsSpecificErr<Error>> {
        let err = SourceLocation::new(err);
        let err = Error::FrontendError(err);
        EvaluationMatcher::new(is_err(err))
    }

    pub fn the_integer(i: i64) -> EvaluationMatcher<Has<Scm>> {
        EvaluationMatcher::new(has(Scm::number(i)))
    }

    pub fn the_boolean(b: bool) -> EvaluationMatcher<Has<Scm>> {
        EvaluationMatcher::new(has(Scm::bool(b)))
    }

    pub fn the_symbol(name: &'static str) -> EvaluationMatcher<IsOk<EqualsSymbol>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_symbol(name)),
        }
    }

    pub fn the_pair(a: impl Into<Scm>, b: impl Into<Scm>) -> EvaluationMatcher<IsOk<EqualsPair>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_pair(a, b)),
        }
    }

    pub fn the_list(items: Vec<impl Into<Scm>>) -> EvaluationMatcher<IsOk<EqualsList>> {
        EvaluationMatcher {
            result_matcher: is_ok(equals_list(items)),
        }
    }

    pub fn nil() -> EvaluationMatcher<EqualTo<Result<Scm, Error>>> {
        EvaluationMatcher::new(equal_to(Ok(Scm::null())))
    }
}

struct EvaluationMatcher<M: Matcher<Result<Scm, Error>>> {
    result_matcher: M,
}

impl<M: Matcher<Result<Scm, Error>>> EvaluationMatcher<M> {
    pub fn new(result_matcher: M) -> Self {
        EvaluationMatcher { result_matcher }
    }
}

impl<M: Matcher<Result<Scm, Error>>> std::fmt::Display for EvaluationMatcher<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.result_matcher.fmt(f)
    }
}

impl<M: Matcher<Result<Scm, Error>>, T: Eval> Matcher<T> for EvaluationMatcher<M> {
    fn matches(&self, testee: T) -> MatchResult {
        let mut context = testee.make_context();
        let result = testee.eval(&mut context);
        self.result_matcher.matches(result)
    }
}

pub trait Eval {
    fn eval(&self, context: &mut Context) -> Result<Scm, Error>;

    fn make_context(&self) -> Context {
        let mut context = Context::new();

        let libfs = LibraryFileSystem::new(vec![std::env::current_dir()
            .unwrap()
            .join("../sunny-libs")
            .as_os_str()
            .to_str()
            .unwrap()]);
        context.set_libfs(libfs);

        define_standard_libraries(&mut context);
        Import::import_all("(sunny core)", context.env());
        context
    }
}

impl Eval for &str {
    fn eval(&self, context: &mut Context) -> Result<Scm, Error> {
        context.eval((*self).into())
    }
}

impl<T: Eval> Eval for Vec<T> {
    fn eval(&self, context: &mut Context) -> Result<Scm, Error> {
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
    Import::import_all("(sunny core)", context.env());

    let libfs = LibraryFileSystem::new(vec![std::env::current_dir()
        .unwrap()
        .join("../sunny-libs")
        .as_os_str()
        .to_str()
        .unwrap()]);

    When { context, libfs }
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
    fn eval(&self, context: &mut Context) -> Result<Scm, Error> {
        self.evalee.eval(context)
    }

    fn make_context(&self) -> Context {
        self.context.take().expect("can only retrieve context once")
    }
}

#[derive(Debug)]
pub struct EqualsPair {
    car: Scm,
    cdr: Scm,
}

impl std::fmt::Display for EqualsPair {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} . {})", self.car, self.cdr)
    }
}

impl Matcher<Scm> for EqualsPair {
    fn matches(&self, actual: Scm) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Scm> for EqualsPair {
    fn eq(&self, x: &Scm) -> bool {
        x.as_pair()
            .map(|p| p.0.is_equal(&self.car) && p.1.is_equal(&self.cdr))
            .unwrap_or(false)
    }
}

impl PartialEq<EqualsPair> for Scm {
    fn eq(&self, x: &EqualsPair) -> bool {
        self.as_pair()
            .map(|p| p.0.is_equal(&x.car) && p.1.is_equal(&x.cdr))
            .unwrap_or(false)
    }
}

pub fn equals_pair(car: impl Into<Scm>, cdr: impl Into<Scm>) -> EqualsPair {
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

impl Matcher<Scm> for EqualsSymbol {
    fn matches(&self, actual: Scm) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Scm> for EqualsSymbol {
    fn eq(&self, x: &Scm) -> bool {
        x.as_symbol().map(|s| *s == *self.name).unwrap_or(false)
    }
}

impl PartialEq<EqualsSymbol> for Scm {
    fn eq(&self, x: &EqualsSymbol) -> bool {
        self.as_symbol().map(|s| *s == *x.name).unwrap_or(false)
    }
}

pub fn equals_symbol(name: &'static str) -> EqualsSymbol {
    EqualsSymbol { name }
}

#[derive(Debug)]
pub struct EqualsList {
    items: Vec<Scm>,
    last_cdr: Scm,
}

impl std::fmt::Display for EqualsList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(")?;
        write!(f, "{}", &self.items[0])?;
        for x in &self.items[1..] {
            write!(f, " {}", x)?;
        }
        if !self.last_cdr.is_null() {
            write!(f, " . {}", self.last_cdr)?;
        }
        write!(f, ")")
    }
}

impl Matcher<Scm> for EqualsList {
    fn matches(&self, actual: Scm) -> MatchResult {
        expect(&actual == self, format!("was {}", actual))
    }
}

impl PartialEq<Scm> for EqualsList {
    fn eq(&self, x: &Scm) -> bool {
        is_list_eq(x, &self.items, &self.last_cdr)
    }
}

impl PartialEq<EqualsList> for Scm {
    fn eq(&self, x: &EqualsList) -> bool {
        is_list_eq(self, &x.items, &x.last_cdr)
    }
}

fn is_list_eq(x: &Scm, items: &[Scm], last_cdr: &Scm) -> bool {
    match (items, last_cdr, x.as_pair()) {
        ([], ld, _) => x.is_equal(ld),
        ([a, d @ ..], ld, Some(p)) => a.is_equal(&p.0) && is_list_eq(&p.1, d, ld),
        _ => false,
    }
}

pub fn equals_list(items: Vec<impl Into<Scm>>) -> EqualsList {
    EqualsList {
        items: items.into_iter().map(|x| x.into()).collect(),
        last_cdr: Scm::null(),
    }
}

pub struct ResultSatisfies {
    description: &'static str,
    predicate: fn(&Scm) -> bool,
}

impl std::fmt::Display for ResultSatisfies {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Ok(<{}>)", self.description)
    }
}

impl<E: std::fmt::Debug> Matcher<Result<Scm, E>> for ResultSatisfies {
    fn matches(&self, actual: Result<Scm, E>) -> MatchResult {
        match actual {
            Ok(val) if (self.predicate)(&val) => success(),
            Ok(val) => Err(format!("was Ok({})", val)),
            Err(e) => Err(format!("was Err({:?})", e)),
        }
    }
}

pub fn satisfies(predicate: fn(&Scm) -> bool, description: &'static str) -> ResultSatisfies {
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
