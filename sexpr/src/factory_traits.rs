//! Traits for constructing S-expressions.
//!
//! These traits were designed with flexibility in mind. In other words,
//! there are no functions like `Sexpr::cons` because there might be an
//! object in the system that needs to track or control allocations.
//! This could be a string interner, a garbage collector, or some other
//! kind of memory manager.
//!

use crate::core_traits::{MaybePair, Nullable, Sexpr, MaybeBool};

/// Implement Factories for this type if no memory management is needed.
/// This enables some convenience interfaces.
pub struct DummyFactory;

impl<T: Clone> CopyTracker<T> for DummyFactory {
    fn copy_value(&mut self, expr: &T) -> T {
        expr.clone()
    }
}

/// An S-expression factory.
/// Convenience trait that collects all factory traits S-expression
/// factories should implement.
pub trait SexprFactory<S: Sexpr>: NullFactory<S> + PairFactory<S> {}

/// Allow copying of S-expressions.
pub trait CopyTracker<T> {
    /// Return a copy of an S-expression value.
    fn copy_value(&mut self, expr: &T) -> T;
}

/// Construct Null values
pub trait NullFactory<T: Nullable> {
    /// Return a new null value.
    fn null(&mut self) -> T;
}

/// Construct Boolean values
pub trait BoolFactory<T: MaybeBool> {
    /// Construct a new boolean value.
    fn bool(&mut self, b: bool) -> T;
}

/// Construct Pair values
pub trait PairFactory<T: MaybePair> {
    /// Construct a new pair from the input arguments.
    fn pair(&mut self, first: T::First, second: T::Second) -> T;

    /// Construct a new pair from the input arguments using Lisp's traditional name.
    fn cons(&mut self, car: impl Into<T::First>, cdr: impl Into<T::Second>) -> T {
        self.pair(car.into(), cdr.into())
    }
}
