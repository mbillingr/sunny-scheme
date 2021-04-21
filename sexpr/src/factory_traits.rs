//! Traits for constructing S-expressions.
//!
//! These traits were designed with flexibility in mind. In other words,
//! there are no functions like `Sexpr::cons` because there might be an
//! object in the system that needs to track or control allocations.
//! This could be a string interner, a garbage collector, or some other
//! kind of memory manager.
//!

use crate::core_traits::{MaybeBool, MaybeChar, MaybeNumber, MaybePair, Nullable, Sexpr};
use crate::prelude::{MaybeString, MaybeSymbol};

/// Implement Factories for this type if no memory management is needed.
/// This enables some convenience interfaces.
pub struct StatelessFactory;

impl<T: Clone> CopyTracker<T> for StatelessFactory {
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

/// Construct generic values
pub trait GenericFactory<S, T> {
    /// Construct a new value.
    fn build_from(&mut self, value: S) -> T;
}

/// Construct Boolean values
pub trait BoolFactory<T: MaybeBool> {
    /// Construct a new boolean value.
    fn bool(&mut self, b: bool) -> T;
}

/// Construct Character values
pub trait CharFactory<T: MaybeChar> {
    /// Construct a new unicode character value.
    /// May panic if the implementation does not support the character.
    fn char(&mut self, ch: char) -> T;

    /// Construct a new ascii character value.
    fn ascii(&mut self, ch: u8) -> T;
}

/// Construct Numeric values
pub trait NumberFactory<T: MaybeNumber> {
    /// Construct a new exact number.
    fn number(&mut self, n: T::Number) -> T;

    ///Construct a new zero-valued raw number.
    fn raw_zero(&mut self) -> T::Number;

    ///Construct a new one-valued raw number.
    fn raw_one(&mut self) -> T::Number;
}

/// Construct Pair values
pub trait PairFactory<T: MaybePair> {
    /// Construct a new pair from the input arguments.
    fn pair(&mut self, first: T::Left, second: T::Right) -> T;

    /// Construct a new pair from the input arguments using Lisp's traditional name.
    fn cons(&mut self, car: impl Into<T::Left>, cdr: impl Into<T::Right>) -> T {
        self.pair(car.into(), cdr.into())
    }
}

/// Construct Symbol values
pub trait SymbolFactory<S, T: MaybeSymbol> {
    /// Construct a new interned symbol.
    ///
    /// Any two symbols with the same name created by this function
    /// must compare equal when passed to [`MaybeSymbol::is_same_symbol`].
    ///
    ///[`MaybeSymbol::is_same_symbol`]: crate::core_traits::MaybeSymbol::is_same_symbol
    fn interned_symbol(&mut self, name: S) -> T;

    /// Construct a new uninterned symbol.
    ///
    /// Any two symbols must not compare equal when  passed to
    /// [`MaybeSymbol::is_same_symbol`] if at least one of them was
    /// created with `uninterned_symbol`. Even if they have the same name.
    ///
    ///[`MaybeSymbol::is_same_symbol`]: crate::core_traits::MaybeSymbol::is_same_symbol
    fn uninterned_symbol(&mut self, name: S) -> T;
}

/// Construct string values
pub trait StringFactory<S, T: MaybeString> {
    /// Construct a new immutable string.
    fn constant_string(&mut self, content: S) -> T;

    /// Construct a new mutable string.
    fn mutable_string(&mut self, content: S) -> T;
}
