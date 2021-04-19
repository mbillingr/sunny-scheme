//! Equality predicates and hash functions

use std::hash::Hasher;

/// Pointer Equality
///
/// Semantically equivalent to Scheme's `eq?` predicate.
pub trait PointerEq {
    /// Return `true` if two values refer to the same location in memory.
    /// This function may also return `true` for certain primitive types
    /// even if their values reside in different memory locations. For
    /// example, `ptr_eq(Box::new(42), Box::new(42))` is allowed to return
    /// `true`.
    fn ptr_eq(&self, other: &Self) -> bool;
}

/// Value Equality
///
/// Semantically equivalent to Scheme's `eqv?` predicate.
pub trait ValueEq {
    /// Return `true` if two values can be "normally regarded as the
    /// same object", as R7RS puts it. For example,
    /// `val_eq(Box::new(42), Box::new(42))` must return `true`.
    fn val_eq(&self, other: &Self) -> bool;
}

/// Recursive Equality
///
/// Semantically equivalent to Scheme's `equal?` predicate.
pub trait RecursionEq {
    /// Return `true` two values are equal.
    ///
    /// Recursively descends into compound structures. This function
    /// must terminate, even when called with circular values.
    fn rec_eq(&self, other: &Self) -> bool;
}

/// Pointer Hash
pub trait PointerHash {
    /// Compute a hash value based on the object's address in memory.
    /// If [`ptr_eq`] returns `true`, `ptr_hash` must return the same
    /// hash for each of its arguments.
    fn ptr_hash<H: Hasher>(&self, state: &mut H);
}

/// Value Hash
pub trait ValueHash {
    /// Compute a hash value based on the object's identity.
    /// If [`val_eq`] returns `true`, `val_hash` must return the same
    /// hash for each of its arguments.
    fn val_hash<H: Hasher>(&self, state: &mut H);
}

/// Recursive Hash
pub trait RecursionHash {
    /// Compute a hash value based on the object's value.
    /// If [`rec_eq`] returns `true`, `rec_hash` must return the same
    /// hash for each of its arguments.
    fn rec_hash<H: Hasher>(&self, state: &mut H);
}
