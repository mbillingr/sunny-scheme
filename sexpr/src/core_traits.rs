//! The traits defined in this module are central to S-expressions.

/// An S-expression.
/// Convenience trait that collects all traits S-expressions should implement.
pub trait Sexpr: Nullable + MaybePair {}

/// Trait for types that can be "null".
pub trait Nullable {
    /// Return `true` if the value represents "null".
    fn is_null(&self) -> bool;
}

/// Trait for types that can represent pairs.
pub trait MaybePair {
    /// Type of the pair's first element.
    type First;
    /// Type of the pair's second element.
    type Second;

    /// Return a reference to the pair's first element
    /// or `None` if the value does not represent a pair.
    fn first(&self) -> Option<&Self::First>;

    /// Return a reference to the pair's second element
    /// or `None` if the value does not represent a pair.
    fn second(&self) -> Option<&Self::Second>;
}
