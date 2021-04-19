//! Equality predicates and hash functions

use std::hash::{Hash, Hasher};

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

/// Identity Equality
///
/// Semantically equivalent to Scheme's `eqv?` predicate.
pub trait IdentityEq {
    /// Return `true` if two values can be "normally regarded as the
    /// same object", as R7RS puts it. For example,
    /// `val_eq(Box::new(42), Box::new(42))` must return `true`.
    fn id_eq(&self, other: &Self) -> bool;
}

/// Value Equality
///
/// Semantically equivalent to Scheme's `equal?` predicate.
pub trait ValueEq {
    /// Return `true` two values are equal.
    ///
    /// Recursively descends into compound structures. This function
    /// must terminate, even when called with circular values.
    fn val_eq(&self, other: &Self) -> bool;
}

/// Pointer Hash
pub trait PointerHash {
    /// Compute a hash value based on the object's address in memory.
    /// If [`ptr_eq`] returns `true`, `ptr_hash` must return the same
    /// hash for each of its arguments.
    ///
    /// [`ptr_eq`]: crate::equality::PointerEq::ptr_eq
    fn ptr_hash<H: Hasher>(&self, state: &mut H);
}

/// Identity Hash
pub trait IdentityHash {
    /// Compute a hash value based on the object's identity.
    /// If [`id_eq`] returns `true`, `val_hash` must return the same
    /// hash for each of its arguments.
    ///
    /// [`id_eq`]: crate::equality::IdentityEq::id_eq
    fn id_hash<H: Hasher>(&self, state: &mut H);
}

/// Value Hash
pub trait ValueHash {
    /// Compute a hash value based on the object's value.
    /// If [`val_eq`] returns `true`, `rec_hash` must return the same
    /// hash for each of its arguments.
    ///
    /// [`val_eq`]: crate::equality::ValueEq::val_eq
    fn val_hash<H: Hasher>(&self, state: &mut H);
}

macro_rules! define_keys {
    ($(
        $(#[$extra_docs:meta])*
        $typename:ident(eq=$eq_trait:ident :: $eq_func:ident,
                        hash=$hash_trait:ident :: $hash_func:ident);
    )*) => {
        $(
            $(#[$extra_docs])*
            #[derive(Debug, Clone)]
            #[repr(transparent)]
            pub struct $typename<T: $eq_trait + $hash_trait>(T);

            impl<T: $eq_trait + $hash_trait> Hash for $typename<T> {
                fn hash<H: Hasher>(&self, state: &mut H) {
                    self.0.$hash_func(state);
                }
            }

            impl<T: $eq_trait + $hash_trait> Eq for $typename<T> {}

            impl<T: $eq_trait + $hash_trait> PartialEq for $typename<T> {
                fn eq(&self, other: &Self) -> bool {
                    self.0.$eq_func(&other.0)
                }
            }

            impl<T: $eq_trait + $hash_trait> $typename<T> {
                pub fn into_inner(self) -> T {
                    self.0
                }

                pub fn from_ref(key: &T) -> &Self {
                    unsafe {
                        // Safety: this is safe because $typename is #[repr(transparent)]
                        std::mem::transmute(key)
                    }
                }
            }

            impl<T: $eq_trait + $hash_trait> std::borrow::Borrow<T> for $typename<T> {
                fn borrow(&self) -> &T {
                    &self.0
                }
            }

            impl<T: $eq_trait + $hash_trait> From<T> for $typename<T> {
                fn from(key: T) -> Self {
                    $typename(key)
                }
            }
        )*
    }
}

define_keys! {
    /// Wrapper type for pointer-based keys in hash tables.
    PointerKey(eq=PointerEq::ptr_eq, hash=PointerHash::ptr_hash);

    /// Wrapper type for identity-based keys in hash tables.
    IdentityKey(eq=IdentityEq::id_eq, hash=IdentityHash::id_hash);

    /// Wrapper type for value-based keys in hash tables.
    ValueKey(eq=ValueEq::val_eq, hash=ValueHash::val_hash);
}
