//! The traits defined in this module are central to S-expressions.

/// An S-expression.
/// Convenience trait that collects all traits S-expressions should implement.
pub trait Sexpr: Nullable + MaybePair {}

/// Trait for types that can be "null".
pub trait Nullable {
    /// Return `true` if `self` represents "null".
    fn is_null(&self) -> bool;
}

/// Trait for types that can represent other types.
pub trait MaybeGeneric {
    /// Return true if `self` represents a value of target type.
    fn is_of_type<T: 'static>(&self) -> bool {
        self.to_type::<T>().is_some()
    }

    /// Return reference of target type;
    /// returns `None` if `self` does not represent a value of that type.
    fn to_type<T: 'static>(&self) -> Option<&T>;
}

/// Trait for types that can represent booleans.
pub trait MaybeBool {
    /// Return true if `self` represents boolean "true".
    fn is_true(&self) -> bool {
        self.to_bool().unwrap_or(false)
    }

    /// Return true if `self` represents boolean "false".
    fn is_false(&self) -> bool {
        self.to_bool().map(|b| !b).unwrap_or(false)
    }

    /// Return true if `self` represents a boolean value.
    fn is_bool(&self) -> bool {
        self.to_bool().is_some()
    }

    /// Return boolean value; returns `None` if `self` does not represent a boolean.
    fn to_bool(&self) -> Option<bool>;
}

/// Trait for types that can represent characters.
pub trait MaybeChar {
    /// Return `true` if `self` represents a character.
    fn is_char(&self) -> bool {
        self.to_char().is_some()
    }

    /// Return `true` if `self` represents an ASCII character.
    fn is_ascii(&self) -> bool {
        self.to_ascii().is_some()
    }

    /// Return the character value or `None` if `self` does not represent a character.
    fn to_char(&self) -> Option<char>;

    /// Return the character as an `u8` or `None` if this is not possible.
    fn to_ascii(&self) -> Option<u8>;
}

/// Trait for types that can represent numbers.
pub trait MaybeNumber {
    /// Type of the number implementation
    type Number;

    /// Return `true` if `self` represents a number.
    fn is_number(&self) -> bool {
        self.to_number().is_some()
    }

    /// Return numeric value, or `None` if `self` does not represent a number.
    fn to_number(&self) -> Option<&Self::Number>;
}

/// Trait for types that can represent pairs.
pub trait MaybePair {
    /// Type of the pair's left (first) element.
    type Left;
    /// Type of the pair's right (second) element.
    type Right;

    /// Return `true` if `self` represents a pair.
    fn is_pair(&self) -> bool {
        self.left().is_some()
    }

    /// Return a reference to the pair's left (first) element
    /// or `None` if `self` does not represent a pair.
    fn left(&self) -> Option<&Self::Left>;

    /// Return a reference to the pair's right (second) element
    /// or `None` if `self` does not represent a pair.
    fn right(&self) -> Option<&Self::Right>;
}

/// Trait for types that can represent symbols.
pub trait MaybeSymbol {
    /// Return `true` if `self` represents a symbol.
    fn is_symbol(&self) -> bool {
        self.to_symbol().is_some()
    }

    /// Return the name of the symbol, or `None` if `self` does not represent a symbol.
    fn to_symbol(&self) -> Option<&str>;

    /// Return `true` if two symbols are the same,
    /// `false` if they are different symbols,
    /// or `None` if either argument does not represent a symbol.
    fn is_same_symbol(&self, other: &Self) -> Option<bool> {
        Some(std::ptr::eq(self.to_symbol()?, other.to_symbol()?))
    }
}

/// Trait for types that can represent strings.
pub trait MaybeString {
    /// Return `true` if `self` represents a string.
    fn is_str(&self) -> bool {
        self.to_str().is_some()
    }

    /// Return `true` if `self` represents a mutable string.
    fn is_mut_str(&mut self) -> bool {
        self.to_mut_str().is_some()
    }

    /// Return a reference to the string content,
    /// or `None` if `self` does not represent a string.
    fn to_str(&self) -> Option<&str>;

    /// Return a mutable reference to the string the content,
    /// or `None` if `self` does not represent a string.
    fn to_mut_str(&mut self) -> Option<&mut str>;
}
