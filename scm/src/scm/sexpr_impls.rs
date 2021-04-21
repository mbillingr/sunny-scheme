use crate::Scm;
use sexpr_generics::equality::{
    IdentityEq, IdentityHash, PointerEq, PointerHash, ValueEq, ValueHash,
};
use sexpr_generics::prelude::*;
use std::hash::Hasher;

impl MaybeGeneric for Scm {
    fn to_type<T: 'static>(&self) -> Option<&T> {
        Scm::as_type(self)
    }
}

impl GenericFactory<i64, Scm> for StatelessFactory {
    fn build_from(&mut self, value: i64) -> Scm {
        Scm::number(value)
    }
}

impl Nullable for Scm {
    fn is_null(&self) -> bool {
        Scm::is_null(self)
    }
}

impl NullFactory<Scm> for StatelessFactory {
    fn null(&mut self) -> Scm {
        Scm::null()
    }
}

impl MaybeBool for Scm {
    fn to_bool(&self) -> Option<bool> {
        Scm::as_bool(self)
    }
}

impl BoolFactory<Scm> for StatelessFactory {
    fn bool(&mut self, b: bool) -> Scm {
        Scm::bool(b)
    }
}

impl MaybeChar for Scm {
    fn to_char(&self) -> Option<char> {
        unimplemented!()
    }

    fn to_ascii(&self) -> Option<u8> {
        unimplemented!()
    }
}

impl CharFactory<Scm> for StatelessFactory {
    fn char(&mut self, _ch: char) -> Scm {
        unimplemented!()
    }

    fn ascii(&mut self, _ch: u8) -> Scm {
        unimplemented!()
    }
}

impl MaybeNumber for Scm {
    type Number = i64;
    fn to_number(&self) -> Option<&i64> {
        Scm::as_number(self)
    }
}

impl NumberFactory<Scm> for StatelessFactory {
    fn number(&mut self, n: i64) -> Scm {
        Scm::int(n)
    }
    fn raw_zero(&mut self) -> i64 {
        0
    }
    fn raw_one(&mut self) -> i64 {
        1
    }
}

impl MaybePair for Scm {
    type Left = Scm;
    type Right = Scm;

    fn left(&self) -> Option<&Self::Left> {
        self.as_pair().map(|(car, _)| car)
    }

    fn right(&self) -> Option<&Self::Left> {
        self.as_pair().map(|(_, cdr)| cdr)
    }
}

impl PairFactory<Scm> for StatelessFactory {
    fn pair(&mut self, first: Scm, second: Scm) -> Scm {
        Scm::cons(first, second)
    }
}

impl MaybeSymbol for Scm {
    fn to_symbol(&self) -> Option<&str> {
        self.as_symbol()
    }
}

impl SymbolFactory<&str, Scm> for StatelessFactory {
    fn interned_symbol(&mut self, name: &str) -> Scm {
        Scm::symbol(name)
    }

    fn uninterned_symbol(&mut self, name: &str) -> Scm {
        Scm::uninterned_symbol(name)
    }
}

impl MaybeString for Scm {
    fn to_str(&self) -> Option<&str> {
        self.as_string()
    }

    fn to_mut_str(&mut self) -> Option<&mut str> {
        unimplemented!()
    }
}

impl StringFactory<&str, Scm> for StatelessFactory {
    fn constant_string(&mut self, content: &str) -> Scm {
        Scm::string(content)
    }

    fn mutable_string(&mut self, content: &str) -> Scm {
        Scm::string(content)
    }
}

impl PointerEq for Scm {
    fn ptr_eq(&self, other: &Self) -> bool {
        self.is_eq(other)
    }
}

impl IdentityEq for Scm {
    fn id_eq(&self, other: &Self) -> bool {
        self.is_eq(other) || self.0.is_eqv(&*other.0)
    }
}

impl ValueEq for Scm {
    fn val_eq(&self, other: &Self) -> bool {
        self.is_eq(other) || self.0.is_equal(&*other.0)
    }
}

impl PointerHash for Scm {
    fn ptr_hash<H: Hasher>(&self, state: &mut H) {
        self.eq_hash(state)
    }
}

impl IdentityHash for Scm {
    fn id_hash<H: Hasher>(&self, state: &mut H) {
        self.eqv_hash(state)
    }
}

impl ValueHash for Scm {
    fn val_hash<H: Hasher>(&self, state: &mut H) {
        self.equal_hash(state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sexpr_generics::equality::{IdentityKey, PointerKey, ValueKey};
    use std::hash::Hash;

    fn value_hash(x: &impl ValueHash) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        x.val_hash(&mut hasher);
        hasher.finish()
    }

    fn pointer_hash(x: &impl PointerHash) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        x.ptr_hash(&mut hasher);
        hasher.finish()
    }

    fn hash(x: &impl Hash) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        x.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn two_different_pairs_can_have_same_value() {
        let a = Scm::cons(1, 2);
        let b = Scm::cons(1, 2);

        assert!(a.val_eq(&b));
        assert_eq!(value_hash(&a), value_hash(&b));
    }

    #[test]
    fn two_different_pairs_are_different_objects() {
        let a = Scm::cons(1, 2);
        let b = Scm::cons(1, 2);

        assert!(a.ptr_eq(&a.clone()));
        assert!(!a.ptr_eq(&b));
        assert_ne!(pointer_hash(&a), pointer_hash(&b));
    }

    #[test]
    fn number_pointer_keys() {
        let a = Scm::int(42);
        let b = a.clone();
        let c = Scm::int(42);

        let ka = PointerKey::from(a);
        let kb = PointerKey::from(b);
        let kc = PointerKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_ne!(ka, kc);
        assert_ne!(hash(&ka), hash(&kc));
    }

    #[test]
    fn number_identity_keys() {
        let a = Scm::int(42);
        let b = a.clone();
        let c = Scm::int(42);

        let ka = IdentityKey::from(a);
        let kb = IdentityKey::from(b);
        let kc = IdentityKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_eq!(ka, kc);
        assert_eq!(hash(&ka), hash(&kc));
    }

    #[test]
    fn number_value_keys() {
        let a = Scm::int(42);
        let b = a.clone();
        let c = Scm::int(42);

        let ka = ValueKey::from(a);
        let kb = ValueKey::from(b);
        let kc = ValueKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_eq!(ka, kc);
        assert_eq!(hash(&ka), hash(&kc));
    }

    #[test]
    fn pair_pointer_keys() {
        let a = Scm::cons(1, 2);
        let b = a.clone();
        let c = Scm::cons(1, 2);

        let ka = PointerKey::from(a);
        let kb = PointerKey::from(b);
        let kc = PointerKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_ne!(ka, kc);
        assert_ne!(hash(&ka), hash(&kc));
    }

    #[test]
    fn pair_identity_keys() {
        let a = Scm::cons(1, 2);
        let b = a.clone();
        let c = Scm::cons(1, 2);

        let ka = IdentityKey::from(a);
        let kb = IdentityKey::from(b);
        let kc = IdentityKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_ne!(ka, kc);
        assert_ne!(hash(&ka), hash(&kc));
    }

    #[test]
    fn pair_value_keys() {
        let a = Scm::cons(1, 2);
        let b = a.clone();
        let c = Scm::cons(1, 2);

        let ka = ValueKey::from(a);
        let kb = ValueKey::from(b);
        let kc = ValueKey::from(c);

        assert_eq!(ka, kb);
        assert_eq!(hash(&ka), hash(&kb));
        assert_eq!(ka, kc);
        assert_eq!(hash(&ka), hash(&kc));
    }
}
