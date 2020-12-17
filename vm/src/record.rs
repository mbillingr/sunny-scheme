use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Deref, DerefMut, Index};

pub struct Record<K, V> {
    fields: Vec<V>,
    names: HashMap<K, usize>,
}

impl<K, V> Record<K, V> {
    pub fn new() -> Self {
        Record {
            fields: vec![],
            names: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    pub fn append(&mut self, value: V) {
        self.fields.push(value)
    }
}

impl<K, V> Record<K, V>
where
    K: Eq + Hash,
{
    pub fn append_named(&mut self, name: K, value: V) {
        self.names.insert(name, self.fields.len());
        self.fields.push(value);
    }

    pub fn get_named(&self, name: impl Borrow<K>) -> Option<&V> {
        self.names
            .get(name.borrow())
            .and_then(|&i| self.fields.get(i))
    }
}

impl<K, V> Deref for Record<K, V> {
    type Target = [V];
    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

impl<K, V> DerefMut for Record<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.fields
    }
}

impl<I, K, V> Index<I> for Record<K, V>
where
    K: Eq + Hash,
    I: Borrow<K> + Debug,
{
    type Output = V;
    fn index(&self, index: I) -> &Self::Output {
        self.get_named(index.borrow())
            .unwrap_or_else(|| panic!("unknown field: {:?}", index))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_records_start_empty() {
        let rec = Record::<(), ()>::new();
        assert!(rec.is_empty());
    }

    #[test]
    fn record_with_item_appended_is_not_empty() {
        let mut rec = Record::<(), _>::new();
        rec.append(42);
        assert!(!rec.is_empty());
    }

    #[test]
    fn records_take_any_type() {
        let mut rec = Record::<(), _>::new();
        rec.append("foo");
        assert!(!rec.is_empty());
    }

    #[test]
    fn get_item_by_index() {
        let mut rec = Record::<(), _>::new();
        rec.append("foo");
        rec.append("bar");
        assert_eq!(rec.get(0), Some(&"foo"));
        assert_eq!(rec.get(1), Some(&"bar"));
        assert_eq!(rec.get(2), None);
    }

    #[test]
    fn assign_by_index() {
        let mut rec = Record::<(), _>::new();
        rec.append("foo");
        *rec.get_mut(0).unwrap() = "bar";
        assert_eq!(rec.get(0), Some(&"bar"));
    }

    #[test]
    fn slice_access() {
        let mut rec = Record::<(), _>::new();
        rec.append("foo");
        rec.append("bar");
        assert_eq!((*rec)[0], "foo");
        assert_eq!((*rec)[1], "bar");
    }

    #[test]
    #[should_panic(expected = "unknown field")]
    fn named_slice_access_panics_if_field_is_not_there() {
        let rec = Record::<&str, ()>::new();
        rec["foo"];
    }

    #[test]
    fn append_named_field() {
        let mut rec = Record::new();
        rec.append_named("x", 1);
        rec.append_named("y", 2);
        assert_eq!((*rec)[0], 1);
        assert_eq!(rec["x"], 1);
        assert_eq!((*rec)[1], 2);
        assert_eq!(rec["y"], 2);
    }
}
