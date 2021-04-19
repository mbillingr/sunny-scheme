//! Generic list algorithms.

use crate::core_traits::{MaybePair, Nullable};
use crate::equality::{IdentityEq, PointerEq, ValueEq};
use crate::factory_traits::{CopyTracker, NullFactory, PairFactory};
use crate::prelude::StatelessFactory;

/// Trait for types that can represent lists.
pub trait List<T> {
    /// Return `true` if the value represents an empty list.
    fn is_empty(&self) -> bool;

    /// Return a reference to the list's first element
    /// or `None` if the list is empty.
    fn first(&self) -> Option<&T>;

    /// Return a reference to the sublist after the first element
    /// or `None` if the list is empty.
    fn rest(&self) -> Option<&Self>;
}

/// Construct List values
pub trait ListFactory<T, S: List<T>> {
    /// Return a new empty list.
    fn empty(&mut self) -> S;

    /// Return a new list whose first element is `item`, followed by `list`.
    fn cons(&mut self, item: T, list: S) -> S;
}

impl<T, S> List<T> for S
where
    S: MaybePair<First = T, Second = S>,
{
    fn is_empty(&self) -> bool {
        !self.is_pair()
    }

    fn first(&self) -> Option<&T> {
        MaybePair::first(self)
    }

    fn rest(&self) -> Option<&Self> {
        MaybePair::second(self)
    }
}

impl<T, S, F> ListFactory<T, S> for F
where
    S: Nullable + MaybePair<First = T, Second = S>,
    F: NullFactory<S> + PairFactory<S>,
{
    fn empty(&mut self) -> S {
        self.null()
    }

    fn cons(&mut self, item: T, list: S) -> S {
        self.pair(item, list)
    }
}

/// Iterate over a list's items.
pub fn iter<'a, T: 'a, S>(list: &'a S) -> impl Iterator<Item = &T>
where
    S: List<T>,
{
    let mut cursor = list;
    (0..)
        .map(move |_| {
            let item = cursor.first();
            cursor = cursor.rest()?;
            item
        })
        .take_while(|x| x.is_some())
        .map(Option::unwrap)
}

/// Return a new list with all items replaced by the result of calling
/// a function on them.
pub fn map<T, U, S, R>(list: &S, func: impl FnMut(&T) -> U) -> R
where
    S: List<T>,
    R: List<U>,
    StatelessFactory: ListFactory<U, R>,
{
    factory::map(list, &mut StatelessFactory, func)
}

/// Left fold
pub fn fold_left<A, T, S>(list: &S, acc: A, mut func: impl FnMut(A, &T) -> A) -> A
where
    S: List<T>,
{
    if list.is_empty() {
        acc
    } else {
        let acc = func(acc, list.first().unwrap());
        fold_left(list.rest().unwrap(), acc, func)
    }
}

/// Right fold
pub fn fold_right<A, T, S>(list: &S, acc: A, func: impl FnMut(A, &T) -> A) -> A
where
    S: List<T>,
{
    let (result, _) = foldr(list, acc, func);
    result
}

fn foldr<A, T, S, P>(list: &S, acc: A, func: P) -> (A, P)
where
    S: List<T>,
    P: FnMut(A, &T) -> A,
{
    if list.is_empty() {
        (acc, func)
    } else {
        let (acc, mut func) = foldr(list.rest().unwrap(), acc, func);
        (func(acc, list.first().unwrap()), func)
    }
}

/// Return `true` if `expr` is a proper list.
pub fn is_list<T, S>(expr: &S) -> bool
where
    S: List<T>,
{
    expr.is_empty() || expr.rest().map(is_list).unwrap_or(false)
}

/// Return the length if `list`.
pub fn length<T, S>(list: &S) -> usize
where
    S: List<T>,
{
    fold_left(list, 0, |n, _| n + 1)
}

/// Return the `k`th element of `list`.
/// Return `None` if `k` equals the list's length or more.
pub fn get<T, S>(list: &S, k: usize) -> Option<&T>
where
    S: List<T>,
{
    tail(list, k).and_then(List::first)
}

/// Return the sublist of `list` obtained by omitting the first `k` elements.
/// Return `None` if `k` exceeds the list's length.
pub fn tail<T, S>(list: &S, k: usize) -> Option<&S>
where
    S: List<T>,
{
    if k == 0 {
        Some(list)
    } else {
        tail(list.rest()?, k - 1)
    }
}

/// Return the sublist of `list` obtained by omitting the first `k` elements.
/// Return `None` if `k` exceeds the list's length.
pub fn head<T, S>(list: &S, k: usize) -> Option<S>
where
    StatelessFactory: CopyTracker<T> + ListFactory<T, S>,
    S: List<T>,
{
    factory::head(list, k, &mut StatelessFactory)
}

/// Returns a list consisting of the elements of `left` followed by `right`.
pub fn append<T, S>(left: &S, right: &S) -> S
where
    StatelessFactory: CopyTracker<T> + CopyTracker<S> + ListFactory<T, S>,
    S: List<T>,
{
    factory::append(left, right, &mut StatelessFactory)
}

/// Return the reverse of `list`.
pub fn reverse<T, S>(list: &S) -> S
where
    StatelessFactory: CopyTracker<T> + ListFactory<T, S>,
    S: List<T>,
{
    factory::reverse(list, &mut StatelessFactory)
}

/// Returns the first sublist of `list` whose first element satisfies the predicate.
pub fn find<T, S>(list: &S, pred: impl Fn(&T) -> bool) -> Option<&S>
where
    S: List<T>,
{
    if pred(list.first()?) {
        Some(list)
    } else {
        find(list.rest()?, pred)
    }
}

/// Returns the first sublist of `list` whose first element and `obj` are equivalent
/// according to [`val_eq`].
///
/// [`val_eq`]: crate::equality::ValueEq::val_eq
pub fn member<'a, T, S>(list: &'a S, obj: &T) -> Option<&'a S>
where
    T: ValueEq,
    S: List<T>,
{
    find(list, |item| item.val_eq(obj))
}

/// Returns the first sublist of `list` whose first element and `obj` are equivalent
/// according to [`id_eq`].
///
/// [`id_eq`]: crate::equality::IdentityEq::id_eq
pub fn memv<'a, T, S>(list: &'a S, obj: &T) -> Option<&'a S>
where
    T: IdentityEq,
    S: List<T>,
{
    find(list, |item| item.id_eq(obj))
}

/// Returns the first sublist of `list` whose first element and `obj` are equivalent
/// according to [`ptr_eq`].
///
/// [`ptr_eq`]: crate::equality::PointerEq::ptr_eq
pub fn memq<'a, T, S>(list: &'a S, obj: &T) -> Option<&'a S>
where
    T: PointerEq,
    S: List<T>,
{
    find(list, |item| item.ptr_eq(obj))
}

/// Returns the first pair in `list` whose first element satisfies the predicate.
pub fn afind<K, T, S>(list: &S, pred: impl Fn(&K) -> bool) -> Option<&T>
where
    T: MaybePair<First = K>,
    S: List<T>,
{
    find(list, |item| {
        if let Some(key) = item.first() {
            pred(key)
        } else {
            false
        }
    })
    .and_then(S::first)
}

/// Returns the first pair in `list` whose first element and `obj` are equivalent
/// according to [`val_eq`].
///
/// [`val_eq`]: crate::equality::ValueEq::val_eq
pub fn assoc<'a, K, T, S>(list: &'a S, obj: &K) -> Option<&'a T>
where
    T: MaybePair<First = K>,
    K: ValueEq,
    S: List<T>,
{
    afind(list, |key| key.val_eq(obj))
}

/// Returns the first pair in `list` whose first element and `obj` are equivalent
/// according to [`id_eq`].
///
/// [`id_eq`]: crate::equality::IdentityEq::id_eq
pub fn assv<'a, K, T, S>(list: &'a S, obj: &K) -> Option<&'a T>
where
    T: MaybePair<First = K>,
    K: IdentityEq,
    S: List<T>,
{
    afind(list, |key| key.id_eq(obj))
}

/// Returns the first pair in `list` whose first element and `obj` are equivalent
/// according to [`ptr_eq`].
///
/// [`ptr_eq`]: crate::equality::PointerEq::ptr_eq
pub fn assq<'a, K, T, S>(list: &'a S, obj: &K) -> Option<&'a T>
where
    T: MaybePair<First = K>,
    K: PointerEq,
    S: List<T>,
{
    afind(list, |key| key.ptr_eq(obj))
}

/// Creates a [`List`] containing the arguments.
///
/// [`List`]: crate::lists::List
#[macro_export]
macro_rules! list {
    () => {$crate::factory_traits::StatelessFactory.null()};
    ($x:expr) => {list![$x,]};
    ($x:expr, $($rest:expr),*) => {
        $crate::lists::ListFactory::cons(
            &mut $crate::factory_traits::StatelessFactory,
            $x,
            list![$($rest),*]
        )
    };
}

/// API for types that require custom factories for construction.
pub mod factory {
    use super::*;

    /// Return a new list with all items replaced by the result of calling
    /// a function on them.
    pub fn map<T, U, S, R, F>(list: &S, factory: &mut F, mut func: impl FnMut(&T) -> U) -> R
    where
        S: List<T>,
        R: List<U>,
        F: ListFactory<U, R>,
    {
        if list.is_empty() {
            factory.empty()
        } else {
            let first = func(list.first().unwrap());
            let rest = map(list.rest().unwrap(), factory, func);
            factory.cons(first, rest)
        }
    }

    /// Returns a list consisting of the elements of `left` followed by `right`.
    pub fn append<T, S, F>(left: &S, right: &S, factory: &mut F) -> S
    where
        F: CopyTracker<T> + CopyTracker<S> + ListFactory<T, S>,
        S: List<T>,
    {
        let right = factory.copy_value(right);
        fold_right(left, right, |acc, item| {
            let item = factory.copy_value(item);
            factory.cons(item, acc)
        })
    }

    /// Return the reverse of `list`.
    pub fn reverse<T, S, F>(list: &S, factory: &mut F) -> S
    where
        F: CopyTracker<T> + ListFactory<T, S>,
        S: List<T>,
    {
        fold_left(list, factory.empty(), |acc, item| {
            let item = factory.copy_value(item);
            factory.cons(item, acc)
        })
    }

    /// Return a list containing the first `k` elements of `list`.
    /// Return `None` if `k` exceeds the list's length.
    pub fn head<T, S, F>(list: &S, k: usize, factory: &mut F) -> Option<S>
    where
        F: CopyTracker<T> + ListFactory<T, S>,
        S: List<T>,
    {
        if k == 0 {
            Some(factory.empty())
        } else {
            let item = factory.copy_value(list.first()?);
            let rest = head(list.rest()?, k - 1, factory)?;
            Some(factory.cons(item, rest))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_traits::{MaybePair, Nullable};
    use crate::factory_traits::{NullFactory, PairFactory, StatelessFactory};

    #[derive(Debug, Clone)]
    enum MyList<T> {
        Empty,
        Item(T, Box<MyList<T>>),
    }

    impl<T: PartialEq> PartialEq for MyList<T> {
        fn eq(&self, other: &Self) -> bool {
            use MyList::*;
            match (self, other) {
                (Empty, Empty) => true,
                (Item(a, ta), Item(b, tb)) => a == b && ta == tb,
                _ => false,
            }
        }
    }

    impl<T> Nullable for MyList<T> {
        fn is_null(&self) -> bool {
            matches!(self, MyList::Empty)
        }
    }

    impl<T> NullFactory<MyList<T>> for StatelessFactory {
        fn null(&mut self) -> MyList<T> {
            MyList::Empty
        }
    }

    impl<T> MaybePair for MyList<T> {
        type First = T;
        type Second = MyList<T>;

        fn first(&self) -> Option<&Self::First> {
            match self {
                MyList::Empty => None,
                MyList::Item(x, _) => Some(x),
            }
        }

        fn second(&self) -> Option<&Self::Second> {
            match self {
                MyList::Empty => None,
                MyList::Item(_, tail) => Some(tail),
            }
        }
    }

    impl<T> PairFactory<MyList<T>> for StatelessFactory {
        fn pair(&mut self, first: T, second: MyList<T>) -> MyList<T> {
            MyList::Item(first.into(), Box::new(second.into()))
        }
    }

    #[test]
    fn length_of_empty_list_is_zero() {
        let list: MyList<()> = list![];
        let result = length(&list);
        assert_eq!(result, 0);
    }

    #[test]
    fn length_of_arbitrary_list_is_equal_to_number_of_items() {
        let result = length(&list![1, 2, 3]);
        assert_eq!(result, 3);
    }

    #[test]
    fn append_to_empty_list_produces_second_list() {
        let list1 = list![];
        let list2 = list![4, 5, 6];
        let result = append(&list1, &list2);
        assert_eq!(result, list2)
    }

    #[test]
    fn append_an_empty_list_produces_first_list() {
        let list1 = list![1, 2, 3];
        let list2 = list![];
        let result = append(&list1, &list2);
        assert_eq!(result, list1)
    }

    #[test]
    fn append_two_lists() {
        let list1 = list![1, 2, 3];
        let list2 = list![4, 5, 6];
        let result = append(&list1, &list2);
        assert_eq!(result, list![1, 2, 3, 4, 5, 6])
    }

    #[test]
    fn reverse_empty_list_produces_empty_list() {
        let list: MyList<()> = list![];
        let result = reverse(&list);
        assert_eq!(result, list![]);
    }

    #[test]
    fn reverse_single_element_list_produces_same_list() {
        let list = list![42];
        let result = reverse(&list);
        assert_eq!(result, list);
    }

    #[test]
    fn reverse_list() {
        let result = reverse(&list![1, 2, 3]);
        assert_eq!(result, list![3, 2, 1]);
    }

    #[test]
    fn list_tail_zero_is_same_list() {
        let list = list![1, 2, 3];
        let result = tail(&list, 0);
        assert_eq!(result, Some(&list));
    }

    #[test]
    fn list_tail_omits_k_items() {
        let list = list![1, 2, 3, 4];
        let result = tail(&list, 2);
        assert_eq!(result, Some(&list![3, 4]));
    }

    #[test]
    fn list_tail_returns_none_if_k_above_list_length() {
        let list = list![1, 2, 3];
        let result = tail(&list, 4);
        assert_eq!(result, None);
    }

    #[test]
    fn list_ref_returns_none_if_k_is_list_length() {
        let list = list![1, 2, 3];
        let result = get(&list, 3);
        assert_eq!(result, None);
    }

    #[test]
    fn list_ref_returns_kth_element() {
        let list = list![1, 2, 3];
        let result = get(&list, 1);
        assert_eq!(result, Some(&2));
    }

    #[test]
    fn list_head_zero_creates_empty_list() {
        let list = list![1, 2, 3];
        let result = head(&list, 0);
        assert_eq!(result, Some(list![]));
    }

    #[test]
    fn list_head_returns_same_list_if_k_equals_list_length() {
        let list = list![1, 2, 3];
        let result = head(&list, 4);
        assert_eq!(result, None);
    }

    #[test]
    fn list_head_returns_none_if_k_exceeds_list_length() {
        let list = list![1, 2, 3];
        let result = head(&list, 3);
        assert_eq!(result, Some(list));
    }

    #[test]
    fn list_head_returns_k_elements() {
        let list = list![1, 2, 3];
        let result = head(&list, 2);
        assert_eq!(result, Some(list![1, 2]));
    }

    #[test]
    fn map_list() {
        let list = list![1, 2, 3];
        let result = map(&list, |&x| x * x);
        assert_eq!(result, list![1, 4, 9]);
    }

    #[test]
    fn find_in_list() {
        let list = list![1, 2, 3];
        let result = find(&list, |&x| x == 2);
        assert_eq!(result, Some(&list![2, 3]));
    }

    #[test]
    fn find_in_list_returns_none_if_not_found() {
        let list = list![1, 2, 3];
        let result = find(&list, |_| false);
        assert_eq!(result, None);
    }
}
