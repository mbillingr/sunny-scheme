use crate::core_traits::{MaybePair, Nullable};
use crate::factory_traits::{CopyTracker, NullFactory, PairFactory};

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

/// Returns a list consisting of the elements of the `left` followed by the elements `right`.
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

/// Return the reverse of the list if `expr`.
pub fn reverse<T, S, F>(expr: &S, factory: &mut F) -> S
where
    F: CopyTracker<T> + ListFactory<T, S>,
    S: List<T>,
{
    fold_left(expr, factory.empty(), |acc, item| {
        let item = factory.copy_value(item);
        factory.cons(item, acc)
    })
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

/// Convenience interface for types that don't need explicit memory management.
#[macro_use]
pub mod convenience {
    use super::*;
    use crate::factory_traits::DummyFactory;

    pub use super::{get, is_list, length, tail};

    /// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
    pub fn reverse<T, S>(expr: &S) -> S
    where
        DummyFactory: CopyTracker<T> + ListFactory<T, S>,
        S: List<T>,
    {
        super::reverse(expr, &mut DummyFactory)
    }

    /// Returns a list consisting of the elements of the `left` followed by the elements `right`.
    /// Returns `None` if `left` is not a proper list.
    pub fn append<T, S>(left: &S, right: &S) -> S
    where
        DummyFactory: CopyTracker<T> + CopyTracker<S> + ListFactory<T, S>,
        S: List<T>,
    {
        super::append(left, right, &mut DummyFactory)
    }

    /// Return the sublist of `list` obtained by omitting the first `k` elements.
    /// Return `None` if `k` exceeds the list's length.
    pub fn head<T, S>(list: &S, k: usize) -> Option<S>
    where
        DummyFactory: CopyTracker<T> + ListFactory<T, S>,
        S: List<T>,
    {
        super::head(list, k, &mut DummyFactory)
    }

    #[macro_export]
    macro_rules! list {
        () => {crate::factory_traits::DummyFactory.null()};
        ($x:expr) => {list![$x,]};
        ($x:expr, $($rest:expr),*) => {
            crate::factory_traits::DummyFactory.pair($x, list![$($rest),*])
        };
    }
}

#[cfg(test)]
mod tests {
    use super::convenience::*;
    use crate::core_traits::{MaybePair, Nullable};
    use crate::factory_traits::{DummyFactory, NullFactory, PairFactory};

    #[derive(Debug, Clone)]
    enum List<T> {
        Empty,
        Item(T, Box<List<T>>),
    }

    impl<T: PartialEq> PartialEq for List<T> {
        fn eq(&self, other: &Self) -> bool {
            use List::*;
            match (self, other) {
                (Empty, Empty) => true,
                (Item(a, ta), Item(b, tb)) => a == b && ta == tb,
                _ => false,
            }
        }
    }

    impl<T> Nullable for List<T> {
        fn is_null(&self) -> bool {
            matches!(self, List::Empty)
        }
    }

    impl<T> NullFactory<List<T>> for DummyFactory {
        fn null(&mut self) -> List<T> {
            List::Empty
        }
    }

    impl<T> MaybePair for List<T> {
        type First = T;
        type Second = List<T>;

        fn first(&self) -> Option<&Self::First> {
            match self {
                List::Empty => None,
                List::Item(x, _) => Some(x),
            }
        }

        fn second(&self) -> Option<&Self::Second> {
            match self {
                List::Empty => None,
                List::Item(_, tail) => Some(tail),
            }
        }
    }

    impl<T> PairFactory<List<T>> for DummyFactory {
        fn pair(&mut self, first: T, second: List<T>) -> List<T> {
            List::Item(first.into(), Box::new(second.into()))
        }
    }

    #[test]
    fn length_of_empty_list_is_zero() {
        let list: List<()> = list![];
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
        let list: List<()> = list![];
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
}
