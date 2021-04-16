use crate::core_traits::{Nullable, Pair};
use crate::factory_traits::{CopyTracker, NullFactory, PairFactory};

/// Trait for types that can represent lists.
pub trait List<T> {
    /// Return `true` if the value represents an empty list.
    fn is_empty(&self) -> bool;

    /// Return a reference to the list's first element
    /// or `None` if the value does not represent a list.
    fn first(&self) -> Option<&T>;

    /// Return a reference to the sublist after the first element
    /// or `None` if the value does not represent a list.
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
    S: Nullable + Pair<First = T, Second = S>,
{
    fn is_empty(&self) -> bool {
        self.is_null()
    }

    fn first(&self) -> Option<&T> {
        Pair::first(self)
    }

    fn rest(&self) -> Option<&Self> {
        Pair::second(self)
    }
}

impl<T, S, F> ListFactory<T, S> for F
where
    S: Nullable + Pair<First = T, Second = S>,
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
/// a function on them. Returns `None` if `list` is not a proper list.
pub fn map<T, U, S, R, F>(list: &S, factory: &mut F, func: impl Fn(&T) -> U) -> Option<R>
where
    S: List<T>,
    R: List<U>,
    F: ListFactory<U, R>,
{
    if list.is_empty() {
        Some(factory.empty())
    } else {
        let first = func(list.first()?);
        let rest = map(list.rest()?, factory, func)?;
        Some(factory.cons(first, rest))
    }
}

/// Return a new list with all items replaced by the result of calling
/// a function on them. Returns `None` if `list` is not a proper list.
pub fn fold<A, T, S>(list: &S, acc: A, func: impl Fn(A, &T) -> A) -> Option<A>
where
    S: List<T>,
{
    if list.is_empty() {
        Some(acc)
    } else {
        let acc = func(acc, list.first()?);
        fold(list.rest()?, acc, func)
    }
}

/// Return `true` if `expr` is a proper list.
pub fn is_list<T, S>(expr: &S) -> bool
where
    S: List<T>,
{
    expr.is_empty() || expr.rest().map(is_list).unwrap_or(false)
}

/// Return the length if `list` is a proper list and `None` otherwise.
pub fn length<T, S>(list: &S) -> Option<usize>
where
    S: List<T>,
{
    fold(list, 0, |n, _| n + 1)
}

/// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
pub fn append<T, S, F>(left: &S, right: &S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + CopyTracker<S> + ListFactory<T, S>,
    S: List<T>,
{
    if left.is_empty() {
        Some(factory.copy_value(right))
    } else {
        let car = factory.copy_value(left.first()?);
        let cdr = append(left.rest()?, right, factory)?;
        Some(factory.cons(car, cdr))
    }
}

/// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
pub fn reverse<T, S, F>(expr: &S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + ListFactory<T, S>,
    S: List<T>,
{
    reverse_iter(expr, factory.empty(), factory)
}

fn reverse_iter<T, S, F>(list: &S, acc: S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + ListFactory<T, S>,
    S: List<T>,
{
    if list.is_empty() {
        Some(acc)
    } else {
        let car = factory.copy_value(list.first()?);
        let acc = factory.cons(car, acc);
        reverse_iter(list.rest()?, acc, factory)
    }
}

/// Convenience interface for types that don't need explicit memory management.
#[macro_use]
pub mod convenience {
    use super::*;
    use crate::factory_traits::DummyFactory;

    pub use super::{is_list, length};

    /// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
    pub fn reverse<T, S>(expr: &S) -> Option<S>
    where
        DummyFactory: CopyTracker<T> + ListFactory<T, S>,
        S: List<T>,
    {
        super::reverse(expr, &mut DummyFactory)
    }

    /// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
    pub fn append<T, S>(left: &S, right: &S) -> Option<S>
    where
        DummyFactory: CopyTracker<T> + CopyTracker<S> + ListFactory<T, S>,
        S: List<T>,
    {
        super::append(left, right, &mut DummyFactory)
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
    use crate::core_traits::{Nullable, Pair};
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

    impl<T> Pair for List<T> {
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
    }

    #[test]
    fn length_of_arbitrary_list_is_equal_to_number_of_items() {
        let result = length(&list![1, 2, 3]);
        assert_eq!(result, Some(3));
    }

    #[test]
    fn append_to_empty_list_produces_second_list() {
        let list1 = list![];
        let list2 = list![4, 5, 6];
        let result = append(&list1, &list2);
        assert_eq!(result, Some(list2))
    }

    #[test]
    fn append_an_empty_list_produces_first_list() {
        let list1 = list![1, 2, 3];
        let list2 = list![];
        let result = append(&list1, &list2);
        assert_eq!(result, Some(list1))
    }

    #[test]
    fn append_two_lists() {
        let list1 = list![1, 2, 3];
        let list2 = list![4, 5, 6];
        let result = append(&list1, &list2);
        assert_eq!(result, Some(list![1, 2, 3, 4, 5, 6]))
    }

    #[test]
    fn reverse_empty_list_produces_empty_list() {
        let list: List<()> = list![];
        let result = reverse(&list);
        assert_eq!(result, Some(list![]));
    }

    #[test]
    fn reverse_single_element_list_produces_same_list() {
        let list = list![42];
        let result = reverse(&list);
        assert_eq!(result, Some(list));
    }

    #[test]
    fn reverse_list() {
        let result = reverse(&list![1, 2, 3]);
        assert_eq!(result, Some(list![3, 2, 1]));
    }
}
