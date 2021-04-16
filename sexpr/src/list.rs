use crate::core_traits::{Nullable, Pair};
use crate::factory_traits::{CopyTracker, NullFactory, PairFactory};

/// Return `true` if `expr` is a proper list.
pub fn is_list<S>(expr: &S) -> bool
where
    S: Nullable + Pair<First = S, Second = S>,
{
    expr.is_null() || expr.second().map(is_list).unwrap_or(false)
}

/// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
pub fn reverse<T, S, F>(expr: &S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + NullFactory<S> + PairFactory<S>,
    S: Nullable + Pair<First = T, Second = S>,
{
    reverse_iter(expr, factory.null(), factory)
}

fn reverse_iter<T, S, F>(rest: &S, acc: S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + NullFactory<S> + PairFactory<S>,
    S: Nullable + Pair<First = T, Second = S>,
{
    if rest.is_null() {
        Some(acc)
    } else {
        let car = factory.copy_value(rest.first()?);
        let acc = factory.cons(car, acc);
        reverse_iter(rest.second()?, acc, factory)
    }
}

/// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
pub fn append<T, S, F>(left: &S, right: &S, factory: &mut F) -> Option<S>
where
    F: CopyTracker<T> + CopyTracker<S> + NullFactory<S> + PairFactory<S>,
    S: Nullable + Pair<First = T, Second = S>,
{
    if left.is_null() {
        Some(factory.copy_value(right))
    } else {
        let car = factory.copy_value(left.first()?);
        let cdr = append(left.second()?, right, factory)?;
        Some(factory.cons(car, cdr))
    }
}

/// Convenience interface for types that don't need explicit memory management.
#[macro_use]
pub mod convenience {
    use super::*;
    use crate::factory_traits::DummyFactory;

    /// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
    pub fn reverse<T, S>(expr: &S) -> Option<S>
    where
        DummyFactory: CopyTracker<T> + NullFactory<S> + PairFactory<S>,
        S: Nullable + Pair<First = T, Second = S>,
    {
        super::reverse(expr, &mut DummyFactory)
    }

    /// Return the reverse of the list if `expr` is a proper list and `None` otherwise.
    pub fn append<T, S>(left: &S, right: &S) -> Option<S>
    where
        DummyFactory: CopyTracker<T> + CopyTracker<S> + NullFactory<S> + PairFactory<S>,
        S: Nullable + Pair<First = T, Second = S>,
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
}
