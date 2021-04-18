use crate::core_traits::MaybeNumber;
use crate::factory_traits::NumberFactory;
use std::ops::Add;

/// Add two numbers. Returns `None` if either argument is not a number.
pub fn add<N, S, F>(a: &S, b: &S, factory: &mut F) -> Option<S>
where
    for<'a> &'a N: Add<Output = N>,
    S: MaybeNumber<N>,
    F: NumberFactory<N, S>,
{
    let a = a.to_number()?;
    let b = b.to_number()?;
    let c = factory.number(a + b);
    Some(c)
}

/// Sum multiple numbers. Returns `None` if any value is not a number.
pub fn sum<'a, N, S: 'a, F>(values: impl Iterator<Item = &'a S>, factory: &mut F) -> Option<S>
where
    for<'b> &'b N: Add<Output = N>,
    S: MaybeNumber<N>,
    F: NumberFactory<N, S>,
{
    let mut acc = factory.raw_zero();
    for x in values {
        let x = x.to_number()?;
        acc = &acc + x;
    }
    Some(factory.number(acc))
}

/// Convenience interface for types that don't need explicit memory management.
#[macro_use]
pub mod convenience {
    use super::*;
    use crate::factory_traits::DummyFactory;

    /// Add two numbers. Returns `None` if either argument is not a number.
    pub fn add<N, S>(a: &S, b: &S) -> Option<S>
    where
        for<'a> &'a N: Add<Output = N>,
        S: MaybeNumber<N>,
        DummyFactory: NumberFactory<N, S>,
    {
        super::add(a, b, &mut DummyFactory)
    }

    /// Sum multiple numbers. Returns `None` if any value is not a number.
    pub fn sum<'a, N, S: 'a>(values: impl Iterator<Item = &'a S>) -> Option<S>
    where
        for<'b> &'b N: Add<Output = N>,
        S: MaybeNumber<N>,
        DummyFactory: NumberFactory<N, S>,
    {
        super::sum(values, &mut DummyFactory)
    }
}

#[cfg(test)]
mod tests {
    use super::convenience::*;
    use crate::core_traits::MaybeNumber;
    use crate::factory_traits::{DummyFactory, NumberFactory};

    impl MaybeNumber<i32> for i32 {
        fn to_number(&self) -> Option<&i32> {
            Some(self)
        }
    }

    impl NumberFactory<i32, i32> for DummyFactory {
        fn number(&mut self, n: i32) -> i32 {
            n
        }
        fn raw_zero(&mut self) -> i32 {
            0
        }
    }

    #[test]
    fn adding_two_exact_numbers_produces_sum_of_numbers() {
        let c = add(&1, &2);
        assert_eq!(c, Some(3));
    }
}
