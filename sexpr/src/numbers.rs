use crate::core_traits::MaybeNumber;
use crate::factory_traits::NumberFactory;
use std::ops::Add;

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

/// Convenience interface for types that don't need explicit memory management.
#[macro_use]
pub mod convenience {
    use super::*;
    use crate::factory_traits::DummyFactory;

    pub fn add<N, S>(a: &S, b: &S) -> Option<S>
    where
        for<'a> &'a N: Add<Output = N>,
        S: MaybeNumber<N>,
        DummyFactory: NumberFactory<N, S>,
    {
        super::add(a, b, &mut DummyFactory)
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
    }

    #[test]
    fn adding_two_exact_numbers_produces_sum_of_numbers() {
        let c = add(&1, &2);
        assert_eq!(c, Some(3));
    }
}
