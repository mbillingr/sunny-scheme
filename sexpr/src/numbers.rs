use crate::core_traits::MaybeNumber;
use crate::factory_traits::NumberFactory;
use std::ops::Add;

macro_rules! define_binary_operations {
    ($(
        $(#[$extra_docs:meta])*
        $trait:ident :: $method:ident;
    )*) => {
        $(
            $(#[$extra_docs])*
            /// Returns `None` if either argument is not a number.
            pub fn $method<N, S, F>(a: &S, b: &S, factory: &mut F) -> Option<S>
                where
                        for<'a> &'a N: std::ops::$trait<Output = N>,
                        S: crate::core_traits::MaybeNumber<N>,
                        F: crate::factory_traits::NumberFactory<N, S>,
            {
                let a = a.to_number()?;
                let b = b.to_number()?;
                let c = factory.number(std::ops::$trait::$method(a, b));
                Some(c)
            }
        )*

        mod convenience_binop {
            use crate::factory_traits::{DummyFactory, NumberFactory};

            $(
                $(#[$extra_docs])*
                /// Returns `None` if either argument is not a number.
                pub fn $method<N, S>(a: &S, b: &S) -> Option<S>
                where
                    for<'a> &'a N: std::ops::$trait<Output = N>,
                    S: crate::core_traits::MaybeNumber<N>,
                    DummyFactory: NumberFactory<N, S>,
                {
                    super::$method(a, b, &mut DummyFactory)
                }
            )*
        }
    };
}

define_binary_operations!(
    /// Add two numbers.
    Add::add;

    /// Subtract two numbers.
    Sub::sub;

    /// Multiply two numbers.
    Mul::mul;

    /// Divide two numbers.
    Div::div;

    /// Remainder of dividing two numbers.
    Rem::rem;

    /// Bitwise AND of two numbers.
    BitAnd::bitand;

    /// Bitwise OR of two numbers.
    BitOr::bitor;

    /// Bitwise XOR of two numbers.
    BitXor::bitxor;
);

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

    pub use super::convenience_binop::{add, bitand, bitor, bitxor, div, mul, rem, sub};

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

    use Num::*;

    #[derive(Debug, PartialEq)]
    enum Num {
        NaN,
        N(i32),
    }

    impl MaybeNumber<i32> for Num {
        fn to_number(&self) -> Option<&i32> {
            match self {
                Num::NaN => None,
                Num::N(n) => Some(n),
            }
        }
    }

    impl NumberFactory<i32, Num> for DummyFactory {
        fn number(&mut self, n: i32) -> Num {
            Num::N(n)
        }
        fn raw_zero(&mut self) -> i32 {
            0
        }
    }

    #[test]
    fn adding_two_numbers_produces_sum_of_numbers() {
        let c = add(&N(1), &N(2));
        assert_eq!(c, Some(N(3)));
    }

    #[test]
    fn adding_two_non_numbers_produces_none() {
        let c = add(&NaN, &NaN);
        assert_eq!(c, None);
    }

    #[test]
    fn adding_number_and_non_number_produces_none() {
        assert_eq!(add(&N(1), &NaN), None);
        assert_eq!(add(&NaN, &N(1)), None);
    }

    #[test]
    fn subtract_numbers() {
        let c = sub(&N(5), &N(3));
        assert_eq!(c, Some(N(2)));
    }

    #[test]
    fn multiply_numbers() {
        let c = mul(&N(5), &N(3));
        assert_eq!(c, Some(N(15)));
    }

    #[test]
    fn divide_numbers() {
        let c = div(&N(12), &N(3));
        assert_eq!(c, Some(N(4)));
    }

    #[test]
    fn remainder_numbers() {
        let c = rem(&N(11), &N(3));
        assert_eq!(c, Some(N(2)));
    }

    #[test]
    fn bitwise_and_numbers() {
        let c = bitand(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Some(N(0b_1000)));
    }

    #[test]
    fn bitwise_or_numbers() {
        let c = bitor(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Some(N(0b_1110)));
    }

    #[test]
    fn bitwise_xor_numbers() {
        let c = bitxor(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Some(N(0b_0110)));
    }
}
