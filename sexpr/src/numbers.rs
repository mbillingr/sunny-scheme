//! Generic operations on values that may represent numbers.

use crate::core_traits::MaybeNumber;
use crate::factory_traits::{NumberFactory, StatelessFactory};
use std::ops::{Add, Div, Mul, Neg, Sub};

macro_rules! define_binary_operations {
    ($(
        $(#[$extra_docs:meta])*
        $trait:ident :: $method:ident;
    )*) => {

        $(
            $(#[$extra_docs])*
            /// Returns `Err` if either argument is not a number.
            pub fn $method<'b, N:'b, S>(a: &'b S, b: &'b S) -> Result<S, &'b S>
            where
                &'b N: std::ops::$trait<Output = N>,
                S: crate::core_traits::MaybeNumber<Number = N>,
                StatelessFactory: NumberFactory<S>,
            {
                factory::$method(a, b, &mut StatelessFactory)
            }
        )*

        mod factory_binop {
            $(
                $(#[$extra_docs])*
                /// Returns `Err` if either argument is not a number.
                pub fn $method<'b, N:'b, S, F>(a: &'b S, b: &'b S, factory: &mut F) -> Result<S, &'b S>
                    where
                            &'b N: 'b+std::ops::$trait<Output = N>,
                            S: crate::core_traits::MaybeNumber<Number = N>,
                            F: crate::factory_traits::NumberFactory<S>,
                {
                    let a = $crate::numbers::to_number(a)?;
                    let b = $crate::numbers::to_number(b)?;
                    let c = factory.number(std::ops::$trait::$method(a, b));
                    Ok(c)
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
pub fn sum<'a, N: 'a, S: 'a>(values: impl Iterator<Item = &'a S>) -> Result<S, &'a S>
where
    for<'b> &'b N: Add<Output = N>,
    S: MaybeNumber<Number = N>,
    StatelessFactory: NumberFactory<S>,
{
    factory::sum(values, &mut StatelessFactory)
}

/// Multiply multiple numbers.
/// If any value is not a number returns `Err` with the first non-number.
pub fn prod<'a, N: 'a, S: 'a>(values: impl Iterator<Item = &'a S>) -> Result<S, &'a S>
where
    for<'b> &'b N: Mul<Output = N>,
    S: MaybeNumber<Number = N>,
    StatelessFactory: NumberFactory<S>,
{
    factory::prod(values, &mut StatelessFactory)
}

/// Subtract multiple numbers.
/// If any value is not a number returns `Err` with the first non-number.
///
/// This function follows the semantics of Scheme's `-` procedure:
///   - no arguments returns 0
///   - a single argument is negated
///   - more than one argument returns the first minus the sum of all others
pub fn diff<'a, N: 'a, S: 'a>(values: impl Iterator<Item = &'a S>) -> Result<S, &'a S>
where
    for<'b> &'b N: Sub<Output = N>,
    for<'b> &'b N: Neg<Output = N>,
    S: MaybeNumber<Number = N>,
    StatelessFactory: NumberFactory<S>,
{
    factory::diff(values, &mut StatelessFactory)
}

/// Divide multiple numbers.
/// If any value is not a number returns `Err` with the first non-number.
///
/// This function follows the semantics of Scheme's `/` procedure:
///   - no arguments returns 1
///   - a single argument is inverted
///   - more than one argument returns the first divided by the product of all others
pub fn quot<'a, N: 'a, S: 'a>(values: impl Iterator<Item = &'a S>) -> Result<S, &'a S>
where
    for<'b> &'b N: Div<Output = N>,
    S: MaybeNumber<Number = N>,
    StatelessFactory: NumberFactory<S>,
{
    factory::quot(values, &mut StatelessFactory)
}

/// API for types that require custom factories for construction.
pub mod factory {
    use super::*;

    pub use factory_binop::{add, bitand, bitor, bitxor, div, mul, rem, sub};

    /// Sum multiple numbers.
    /// If any value is not a number returns `Err` with the first non-number.
    pub fn sum<'a, N: 'a, S: 'a, F>(
        values: impl Iterator<Item = &'a S>,
        factory: &mut F,
    ) -> Result<S, &'a S>
    where
        for<'b> &'b N: Add<Output = N>,
        S: MaybeNumber<Number = N>,
        F: NumberFactory<S>,
    {
        let mut acc = factory.raw_zero();
        for x in values {
            let x = to_number(x)?;
            acc = &acc + x;
        }
        Ok(factory.number(acc))
    }

    /// Multiply multiple numbers.
    /// If any value is not a number returns `Err` with the first non-number.
    pub fn prod<'a, N: 'a, S: 'a, F>(
        values: impl Iterator<Item = &'a S>,
        factory: &mut F,
    ) -> Result<S, &'a S>
    where
        for<'b> &'b N: Mul<Output = N>,
        S: MaybeNumber<Number = N>,
        F: NumberFactory<S>,
    {
        let mut acc = factory.raw_one();
        for x in values {
            let x = to_number(x)?;
            acc = &acc * x;
        }
        Ok(factory.number(acc))
    }

    /// Subtract multiple numbers.
    /// If any value is not a number returns `Err` with the first non-number.
    ///
    /// This function follows the semantics of Scheme's `-` procedure:
    ///   - no arguments returns 0
    ///   - a single argument is negated
    ///   - more than one argument returns the first minus the sum of all others
    pub fn diff<'a, N: 'a, S: 'a, F>(
        mut values: impl Iterator<Item = &'a S>,
        factory: &mut F,
    ) -> Result<S, &'a S>
    where
        for<'b> &'b N: Sub<Output = N>,
        for<'b> &'b N: Neg<Output = N>,
        S: MaybeNumber<Number = N>,
        F: NumberFactory<S>,
    {
        let first = values.next();
        let second = values.next();
        let mut acc = match (first, second) {
            (None, None) => factory.raw_zero(),
            (Some(a), None) => Neg::neg(to_number(a)?),
            (Some(a), Some(b)) => Sub::sub(to_number(a)?, to_number(b)?),
            (None, Some(_)) => unreachable!(),
        };
        for x in values {
            let x = to_number(x)?;
            acc = &acc - x;
        }
        Ok(factory.number(acc))
    }

    /// Subtract multiple numbers.
    /// If any value is not a number returns `Err` with the first non-number.
    ///
    /// This function follows the semantics of Scheme's `-` procedure:
    ///   - no arguments returns 0
    ///   - a single argument is negated
    ///   - more than one argument returns the first minus the sum of all others
    pub fn quot<'a, N: 'a, S: 'a, F>(
        mut values: impl Iterator<Item = &'a S>,
        factory: &mut F,
    ) -> Result<S, &'a S>
    where
        for<'b> &'b N: Div<Output = N>,
        S: MaybeNumber<Number = N>,
        F: NumberFactory<S>,
    {
        let first = values.next();
        let second = values.next();
        let mut acc = match (first, second) {
            (None, None) => factory.raw_one(),
            (Some(a), None) => &factory.raw_one() / to_number(a)?,
            (Some(a), Some(b)) => to_number(a)? / to_number(b)?,
            (None, Some(_)) => unreachable!(),
        };
        for x in values {
            let x = to_number(x)?;
            acc = &acc / x;
        }
        Ok(factory.number(acc))
    }
}

fn to_number<N, S: MaybeNumber<Number = N>>(x: &S) -> Result<&N, &S> {
    x.to_number().ok_or(x)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_traits::MaybeNumber;
    use crate::factory_traits::{NumberFactory, StatelessFactory};

    use Num::*;

    #[derive(Debug, PartialEq)]
    enum Num<T> {
        NaN,
        N(T),
    }

    impl<T> MaybeNumber for Num<T> {
        type Number = T;
        fn to_number(&self) -> Option<&T> {
            match self {
                Num::NaN => None,
                Num::N(n) => Some(n),
            }
        }
    }

    impl NumberFactory<Num<i32>> for StatelessFactory {
        fn number(&mut self, n: i32) -> Num<i32> {
            Num::N(n)
        }

        fn raw_zero(&mut self) -> i32 {
            0
        }

        fn raw_one(&mut self) -> i32 {
            1
        }
    }

    impl NumberFactory<Num<f32>> for StatelessFactory {
        fn number(&mut self, n: f32) -> Num<f32> {
            Num::N(n)
        }

        fn raw_zero(&mut self) -> f32 {
            0.0
        }

        fn raw_one(&mut self) -> f32 {
            1.0
        }
    }

    #[test]
    fn adding_two_numbers_produces_sum_of_numbers() {
        let c = add(&N(1), &N(2));
        assert_eq!(c, Ok(N(3)));
    }

    #[test]
    fn adding_two_non_numbers_produces_none() {
        let n = Num::<i32>::NaN;
        let c = add(&n, &n);
        assert_eq!(c, Err(&n));
    }

    #[test]
    fn adding_number_and_non_number_produces_none() {
        assert_eq!(add(&N(1), &NaN), Err(&NaN));
        assert_eq!(add(&NaN, &N(1)), Err(&NaN));
    }

    #[test]
    fn subtract_numbers() {
        let c = sub(&N(5), &N(3));
        assert_eq!(c, Ok(N(2)));
    }

    #[test]
    fn multiply_numbers() {
        let c = mul(&N(5), &N(3));
        assert_eq!(c, Ok(N(15)));
    }

    #[test]
    fn divide_numbers() {
        let c = div(&N(12), &N(3));
        assert_eq!(c, Ok(N(4)));
    }

    #[test]
    fn remainder_numbers() {
        let c = rem(&N(11), &N(3));
        assert_eq!(c, Ok(N(2)));
    }

    #[test]
    fn bitwise_and_numbers() {
        let c = bitand(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Ok(N(0b_1000)));
    }

    #[test]
    fn bitwise_or_numbers() {
        let c = bitor(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Ok(N(0b_1110)));
    }

    #[test]
    fn bitwise_xor_numbers() {
        let c = bitxor(&N(0b_1010), &N(0b_1100));
        assert_eq!(c, Ok(N(0b_0110)));
    }

    #[test]
    fn diff_no_arguments_is_zero() {
        let d = diff([].iter());
        assert_eq!(d, Ok(N(0)));
    }

    #[test]
    fn diff_negates_single_argument() {
        let d = diff([N(1)].iter());
        assert_eq!(d, Ok(N(-1)));
    }

    #[test]
    fn diff_subtracts_two_argument() {
        let d = diff([N(5), N(3)].iter());
        assert_eq!(d, Ok(N(2)));
    }

    #[test]
    fn diff_subtracts_multiple_argument() {
        let d = diff([N(17), N(5), N(3)].iter());
        assert_eq!(d, Ok(N(9)));
    }

    #[test]
    fn quot_no_arguments_is_one() {
        let d = quot([].iter());
        assert_eq!(d, Ok(N(1)));
    }

    #[test]
    fn diff_inverts_single_argument() {
        let d = quot([N(2.0)].iter());
        assert_eq!(d, Ok(N(0.5)));
    }

    #[test]
    fn quot_divides_two_argument() {
        let d = quot([N(6), N(3)].iter());
        assert_eq!(d, Ok(N(2)));
    }

    #[test]
    fn quot_divides_multiple_argument() {
        let d = quot([N(24), N(3), N(4)].iter());
        assert_eq!(d, Ok(N(2)));
    }
}
