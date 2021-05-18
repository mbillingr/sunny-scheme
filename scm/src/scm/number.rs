use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use num::traits::Pow;
use num::{BigInt, BigRational, One, Signed, ToPrimitive, Zero};
use std::any::Any;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    BigInt(BigInt),
    Rational(BigRational),
    Float(f64),
}

impl Number {
    pub fn int(x: i64) -> Self {
        Number::Int(x)
    }

    pub fn float(x: f64) -> Self {
        Number::Float(x)
    }

    pub fn rational(num: impl Into<BigInt>, den: impl Into<BigInt>) -> Self {
        Number::Rational(BigRational::new(num.into(), den.into()))
    }

    pub fn zero() -> Self {
        Number::Int(0)
    }

    pub fn one() -> Self {
        Number::Int(1)
    }

    pub fn to_u8(&self) -> Option<u8> {
        match *self {
            Number::Int(i) if i >= u8::MIN as i64 && i <= u8::MAX as i64 => Some(i as u8),
            _ => None,
        }
    }

    pub fn to_i64(&self) -> Option<&i64> {
        match self {
            Number::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn upcast(&self, target: &Self) -> Cow<Number> {
        use Number::*;
        match (self, target) {
            (Int(i), BigInt(_)) => Cow::Owned(Number::BigInt((*i).into())),
            (Int(i), Rational(_)) => Cow::Owned(Number::rational(*i, 1)),
            (Int(i), Float(_)) => Cow::Owned(Number::float(*i as f64)),

            (BigInt(i), Rational(_)) => Cow::Owned(Number::Rational(BigRational::new_raw(
                i.clone(),
                num::BigInt::one(),
            ))),
            (BigInt(i), Float(_)) => Cow::Owned(Number::float(i.to_f64().unwrap())),

            (Rational(r), Float(_)) => Cow::Owned(Number::float(r.to_f64().unwrap())),

            (Int(_), _) | (BigInt(_), _) | (Rational(_), _) | (Float(_), _) => Cow::Borrowed(self),
        }
    }

    fn hash(&self, state: &mut ScmHasher) {
        match self {
            Number::Int(x) => x.hash(state),
            Number::BigInt(x) => x.hash(state),
            Number::Rational(x) => x.hash(state),
            Number::Float(x) => unsafe {
                // Safety: (it's probably UB to cast a float to an int)
                // Hashing a float based on its bit pattern is probably
                // a bad idea, but it's simpler than gracefully handling
                // this as an error...
                std::mem::transmute::<f64, u64>(*x)
            }
            .hash(state),
        }
    }
}

impl ScmObject for Number {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_eqv(&self, other: &dyn ScmObject) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|other| self == other)
            .unwrap_or(false)
    }

    fn eqv_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn equal_hash(&self, state: &mut ScmHasher) {
        self.hash(state)
    }

    fn substitute(&self, _: &HashMap<&str, Scm>) -> Scm {
        self.clone().into()
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Number::Int(x) => write!(f, "{}", x),
            Number::BigInt(x) => write!(f, "{}", x),
            Number::Rational(x) => write!(f, "{}", x),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
}

impl From<i32> for Number {
    fn from(i: i32) -> Self {
        Number::Int(i as i64)
    }
}

impl From<i64> for Number {
    fn from(i: i64) -> Self {
        Number::Int(i)
    }
}

impl From<u64> for Number {
    fn from(i: u64) -> Self {
        if i > i64::MAX as u64 {
            Number::BigInt(i.into())
        } else {
            Number::Int(i as i64)
        }
    }
}

impl From<usize> for Number {
    fn from(i: usize) -> Self {
        if i > i64::MAX as usize {
            Number::BigInt(i.into())
        } else {
            Number::Int(i as i64)
        }
    }
}

impl FromStr for Number {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        println!("parsing {}", s);
        if let Ok(x) = i64::from_str(s) {
            Ok(Number::int(x))
        } else if let Ok(x) = BigInt::from_str(s) {
            Ok(Number::BigInt(x))
        } else if let Ok(x) = BigRational::from_str(s) {
            Ok(Number::Rational(x))
        } else if let Ok(x) = f64::from_str(s) {
            Ok(Number::float(x))
        } else {
            Err(format!("number format not implemented: {}", s))
        }
    }
}

impl Neg for &Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(x) => Number::Int(-*x),
            Number::BigInt(x) => Number::BigInt(-x.clone()),
            Number::Rational(x) => Number::Rational(-x.clone()),
            Number::Float(x) => Number::Float(-*x),
        }
    }
}

impl Add for &Number {
    type Output = Number;

    fn add(self, rhs: &Number) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => {
                if let Some(c) = i64::checked_add(*a, *b) {
                    Number::Int(c)
                } else {
                    let a = num::BigInt::from(*a);
                    Number::BigInt(a + b)
                }
            }
            (BigInt(a), BigInt(b)) => Number::BigInt(a + b),
            (Rational(a), Rational(b)) => Number::Rational(a + b),
            (Float(a), Float(b)) => Number::Float(a + b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                Add::add(&*a, &*b)
            }
        }
    }
}

impl Sub for &Number {
    type Output = Number;

    fn sub(self, rhs: &Number) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => {
                if let Some(c) = i64::checked_sub(*a, *b) {
                    Number::Int(c)
                } else {
                    let a = num::BigInt::from(*a);
                    Number::BigInt(a - b)
                }
            }
            (BigInt(a), BigInt(b)) => Number::BigInt(a - b),
            (Rational(a), Rational(b)) => Number::Rational(a - b),
            (Float(a), Float(b)) => Number::Float(a - b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                Sub::sub(&*a, &*b)
            }
        }
    }
}

impl Mul for &Number {
    type Output = Number;

    fn mul(self, rhs: &Number) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => {
                if let Some(c) = i64::checked_mul(*a, *b) {
                    Number::Int(c)
                } else {
                    let a = num::BigInt::from(*a);
                    Number::BigInt(a * b)
                }
            }
            (BigInt(a), BigInt(b)) => Number::BigInt(a * b),
            (Rational(a), Rational(b)) => Number::Rational(a * b),
            (Float(a), Float(b)) => Number::Float(a * b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                Mul::mul(&*a, &*b)
            }
        }
    }
}

impl Div for &Number {
    type Output = Number;

    fn div(self, rhs: &Number) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => {
                let c = a / b;
                if c * b == *a {
                    Number::Int(c)
                } else {
                    Number::rational(*a, *b)
                }
            }
            (BigInt(a), BigInt(b)) => {
                let c = BigRational::new(a.clone(), b.clone());
                if c.is_integer() {
                    Number::BigInt(c.to_integer())
                } else {
                    Number::Rational(c)
                }
            }
            (Rational(a), Rational(b)) => Number::Rational(a / b),
            (Float(a), Float(b)) => Number::Float(a / b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                Div::div(&*a, &*b)
            }
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Number) -> Option<Ordering> {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => PartialOrd::partial_cmp(a, b),
            (Float(a), Float(b)) => PartialOrd::partial_cmp(a, b),
            (BigInt(a), BigInt(b)) => PartialOrd::partial_cmp(a, b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                PartialOrd::partial_cmp(&*a, &*b)
            }
        }
    }
}

impl Add<i64> for Number {
    type Output = Number;
    fn add(self, rhs: i64) -> Self::Output {
        &self + &Number::int(rhs)
    }
}

impl Number {
    pub fn expt(&self, e: &Number) -> Number {
        use Number::*;
        match (self, e) {
            (Float(b), Int(e)) => Number::Float(b.pow(*e as i32)),
            (Float(b), Float(e)) => Number::Float(b.powf(*e)),
            (b, Int(e)) if *e >= 0 => b.exptint(*e as u64),
            (b, BigInt(e)) if e.is_positive() || e.is_zero() => b.exptbigint(e),
            (b, Rational(e)) => {
                let e = Float(e.to_f64().unwrap());
                b.upcast(&e).expt(&e)
            }
            (b, Float(_)) => b.upcast(e).expt(e),
            _ => unimplemented!(),
        }
    }

    pub fn exptint(&self, e: u64) -> Number {
        use Number::*;
        match (self, e) {
            (_, 0) => Int(1),
            (b, 1) => b.clone(),
            (b, e) if e % 2 == 1 => b * &b.exptint(e - 1),
            (b, e) => (b * b).exptint(e / 2),
        }
    }

    pub fn exptbigint(&self, e: &BigInt) -> Number {
        use Number::*;
        match (self, e) {
            (_, e) if e.is_zero() => Int(1),
            (b, e) if e.is_one() => b.clone(),
            (b, e) if (e % 2i32).is_one() => b * &b.exptbigint(&(e - 1)),
            (b, e) => (b * b).exptbigint(&(e / 2)),
        }
    }
}
