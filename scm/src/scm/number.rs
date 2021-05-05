use crate::scm::ScmHasher;
use crate::{Scm, ScmObject};
use std::any::Any;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    pub fn int(x: i64) -> Self {
        Number::Int(x)
    }
    pub fn float(x: f64) -> Self {
        Number::Float(x)
    }

    pub fn zero() -> Self {
        Number::Int(0)
    }

    pub fn one() -> Self {
        Number::Int(1)
    }

    pub fn as_int(&self) -> Option<&i64> {
        match self {
            Number::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn upcast(&self, target: &Self) -> Cow<Number> {
        use Number::*;
        match (self, target) {
            (Int(i), Float(_)) => Cow::Owned(Number::float(*i as f64)),
            (Int(_), _) | (Float(_), _) => Cow::Borrowed(self),
        }
    }

    fn hash(&self, state: &mut ScmHasher) {
        match self {
            Number::Int(x) => x.hash(state),
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
        (*self).into()
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Number::Int(x) => write!(f, "{}", x),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
}

impl From<i64> for Number {
    fn from(i: i64) -> Self {
        Number::Int(i)
    }
}

impl Neg for &Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(x) => Number::Int(-*x),
            Number::Float(x) => Number::Float(-*x),
        }
    }
}

impl Add for &Number {
    type Output = Number;

    fn add(self, rhs: &Number) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Number::Int(a + b),
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
            (Int(a), Int(b)) => Number::Int(a - b),
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
            (Int(a), Int(b)) => Number::Int(a * b),
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
            (Int(a), Int(b)) => Number::Float(*a as f64 / *b as f64),
            (Float(a), Float(b)) => Number::Float(a / b),
            _ => {
                let a = self.upcast(rhs);
                let b = rhs.upcast(self);
                Div::div(&*a, &*b)
            }
        }
    }
}
