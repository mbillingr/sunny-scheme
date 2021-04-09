use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone)]
pub enum Number {
    Int(i64),
}

#[derive(Debug, Clone)]
pub enum WeakNumber {
    Int(i64),
}

impl Number {
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Number::Int(x) => Some(*x),
        }
    }

    pub fn inv(&self) -> Self {
        match self {
            Number::Int(x) => Number::Int(1 / *x),
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        use Number::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
        }
    }

    pub fn downgrade(&self) -> WeakNumber {
        match self {
            Number::Int(i) => WeakNumber::Int(*i),
        }
    }
}

impl WeakNumber {
    pub fn upgrade(&self) -> Option<Number> {
        match self {
            WeakNumber::Int(i) => Some(Number::Int(*i)),
        }
    }
}

impl From<i32> for Number {
    fn from(x: i32) -> Self {
        Number::Int(x as i64)
    }
}

impl From<i64> for Number {
    fn from(x: i64) -> Self {
        Number::Int(x)
    }
}

impl From<usize> for Number {
    fn from(x: usize) -> Self {
        if x > i64::MAX as usize {
            unimplemented!("Big integers not yet supported.")
        }
        Number::Int(x as i64)
    }
}

impl PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => a == b,
        }
    }
}

impl PartialEq for WeakNumber {
    fn eq(&self, rhs: &Self) -> bool {
        use WeakNumber::*;
        match (self, rhs) {
            (Int(a), Int(b)) => a == b,
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Number::*;
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
        }
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Number::Int(x) => x.fmt(f),
        }
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Int(i) => i.hash(state),
        }
    }
}

impl Hash for WeakNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            WeakNumber::Int(i) => i.hash(state),
        }
    }
}

impl Neg for &Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        match self {
            Number::Int(x) => Number::Int(-*x),
        }
    }
}

impl Add for &Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a + b),
        }
    }
}

impl Sub for &Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a - b),
        }
    }
}

impl Mul for &Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a * b),
        }
    }
}

impl Div for &Number {
    type Output = Number;
    fn div(self, rhs: Self) -> Self::Output {
        use Number::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a / b),
        }
    }
}
