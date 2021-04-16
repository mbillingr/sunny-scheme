use crate::Scm;

impl Scm {
    pub fn try_is_numeq(&self, other: &Self) -> Option<Self> {
        let a = self.as_int()?;
        let b = other.as_int()?;
        Some(Scm::bool(a == b))
    }

    pub fn try_is_less(&self, other: &Self) -> Option<Self> {
        let a = self.as_int()?;
        let b = other.as_int()?;
        Some(Scm::bool(a < b))
    }

    pub fn try_is_greater(&self, other: &Self) -> Option<Self> {
        let a = self.as_int()?;
        let b = other.as_int()?;
        Some(Scm::bool(a > b))
    }

    pub fn try_is_less_or_equal(&self, other: &Self) -> Option<Self> {
        let a = self.as_int()?;
        let b = other.as_int()?;
        Some(Scm::bool(a <= b))
    }

    pub fn try_is_greater_or_equal(&self, other: &Self) -> Option<Self> {
        let a = self.as_int()?;
        let b = other.as_int()?;
        Some(Scm::bool(a >= b))
    }
}
