use std::hash::{Hash, Hasher};

#[repr(u8)]
pub enum Value {
    Values(usize), // mark multiple return values on stack
}

impl Default for Value {
    fn default() -> Self {
        unimplemented!()
    }
}

macro_rules! impl_accessor {
    ($pred:ident, $variant:path) => {
        pub fn $pred(&self) -> bool {
            match self {
                $variant => true,
                _ => false,
            }
        }
    };

    ($pred:ident, $conv:ident, $conv_mut:ident, $variant:path, ref $output:ty) => {
        impl_accessor! {$pred, $conv, $variant, ref $output}
        pub fn $conv_mut(&mut self) -> Option<&mut $output> {
            match self {
                $variant(x) => Some(&mut *x),
                _ => None,
            }
        }
    };

    ($pred:ident, $conv:ident, $variant:path, ref $output:ty) => {
        pub fn $pred(&self) -> bool {
            self.$conv().is_some()
        }
        pub fn $conv(&self) -> Option<&$output> {
            match self {
                $variant(x) => Some(&*x),
            }
        }
    };

    ($pred:ident, $conv:ident, $variant:path, $output:ty) => {
        pub fn $pred(&self) -> bool {
            self.$conv().is_some()
        }
        pub fn $conv(&self) -> Option<$output> {
            match self {
                $variant(x) => Some(*x),
            }
        }
    };
}

impl Value {
    impl_accessor!(is_values, as_values, Value::Values, ref usize);

    pub fn get_tag(&self) -> u8 {
        use Value::*;
        match self {
            Values(_) => 254,
        }
    }

    pub fn eqv(&self, rhs: &Self) -> bool {
        self.eq(rhs)
    }

    pub fn shallow_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Values(n) => n.hash(state),
        }
    }

    pub fn deep_hash<H: Hasher>(&self, state: &mut H) {
        self.get_tag().hash(state);
        match self {
            Value::Values(n) => n.hash(state),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Values(n) => write!(f, "<{} values>", n),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Values(a), Values(b)) => a == b,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.shallow_hash(state)
    }
}
