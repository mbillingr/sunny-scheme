pub mod deterministic;
pub mod nondeterministic;

use std::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StateId(pub(super) usize);
