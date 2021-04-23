use maplit::hashmap;
use sexpr_generics::equality::PointerKey;
use std::collections::{HashMap, HashSet};
use sunny_scm::Scm;

#[derive(Debug, PartialEq)]
pub struct MatchBindings {
    bindings: HashMap<PointerKey<Scm>, Binding>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Simple(Scm),
    Repeated(Vec<Binding>),
}

impl MatchBindings {
    pub fn empty() -> Self {
        MatchBindings {
            bindings: HashMap::new(),
        }
    }

    pub fn new(binding: Scm, value: Scm) -> Self {
        MatchBindings {
            bindings: hashmap![binding.into() => Binding::Simple(value)],
        }
    }

    pub fn empty_repetition(identifiers: impl IntoIterator<Item = PointerKey<Scm>>) -> Self {
        MatchBindings {
            bindings: identifiers
                .into_iter()
                .map(|k| (k, Binding::Repeated(vec![])))
                .collect(),
        }
    }

    pub fn repeated(binding: Scm, values: impl IntoIterator<Item = impl Into<Binding>>) -> Self {
        let bindings = values.into_iter().map(Into::into).collect();
        MatchBindings {
            bindings: hashmap![binding.into() => Binding::Repeated(bindings)],
        }
    }

    pub fn from_sequence(bindings: impl IntoIterator<Item = (Scm, Scm)>) -> Self {
        MatchBindings {
            bindings: bindings
                .into_iter()
                .map(|(key, value)| (key.into(), Binding::Simple(value)))
                .collect(),
        }
    }

    /// Add a new binding to the match result
    pub fn join(self, other: Self) -> Self {
        let mut bindings = self.bindings;
        bindings.extend(other.bindings);
        MatchBindings { bindings }
    }

    /// Attach a new repetition to existing bindings
    pub fn attach(&mut self, repetition: Self) {
        for (name, value) in repetition.bindings {
            self.bindings
                .entry(name)
                .or_insert(Binding::Repeated(vec![]))
                .attach(value)
        }
    }

    pub fn unwrap_ellipsis(&self) -> Option<impl Iterator<Item = Self> + '_> {
        let n_reps: HashSet<usize> = self
            .bindings
            .values()
            .filter_map(|b| match b {
                Binding::Simple(_) => None,
                Binding::Repeated(r) => Some(r.len()),
            })
            .collect();

        if n_reps.len() != 1 {
            return None;
        }
        let n_reps = n_reps.into_iter().next().unwrap();

        Some((0..n_reps).map(move |i| {
            let bindings = self
                .bindings
                .iter()
                .map(|(name, b)| match b {
                    Binding::Simple(_) => (name.clone(), b.clone()),
                    Binding::Repeated(reps) => (name.clone(), reps[i].clone()),
                })
                .collect();
            MatchBindings { bindings }
        }))
    }

    pub fn lookup(&self, key: &Scm) -> Option<&Scm> {
        match self.bindings.get(PointerKey::from_ref(key))? {
            Binding::Simple(value) => Some(value),
            Binding::Repeated(_) => None,
        }
    }
}

impl Binding {
    fn attach(&mut self, value: Self) {
        match self {
            Binding::Simple(_) => panic!("can't attach repetition to simple binding"),
            Binding::Repeated(reps) => reps.push(value),
        }
    }
}

impl<T> From<Vec<T>> for Binding
where
    T: Into<Binding>,
{
    fn from(v: Vec<T>) -> Binding {
        Binding::Repeated(v.into_iter().map(Into::into).collect())
    }
}

impl From<i64> for Binding {
    fn from(x: i64) -> Binding {
        Binding::Simple(x.into())
    }
}
