use maplit::hashmap;
use maplit::hashset;
use sexpr_generics::prelude::*;
use sexpr_generics::with_sexpr_matcher;
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
                .or_insert_with(|| Binding::Repeated(vec![]))
                .attach(value)
        }
    }

    pub fn filter_needed(&self, template: &Scm) -> Self {
        let needed_identifiers = self.bound_in(template);
        MatchBindings {
            bindings: self
                .bindings
                .iter()
                .filter(|&(k, _)| needed_identifiers.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
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

    fn bound_in(&self, template: &Scm) -> HashSet<PointerKey<Scm>> {
        with_sexpr_matcher! {
            match template, {
                {s: Symbol} => {
                    if self.bindings.contains_key(PointerKey::from_ref(s)) {
                        hashset![s.clone().into()]
                    } else {
                        hashset![]
                    }
                }
                (left . right) => {
                    let mut set = self.bound_in(left);
                    set.extend(self.bound_in(right));
                    set
                }
                _ => { hashset![] },
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use sexpr_generics::sexpr;

    #[test]
    fn symbol_bound_in_template() {
        let bindings = MatchBindings::new(sexpr![x], sexpr![0]);
        let idents = bindings.bound_in(&sexpr![x]);
        assert_eq!(idents, hashset![sexpr![x].into()]);
    }

    #[test]
    fn symbol_not_bound_in_template() {
        let bindings = MatchBindings::new(sexpr![x], sexpr![0]);
        let idents = bindings.bound_in(&sexpr![y]);
        assert_eq!(idents, hashset![]);
    }

    #[test]
    fn symbols_bound_in_list_template() {
        let bindings = MatchBindings::join(
            MatchBindings::new(sexpr![x], sexpr![0]),
            MatchBindings::new(sexpr![y], sexpr![0]),
        );
        let idents = bindings.bound_in(&sexpr![(z x z y z)]);
        assert_eq!(idents, hashset![sexpr![x].into(), sexpr![y].into()]);
    }

    #[test]
    fn filter_needed_one_symbol() {
        let bindings = MatchBindings::join(
            MatchBindings::new(sexpr![x], sexpr![0]),
            MatchBindings::new(sexpr![y], sexpr![0]),
        );

        let needed = bindings.filter_needed(&sexpr![x]);

        let expected = MatchBindings::new(sexpr![x], sexpr![0]);
        assert_eq!(needed, expected);
    }

    #[test]
    fn filter_needed_both_symbols() {
        let bindings = MatchBindings::join(
            MatchBindings::new(sexpr![x], sexpr![0]),
            MatchBindings::new(sexpr![y], sexpr![0]),
        );

        let needed = bindings.filter_needed(&sexpr![(x y)]);

        assert_eq!(needed, bindings);
    }

    #[test]
    fn filter_needed_extra_symbol_ignored() {
        let bindings = MatchBindings::join(
            MatchBindings::new(sexpr![x], sexpr![0]),
            MatchBindings::new(sexpr![y], sexpr![0]),
        );

        let needed = bindings.filter_needed(&sexpr![(x z)]);

        let expected = MatchBindings::new(sexpr![x], sexpr![0]);
        assert_eq!(needed, expected);
    }
}
