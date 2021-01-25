use super::super::simple_regex::Regex;
use maplit::hashset;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StateId(pub(super) usize);

pub type Nfa = NondeterministicFiniteAutomaton<(), u8>;

pub struct NondeterministicFiniteAutomaton<S, T> {
    pub(super) states: Vec<S>,
    pub(super) accepting_states: HashSet<StateId>,
    pub(super) transitions: HashMap<StateId, (HashMap<T, StateId>, HashSet<StateId>)>,
}

impl<S, T: Eq + Hash> NondeterministicFiniteAutomaton<S, T> {
    pub fn new() -> Self {
        NondeterministicFiniteAutomaton {
            states: vec![],
            accepting_states: HashSet::new(),
            transitions: HashMap::new(),
        }
    }

    pub fn starting_state(&self) -> StateId {
        // for now, assume state 0 is always the starting state
        StateId(0)
    }

    pub fn add_state(&mut self, s: S) -> StateId {
        let id = self.states.len();
        self.states.push(s);
        StateId(id)
    }

    pub fn add_transition(&mut self, s1: StateId, s2: StateId, t: T) {
        self.transitions
            .entry(s1)
            .or_insert((HashMap::new(), HashSet::new()))
            .0
            .insert(t, s2);
    }

    pub fn add_epsilon_transition(&mut self, s1: StateId, s2: StateId) {
        self.transitions
            .entry(s1)
            .or_insert((HashMap::new(), HashSet::new()))
            .1
            .insert(s2);
    }

    pub fn add_accepting_state(&mut self, sid: StateId) {
        self.accepting_states.insert(sid);
    }

    pub fn clear_accepting_states(&mut self) {
        self.accepting_states.clear();
    }

    pub fn get_transition(&self, s: StateId, t: &T) -> Option<StateId> {
        self.transitions
            .get(&s)
            .and_then(|(non_eps, _)| non_eps.get(t))
            .copied()
    }

    pub fn count_transitions(&self) -> usize {
        self.transitions
            .iter()
            .flat_map(|(_, (non_eps, _))| non_eps)
            .count()
    }

    pub fn count_epsilon_transitions(&self) -> usize {
        self.transitions
            .iter()
            .flat_map(|(_, (_, eps))| eps)
            .count()
    }

    pub fn transitions_from(&self, s: StateId) -> impl Iterator<Item = (StateId, &T)> {
        self.transitions
            .get(&s)
            .into_iter()
            .map(|(non_eps, _)| non_eps)
            .flatten()
            .map(|(t, s_next)| (*s_next, t))
    }

    pub fn epsilon_transitions_from(&self, s: StateId) -> impl Iterator<Item = StateId> + '_ {
        self.transitions
            .get(&s)
            .into_iter()
            .map(|(_, eps)| eps)
            .flatten()
            .copied()
    }

    pub fn delta(&self, states: impl Iterator<Item = StateId>) -> Vec<(StateId, StateId, &T)> {
        states
            .flat_map(|s0| self.transitions_from(s0).map(move |(s1, t)| (s0, s1, t)))
            .collect()
    }
}

impl Nfa {
    pub fn from_regex(re: &Regex) -> Self {
        match re {
            Regex::Empty => {
                let mut fa = Self::new();
                let s = fa.add_state(());
                fa.add_accepting_state(s);
                fa
            }
            Regex::Character(ch) => {
                let mut fa = Self::new();
                let s0 = fa.add_state(());
                let s1 = fa.add_state(());
                fa.add_accepting_state(s1);
                fa.add_transition(s0, s1, *ch);
                fa
            }
            Regex::Concatenation(a, b) => {
                let nfa1 = Self::from_regex(a);
                let nfa2 = Self::from_regex(b);
                nfa1.concatenate(nfa2)
            }
            Regex::Alternation(a, b) => {
                let nfa1 = Self::from_regex(a);
                let nfa2 = Self::from_regex(b);
                nfa1.alternate(nfa2)
            }
            Regex::Closure(a) => {
                let nfa = Self::from_regex(a);
                nfa.repeat()
            }
        }
    }

    fn concatenate(mut self, other: Self) -> Self {
        let (other_entry, other_exit) = self.merge(other);

        let old_accepting_state = self.accepting_state();

        self.clear_accepting_states();
        self.add_accepting_state(other_exit);

        self.add_epsilon_transition(old_accepting_state, other_entry);

        self
    }

    fn alternate(self, other: Self) -> Self {
        let mut new = Self::new();

        let entry = new.add_state(());

        let (entry1, exit1) = new.merge(self);
        let (entry2, exit2) = new.merge(other);

        let exit = new.add_state(());
        new.add_accepting_state(exit);

        new.add_epsilon_transition(exit1, exit);
        new.add_epsilon_transition(exit2, exit);

        new.add_epsilon_transition(entry, entry1);
        new.add_epsilon_transition(entry, entry2);

        new
    }

    fn repeat(self) -> Self {
        let mut new = Self::new();

        let entry = new.add_state(());

        let (old_entry, old_exit) = new.merge(self);

        let exit = new.add_state(());
        new.add_accepting_state(exit);

        new.add_epsilon_transition(entry, old_entry);
        new.add_epsilon_transition(old_exit, exit);
        new.add_epsilon_transition(old_exit, old_entry);
        new.add_epsilon_transition(entry, exit);

        new
    }

    fn merge(&mut self, other: Self) -> (StateId, StateId) {
        let id_offset = self.states.len();

        let id_exit = StateId(other.accepting_state().0 + id_offset);

        self.states.extend(other.states);

        for (s1, trs) in other.transitions {
            let s1 = StateId(s1.0 + id_offset);
            for (t, s2) in trs.0 {
                let s2 = StateId(s2.0 + id_offset);
                self.add_transition(s1, s2, t);
            }
            for s2 in trs.1 {
                let s2 = StateId(s2.0 + id_offset);
                self.add_epsilon_transition(s1, s2);
            }
        }

        let id_entry = StateId(id_offset);
        (id_entry, id_exit)
    }

    pub fn accepting_state(&self) -> StateId {
        self.accepting_states.iter().next().copied().unwrap()
    }

    pub fn epsilon_closure(&self, mut states: HashSet<StateId>) -> HashSet<StateId> {
        let mut result = hashset! {};

        while let Some(s) = set_pop(&mut states) {
            if result.contains(&s) {
                continue;
            }
            result.insert(s);

            for s_next in self.epsilon_transitions_from(s) {
                states.insert(s_next);
            }
        }

        result
    }
}

fn set_pop<T: Clone + Eq + std::hash::Hash>(set: &mut HashSet<T>) -> Option<T> {
    let item = set.iter().next()?.clone();
    set.take(&item)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_transitions {
        ($fa:expr, { $($body:tt)* }) => { assert_transitions!(@ $fa, { $($body)* }, 0, 0) };

        (@ $fa:expr, { }, $n:expr, $e:expr) => {{
            std::assert_eq!($fa.count_transitions(), $n, "wrong number of transitions");
            std::assert_eq!($fa.count_epsilon_transitions(), $e, "wrong number of epsilon_transitions");
        }};

        (@ $fa:expr, { ($s0:expr, ) => $s1:expr; $($rest:tt)* }, $n:expr, $e:expr) => {{
            std::assert!($fa.transitions[&StateId($s0)].1.contains(&StateId($s1)));
            assert_transitions!(@ $fa, { $($rest)* }, $n, $e + 1)
        }};

        (@ $fa:expr, { ($s0:expr, $ch:ident) => $s1:expr; $($rest:tt)* }, $n:expr, $e:expr) => {{
            assert_transitions!(@ $fa, { ($s0, stringify!($ch).as_bytes()[0]) => $s1; $($rest)* }, $n, $e)
        }};

        (@ $fa:expr, { ($s0:expr, $ch:expr) => $s1:expr; $($rest:tt)* }, $n:expr, $e:expr) => {{
            let s_actual = $fa.get_transition(StateId($s0), &$ch);
            std::assert_eq!(s_actual, Some(StateId($s1)), "Expected ({}, ) => {} but got {:?}", $s0, $s1, s_actual);
            assert_transitions!(@ $fa, { $($rest)* }, $n + 1, $e)
        }};
    }

    #[test]
    fn concatenate_nfas() {
        let nfa1 = Nfa::from_regex(&Regex::new("x"));
        let nfa2 = Nfa::from_regex(&Regex::new("y"));

        let nfa = nfa1.concatenate(nfa2);

        assert_eq!(nfa.states, vec![(), (), (), ()]);

        assert!(nfa.accepting_states.contains(&StateId(3)));
        assert_eq!(nfa.accepting_states.len(), 1);

        assert_transitions!(
            nfa, {
                (0, b'x') => 1;
                (1, ) => 2;
                (2, y) => 3;
            }
        );

        assert_eq!(nfa.transitions[&StateId(0)].0[&b'x'], StateId(1));
        assert_eq!(nfa.transitions[&StateId(1)].1, hashset! {StateId(2)});
        assert_eq!(nfa.transitions[&StateId(2)].0[&b'y'], StateId(3));
        assert_eq!(nfa.transitions.len(), 3);
    }

    #[test]
    fn alternate_nfas() {
        let nfa1 = Nfa::from_regex(&Regex::new("x"));
        let nfa2 = Nfa::from_regex(&Regex::new("y"));

        let nfa = nfa1.alternate(nfa2);

        assert_eq!(nfa.states, vec![(), (), (), (), (), ()]);

        assert!(nfa.accepting_states.contains(&StateId(5)));
        assert_eq!(nfa.accepting_states.len(), 1);

        assert_transitions!(
            nfa, {
                (0, ) => 1;
                (0, ) => 3;
                (1, x) => 2;
                (3, y) => 4;
                (2, ) => 5;
                (4, ) => 5;
            }
        );
    }

    #[test]
    fn repeat_nfa() {
        let nfa = Nfa::from_regex(&Regex::new("x"));

        let nfa = nfa.repeat();

        assert_eq!(nfa.states, vec![(), (), (), ()]);

        assert!(nfa.accepting_states.contains(&StateId(3)));
        assert_eq!(nfa.accepting_states.len(), 1);

        assert_transitions!(
            nfa, {
                (0, ) => 1;
                (0, ) => 3;
                (1, x) => 2;
                (2, ) => 3;
                (2, ) => 1;
            }
        );
    }

    #[test]
    fn nfa_from_empty_regex() {
        let nfa = Nfa::from_regex(&Regex::new(""));
        assert_eq!(nfa.states, vec![()]);
        assert!(nfa.accepting_states.contains(&StateId(0)));
        assert_transitions!(nfa, {});
    }

    #[test]
    fn nfa_from_single_char_regex() {
        let nfa = Nfa::from_regex(&Regex::new("x"));
        assert_eq!(nfa.states, vec![(), ()]);
        assert!(nfa.accepting_states.contains(&StateId(1)));
        assert_transitions!(
            nfa, {
                (0, x) => 1;
            }
        );
    }

    #[test]
    fn nfa_from_concatenated_regex() {
        let nfa = Nfa::from_regex(&Regex::new("xyz"));
        assert_eq!(nfa.states, vec![(), (), (), (), (), ()]);
        assert!(nfa.accepting_states.contains(&StateId(5)));
        assert_transitions!(
            nfa, {
                (0, x) => 1;
                (1, ) => 2;
                (2, y) => 3;
                (3, ) => 4;
                (4, z) => 5;
            }
        );
    }

    #[test]
    fn nfa_from_alternating_regex() {
        let nfa = Nfa::from_regex(&Regex::new("(x|y)|z"));
        assert_eq!(nfa.states, vec![(); 10]);
        assert!(nfa.accepting_states.contains(&StateId(9)));
        assert_transitions!(
            nfa, {
                (0, ) => 1;
                (0, ) => 7;
                (1, ) => 2;
                (1, ) => 4;
                (2, x) => 3;
                (3, ) => 6;
                (4, y) => 5;
                (5, ) => 6;
                (6, ) => 9;
                (7, z) => 8;
                (8, ) => 9;
            }
        );

        /*assert_eq!(nfa.transitions[&(StateId(0), StateId(1))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(1), StateId(2))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(1), StateId(4))], vec![Epsilon]);
        assert_eq!(
            nfa.transitions[&(StateId(2), StateId(3))],
            vec![Character(b'x')]
        );
        assert_eq!(
            nfa.transitions[&(StateId(4), StateId(5))],
            vec![Character(b'y')]
        );
        assert_eq!(nfa.transitions[&(StateId(3), StateId(6))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(6), StateId(9))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(0), StateId(7))], vec![Epsilon]);
        assert_eq!(
            nfa.transitions[&(StateId(7), StateId(8))],
            vec![Character(b'z')]
        );
        assert_eq!(nfa.transitions[&(StateId(8), StateId(9))], vec![Epsilon]);*/
    }

    #[test]
    fn nfa_from_repeating_regex() {
        let nfa = Nfa::from_regex(&Regex::new("x*"));
        assert_eq!(nfa.states, vec![(), (), (), ()]);
        assert!(nfa.accepting_states.contains(&StateId(3)));

        assert_transitions!(
            nfa, {
                (0, ) => 1;
                (0, ) => 3;
                (1, x) => 2;
                (2, ) => 1;
                (2, ) => 3;
            }
        );
    }

    #[test]
    fn epsilon_closure_of_empty_set_is_empty_set() {
        assert_eq!(Nfa::new().epsilon_closure(hashset! {}), hashset! {});
    }

    #[test]
    fn epsilon_closure_does_not_include_nodes_not_reachable_by_epsilon() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        let s2 = nfa.add_state(());
        nfa.add_state(());
        nfa.add_transition(s0, s1, b'x');
        nfa.add_epsilon_transition(s1, s2);
        assert_eq!(nfa.epsilon_closure(hashset! {s0}), hashset! {s0});
    }

    #[test]
    fn epsilon_closure_includes_node_reachable_by_epsilon() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        nfa.add_epsilon_transition(s0, s1);
        assert_eq!(nfa.epsilon_closure(hashset! {s0}), hashset! { s0, s1 });
    }

    #[test]
    fn epsilon_closure_includes_all_nodes_reachable_by_epsilon() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        let s2 = nfa.add_state(());
        let s3 = nfa.add_state(());
        nfa.add_epsilon_transition(s0, s1);
        nfa.add_epsilon_transition(s0, s2);
        nfa.add_epsilon_transition(s2, s3);
        assert_eq!(
            nfa.epsilon_closure(hashset! {s0}),
            hashset! {s0, s1, s2, s3}
        );
    }
}
