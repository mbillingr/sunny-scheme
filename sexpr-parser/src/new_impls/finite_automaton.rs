use std::collections::{HashMap, HashSet};
use NondeterministicTransition::*;
use crate::new_impls::simple_regex::Regex;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StateId(usize);

pub type Nfa = FiniteAutomaton<(), NondeterministicTransition<u8>>;

pub struct FiniteAutomaton<S, T> {
    states: Vec<S>,
    accepting_states: HashSet<StateId>,
    edges: HashMap<StateId, Vec<StateId>>,
    transitions: HashMap<(StateId, StateId), Vec<T>>,
}

impl<S, T> FiniteAutomaton<S, T> {
    pub fn new() -> Self {
        FiniteAutomaton {
            states: vec![],
            accepting_states: HashSet::new(),
            edges: HashMap::new(),
            transitions: HashMap::new(),
        }
    }

    pub fn add_state(&mut self, s: S) -> StateId {
        let id = self.states.len();
        self.states.push(s);
        StateId(id)
    }

    pub fn add_transition(&mut self, s1: StateId, s2: StateId, t: T) {
        self.edges.entry(s1).or_insert(Vec::new()).push(s2);
        self.transitions.entry((s1, s2)).or_insert(Vec::new()).push(t);
    }

    pub fn set_accepting_state(&mut self, sid: StateId) {
        self.accepting_states.insert(sid);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NondeterministicTransition<T> {
    Epsilon,
    Character(T),
}

impl FiniteAutomaton<(), NondeterministicTransition<u8>> {
    fn from_regex(re: &Regex) -> Self {
        let mut fa = FiniteAutomaton::new();

        match re {
            Regex::Empty => {
                let s = fa.add_state(());
                fa.set_accepting_state(s);
            }
            Regex::Character(ch) => {
                let s0 = fa.add_state(());
                let s1 = fa.add_state(());
                fa.set_accepting_state(s1);
                fa.add_transition(s0, s1, Character(*ch));
            }
            _ => unimplemented!()
        }

        fa
    }

    fn concatenate(mut self, other: Self) -> Self {
        let (other_entry, other_exit) = self.merge(other);

        let old_accepting_state = self.accepting_state();

        self.accepting_states.clear();
        self.accepting_states.insert(other_exit);

        self.add_transition(old_accepting_state, other_entry, Epsilon);

        self
    }

    fn alternate(self, other: Self) -> Self {
        let mut new = Self::new();

        let entry = new.add_state(());

        let (entry1, exit1) = new.merge(self);
        let (entry2, exit2) = new.merge(other);

        let exit = new.add_state(());
        new.set_accepting_state(exit);

        new.add_transition(exit1, exit, Epsilon);
        new.add_transition(exit2, exit, Epsilon);

        new.add_transition(entry, entry1, Epsilon);
        new.add_transition(entry, entry2, Epsilon);

        new
    }

    fn repeat(self) -> Self {
        let mut new = Self::new();

        let entry = new.add_state(());

        let (old_entry, old_exit) = new.merge(self);

        let exit = new.add_state(());
        new.set_accepting_state(exit);

        new.add_transition(entry, old_entry, Epsilon);
        new.add_transition(old_exit, exit, Epsilon);
        new.add_transition(old_exit, old_entry, Epsilon);
        new.add_transition(entry, exit, Epsilon);

        new
    }

    fn merge(&mut self, other: Self) -> (StateId, StateId) {
        let id_offset = self.states.len();

        let id_exit = StateId(other.accepting_state().0 + id_offset);

        self.states.extend(other.states);

        for ((s1, s2), trs) in other.transitions {
            let s1 = StateId(s1.0 + id_offset);
            let s2 = StateId(s2.0 + id_offset);
            for t in trs {
                self.add_transition(s1, s2, t);
            }
        }

        let id_entry = StateId(id_offset);
        (id_entry, id_exit)
    }

    fn accepting_state(&self) -> StateId {
        self.accepting_states.iter().next().copied().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nfa_from_empty_regex() {
        let nfa = Nfa::from_regex(&Regex::new(""));
        assert_eq!(nfa.states, vec![()]);
        assert!(nfa.accepting_states.contains(&StateId(0)));
        assert!(nfa.edges.is_empty());
        assert!(nfa.transitions.is_empty());
    }

    #[test]
    fn nfa_from_single_char_regex() {
        let nfa = Nfa::from_regex(&Regex::new("x"));
        assert_eq!(nfa.states, vec![(), ()]);
        assert!(nfa.accepting_states.contains(&StateId(1)));
        assert_eq!(nfa.edges[&StateId(0)], vec![StateId(1)]);
        assert_eq!(nfa.transitions[&(StateId(0), StateId(1))], vec![Character(b'x')]);
    }

    #[test]
    fn concatenate_nfas() {
        let nfa1 = Nfa::from_regex(&Regex::new("x"));
        let nfa2 = Nfa::from_regex(&Regex::new("y"));

        let nfa = nfa1.concatenate(nfa2);

        assert_eq!(nfa.states, vec![(), (), (), ()]);

        assert!(nfa.accepting_states.contains(&StateId(3)));
        assert_eq!(nfa.accepting_states.len(), 1);

        assert_eq!(nfa.edges[&StateId(0)], vec![StateId(1)]);
        assert_eq!(nfa.edges[&StateId(1)], vec![StateId(2)]);
        assert_eq!(nfa.edges[&StateId(2)], vec![StateId(3)]);
        assert_eq!(nfa.edges.len(), 3);

        assert_eq!(nfa.transitions[&(StateId(0), StateId(1))], vec![Character(b'x')]);
        assert_eq!(nfa.transitions[&(StateId(1), StateId(2))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(2), StateId(3))], vec![Character(b'y')]);
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

        assert_eq!(nfa.edges[&StateId(0)], vec![StateId(1), StateId(3)]);
        assert_eq!(nfa.edges[&StateId(1)], vec![StateId(2)]);
        assert_eq!(nfa.edges[&StateId(2)], vec![StateId(5)]);
        assert_eq!(nfa.edges[&StateId(3)], vec![StateId(4)]);
        assert_eq!(nfa.edges[&StateId(4)], vec![StateId(5)]);
        assert_eq!(nfa.edges.len(), 5);

        assert_eq!(nfa.transitions[&(StateId(0), StateId(1))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(0), StateId(3))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(1), StateId(2))], vec![Character(b'x')]);
        assert_eq!(nfa.transitions[&(StateId(3), StateId(4))], vec![Character(b'y')]);
        assert_eq!(nfa.transitions[&(StateId(2), StateId(5))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(4), StateId(5))], vec![Epsilon]);
        assert_eq!(nfa.transitions.len(), 6);
    }

    #[test]
    fn repeat_nfa() {
        let nfa = Nfa::from_regex(&Regex::new("x"));

        let nfa = nfa.repeat();

        assert_eq!(nfa.states, vec![(), (), (), ()]);

        assert!(nfa.accepting_states.contains(&StateId(3)));
        assert_eq!(nfa.accepting_states.len(), 1);

        assert_eq!(nfa.edges[&StateId(0)], vec![StateId(1), StateId(3)]);
        assert_eq!(nfa.edges[&StateId(1)], vec![StateId(2)]);
        assert_eq!(nfa.edges[&StateId(2)], vec![StateId(3), StateId(1)]);
        assert_eq!(nfa.edges.len(), 3);

        assert_eq!(nfa.transitions[&(StateId(0), StateId(1))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(0), StateId(3))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(1), StateId(2))], vec![Character(b'x')]);
        assert_eq!(nfa.transitions[&(StateId(2), StateId(3))], vec![Epsilon]);
        assert_eq!(nfa.transitions[&(StateId(2), StateId(1))], vec![Epsilon]);
        assert_eq!(nfa.transitions.len(), 5);
    }
}