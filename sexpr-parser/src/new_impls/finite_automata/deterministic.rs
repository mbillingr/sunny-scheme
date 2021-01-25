use super::nondeterministic::Nfa;
use maplit::hashset;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StateId(pub(super) usize);

pub type Dfa = FiniteAutomaton<(), u8>;

pub struct FiniteAutomaton<S, T> {
    pub(super) states: Vec<S>,
    pub(super) accepting_states: HashSet<StateId>,
    pub(super) transitions: HashMap<StateId, HashMap<T, StateId>>,
    pub(super) starting_state: StateId,
}

impl<S, T: Eq + Hash> FiniteAutomaton<S, T> {
    pub fn new() -> Self {
        FiniteAutomaton {
            states: vec![],
            accepting_states: HashSet::new(),
            transitions: HashMap::new(),
            starting_state: StateId(0),
        }
    }

    pub fn starting_state(&self) -> StateId {
        self.starting_state
    }

    pub fn add_state(&mut self, s: S) -> StateId {
        let id = self.states.len();
        self.states.push(s);
        StateId(id)
    }

    pub fn add_transition(&mut self, s1: StateId, s2: StateId, t: T) {
        self.transitions
            .entry(s1)
            .or_insert(HashMap::new())
            .insert(t, s2);
    }

    pub fn set_starting_state(&mut self, s: StateId) {
        self.starting_state = s;
    }

    pub fn add_accepting_state(&mut self, sid: StateId) {
        self.accepting_states.insert(sid);
    }

    pub fn clear_accepting_states(&mut self) {
        self.accepting_states.clear();
    }

    pub fn get_transition(&self, s: StateId, t: &T) -> Option<StateId> {
        self.transitions.get(&s).and_then(|trs| trs.get(t)).copied()
    }

    pub fn transitions_from(&self, s: StateId) -> impl Iterator<Item = (StateId, &T)> {
        self.transitions
            .get(&s)
            .into_iter()
            .flatten()
            .map(|(t, s_next)| (*s_next, t))
    }
}

impl Dfa {
    pub fn from_nfa(nfa: Nfa) -> Self {
        let n0 = nfa.starting_state();
        let q0 = nfa.epsilon_closure(hashset![n0]);
        let mut subsets = vec![q0];
        let mut subset_transitions = HashMap::new();
        let mut worklist = vec![0];
        while let Some(q) = worklist.pop() {
            let chars_leaving_q: HashSet<u8> = nfa
                .delta(subsets[q].iter().copied())
                .map(|(_, t)| *t)
                .collect();
            for c in chars_leaving_q {
                let t = nfa.delta_epsilon_closure(&c, &subsets[q]);
                if let Some(i) = subsets.iter().position(|qs| qs == &t) {
                    subset_transitions.insert((q, c), i);
                } else {
                    subset_transitions.insert((q, c), subsets.len());
                    worklist.push(subsets.len());
                    subsets.push(t);
                }
            }
        }

        let mut dfa = Dfa::new();
        for q in subsets {
            let s = dfa.add_state(());
            for n in q {
                if n == nfa.accepting_state() {
                    dfa.add_accepting_state(s);
                }
            }
        }

        for ((s1, ch), s2) in subset_transitions {
            dfa.add_transition(StateId(s1), StateId(s2), ch)
        }

        dfa
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trivial_nfa_to_dfa() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        nfa.add_transition(s0, s1, b'a');
        nfa.set_accepting_state(s1);

        let dfa = Dfa::from_nfa(nfa);

        assert_eq!(dfa.states.len(), 2);
        assert!(dfa.accepting_states.contains(&StateId(1)));
        assert_eq!(dfa.transitions[&StateId(0)][&b'a'], StateId(1));
    }

    #[test]
    fn dfa_can_have_multiple_accepting_states() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        let s2 = nfa.add_state(());
        let s3 = nfa.add_state(());
        let s4 = nfa.add_state(());
        let s5 = nfa.add_state(());
        nfa.set_accepting_state(s5);
        nfa.add_epsilon_transition(s0, s1);
        nfa.add_epsilon_transition(s0, s3);
        nfa.add_transition(s1, s2, b'a');
        nfa.add_transition(s3, s4, b'b');
        nfa.add_epsilon_transition(s2, s5);
        nfa.add_epsilon_transition(s4, s5);

        let dfa = Dfa::from_nfa(nfa);

        assert_eq!(dfa.states.len(), 3);
        assert!(dfa.accepting_states.contains(&StateId(1)));
        assert!(dfa.accepting_states.contains(&StateId(2)));
        assert_eq!(dfa.accepting_states.len(), 2);
        match dfa.transitions[&StateId(0)][&b'a'] {
            StateId(1) => assert_eq!(dfa.transitions[&StateId(0)][&b'b'], StateId(2)),
            StateId(2) => assert_eq!(dfa.transitions[&StateId(0)][&b'b'], StateId(1)),
            _ => panic!("Unexpected transition"),
        }
    }

    #[test]
    fn dfa_from_nfa_complex_example() {
        // EaC book, figure 2.7
        let mut nfa = Nfa::new();
        let n0 = nfa.add_state(());
        let n1 = nfa.add_state(());
        let n2 = nfa.add_state(());
        let n3 = nfa.add_state(());
        let n4 = nfa.add_state(());
        let n5 = nfa.add_state(());
        let n6 = nfa.add_state(());
        let n7 = nfa.add_state(());
        let n8 = nfa.add_state(());
        let n9 = nfa.add_state(());
        nfa.set_accepting_state(n9);
        nfa.add_transition(n0, n1, b'a');
        nfa.add_epsilon_transition(n1, n2);
        nfa.add_epsilon_transition(n2, n3);
        nfa.add_epsilon_transition(n2, n9);
        nfa.add_epsilon_transition(n3, n4);
        nfa.add_epsilon_transition(n3, n6);
        nfa.add_transition(n4, n5, b'b');
        nfa.add_epsilon_transition(n5, n8);
        nfa.add_transition(n6, n7, b'c');
        nfa.add_epsilon_transition(n7, n8);
        nfa.add_epsilon_transition(n8, n9);
        nfa.add_epsilon_transition(n8, n3);

        let dfa = Dfa::from_nfa(nfa);

        assert_eq!(dfa.states.len(), 4);
        assert!(dfa.accepting_states.contains(&StateId(1)));
        assert!(dfa.accepting_states.contains(&StateId(2)));
        assert!(dfa.accepting_states.contains(&StateId(3)));
        assert_eq!(dfa.accepting_states.len(), 3);

        assert_eq!(dfa.transitions[&StateId(0)][&b'a'], StateId(1));
        match dfa.transitions[&StateId(1)][&b'c'] {
            StateId(2) => {
                assert_eq!(dfa.transitions[&StateId(1)][&b'b'], StateId(3));
                assert_eq!(dfa.transitions[&StateId(2)][&b'b'], StateId(3));
                assert_eq!(dfa.transitions[&StateId(2)][&b'c'], StateId(2));
                assert_eq!(dfa.transitions[&StateId(3)][&b'b'], StateId(3));
                assert_eq!(dfa.transitions[&StateId(3)][&b'c'], StateId(2));
            }
            StateId(3) => {
                assert_eq!(dfa.transitions[&StateId(1)][&b'b'], StateId(2));
                assert_eq!(dfa.transitions[&StateId(2)][&b'b'], StateId(2));
                assert_eq!(dfa.transitions[&StateId(2)][&b'c'], StateId(3));
                assert_eq!(dfa.transitions[&StateId(3)][&b'b'], StateId(2));
                assert_eq!(dfa.transitions[&StateId(3)][&b'c'], StateId(3));
            }
            _ => panic!("unexpected transition"),
        }
    }
}
