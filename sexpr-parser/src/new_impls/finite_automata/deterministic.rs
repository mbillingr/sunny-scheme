use super::nondeterministic::Nfa;
use super::StateId;
use maplit::hashset;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub type Dfa = FiniteAutomaton<(), u8>;

pub struct FiniteAutomaton<S, T> {
    pub(super) states: Vec<S>,
    pub(super) accepting_states: HashSet<StateId>,
    pub(super) transitions: HashMap<StateId, HashMap<T, StateId>>,
}

impl<S, T: Eq + Hash> FiniteAutomaton<S, T> {
    pub fn new() -> Self {
        FiniteAutomaton {
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
            .or_insert(HashMap::new())
            .insert(t, s2);
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
        let Q = vec![q0.clone()];
        let mut worklist = vec![q0];
        while !worklist.is_empty() {
            let q = worklist.pop().unwrap();
            let all_transitions_from_q = nfa.delta(q.iter().copied());
            let chars_leaving_q: HashSet<u8> =
                all_transitions_from_q.iter().map(|(_, _, t)| **t).collect();
            for c in chars_leaving_q {
                let dq = all_transitions_from_q
                    .iter()
                    .filter(|(_, _, t)| **t == c)
                    .map(|(_, s2, _)| *s2)
                    .collect();
                let t = nfa.epsilon_closure(dq);
                //T[q, c]
                unimplemented!()
            }
        }
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //#[test]
    fn trivial_nfa_to_dfa() {
        let mut nfa = Nfa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        nfa.add_transition(s0, s1, b'x');
        nfa.add_accepting_state(s1);

        let dfa = Dfa::from_nfa(nfa);

        assert_eq!(dfa.states.len(), 2);
        assert!(dfa.accepting_states.contains(&StateId(1)));
        assert_eq!(dfa.transitions[&StateId(0)][&b'x'], StateId(1));
    }
}
