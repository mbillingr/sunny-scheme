use super::simple_regex::Regex;
use maplit::btreeset;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct StateId(pub(super) usize);

pub type Fa = FiniteAutomaton<(), u8>;

#[derive(Debug, Clone, PartialEq)]
pub struct FiniteAutomaton<S, T: Eq + Ord> {
    pub(super) states: Vec<S>,
    pub(super) starting_state: StateId,
    pub(super) accepting_states: BTreeSet<StateId>,
    pub(super) transitions: BTreeMap<StateId, (BTreeSet<(StateId, T)>, BTreeSet<StateId>)>,
}

impl<S: Default, T: Clone + Eq + Ord> FiniteAutomaton<S, T> {
    pub fn new() -> Self {
        FiniteAutomaton {
            states: vec![],
            starting_state: StateId(0),
            accepting_states: BTreeSet::new(),
            transitions: BTreeMap::new(),
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
            .or_insert((BTreeSet::new(), BTreeSet::new()))
            .0
            .insert((s2, t));
    }

    pub fn add_epsilon_transition(&mut self, s1: StateId, s2: StateId) {
        self.transitions
            .entry(s1)
            .or_insert((BTreeSet::new(), BTreeSet::new()))
            .1
            .insert(s2);
    }

    pub fn get_state_content(&self, s: StateId) -> &S {
        &self.states[s.0]
    }

    pub fn set_starting_state(&mut self, sid: StateId) {
        self.starting_state = sid;
    }

    pub fn set_accepting_state(&mut self, sid: StateId) {
        self.accepting_states = btreeset![sid]
    }

    pub fn add_accepting_state(&mut self, sid: StateId) {
        self.accepting_states.insert(sid);
    }

    pub fn is_accepting_state(&self, sid: StateId) -> bool {
        self.accepting_states.contains(&sid)
    }

    pub fn get_single_accepting_state(&self) -> Option<StateId> {
        if self.accepting_states.len() != 1 {
            return None;
        }
        self.accepting_states.iter().next().copied()
    }

    pub fn get_single_transition(&self, s: StateId, t: &T) -> Option<StateId> {
        let mut trs = self.transitions_from_on(s, t);
        let first = trs.next();
        let second = trs.next();
        if second.is_some() {
            None
        } else {
            first
        }
    }

    pub fn transitions_from_on<'a>(
        &'a self,
        s: StateId,
        t: &'a T,
    ) -> impl Iterator<Item = StateId> + 'a {
        self.transitions
            .get(&s)
            .map(|(non_eps, _)| non_eps)
            .into_iter()
            .flatten()
            .filter(move |(_, tr)| tr == t)
            .map(|&(s1, _)| s1)
    }

    pub fn transitions_from(&self, s: StateId) -> impl Iterator<Item = (StateId, &T)> {
        self.transitions
            .get(&s)
            .map(|(non_eps, _)| non_eps)
            .into_iter()
            .flat_map(|trs| trs.iter().map(|(s1, t)| (*s1, t)))
    }

    pub fn all_transitions(&self) -> impl Iterator<Item = (StateId, StateId, &T)> {
        self.transitions
            .iter()
            .flat_map(|(s0, (trs, _))| trs.iter().map(move |(s1, t)| (*s0, *s1, t)))
    }

    pub fn count_transitions(&self) -> usize {
        self.transitions.len()
    }

    pub fn count_epsilon_transitions(&self) -> usize {
        self.transitions.len()
    }

    fn epsilon_transitions_from(&self, s: StateId) -> impl Iterator<Item = StateId> + '_ {
        self.transitions
            .get(&s)
            .map(|(_, eps)| eps)
            .into_iter()
            .flatten()
            .copied()
    }

    fn delta<'a>(
        &self,
        states: impl IntoIterator<Item = &'a StateId>,
    ) -> impl Iterator<Item = (StateId, &T)> {
        states
            .into_iter()
            .copied()
            .flat_map(move |s0| self.transitions_from(s0).map(move |(s1, t)| (s1, t)))
    }

    fn epsilon_closure(&self, mut states: BTreeSet<StateId>) -> BTreeSet<StateId> {
        let mut result = btreeset! {};

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

    fn delta_epsilon_closure<'a>(
        &self,
        t: &T,
        subset: impl IntoIterator<Item = &'a StateId>,
    ) -> BTreeSet<StateId> {
        let dq = subset
            .into_iter()
            .flat_map(move |&s| self.transitions_from_on(s, t))
            .collect();
        self.epsilon_closure(dq)
    }

    pub fn reverse(self) -> Self {
        let mut reversed = Self::new();
        reversed.set_accepting_state(self.starting_state());

        match self.accepting_states.len() {
            0 => return reversed,
            1 => {
                reversed.set_starting_state(self.accepting_states.into_iter().next().unwrap());
                reversed.states = self.states;
            }
            _ => {
                reversed.states = self.states;
                let start = reversed.add_state(S::default());
                reversed.set_starting_state(start);
                for s in self.accepting_states {
                    reversed.add_epsilon_transition(start, s);
                }
            }
        }

        for (s0, (non_eps, eps)) in self.transitions {
            for (s1, t) in non_eps {
                reversed.add_transition(s1, s0, t);
            }
            for s1 in eps {
                reversed.add_epsilon_transition(s1, s0);
            }
        }

        reversed
    }

    pub fn reachable(mut self) -> Self {
        let reachable = self.find_reachable_states();

        let mut out = Self::new();

        let mut mapping = BTreeMap::new();
        for (s, content) in std::mem::replace(&mut self.states, vec![])
            .into_iter()
            .enumerate()
        {
            let old_state = StateId(s);
            if reachable.contains(&old_state) {
                let new_state = out.add_state(content);

                if self.is_accepting_state(old_state) {
                    out.add_accepting_state(new_state);
                }

                mapping.insert(old_state, new_state);
            }
        }

        out.set_starting_state(mapping[&self.starting_state]);

        for (s0, (non_eps, eps)) in self.transitions {
            if !reachable.contains(&s0) {
                continue;
            }
            let s0 = mapping[&s0];

            for (s1, t) in non_eps {
                if !reachable.contains(&s1) {
                    continue;
                }
                let s1 = mapping[&s1];
                out.add_transition(s0, s1, t);
            }

            for s1 in eps {
                if !reachable.contains(&s1) {
                    continue;
                }
                let s1 = mapping[&s1];
                out.add_epsilon_transition(s0, s1);
            }
        }

        out
    }

    fn find_reachable_states(&self) -> BTreeSet<StateId> {
        let mut queue = vec![self.starting_state()];

        let mut reachable = BTreeSet::new();
        while let Some(state) = queue.pop() {
            if reachable.contains(&state) {
                continue;
            }
            reachable.insert(state);

            for (next, _) in self.transitions_from(state) {
                queue.push(next);
            }

            for next in self.epsilon_transitions_from(state) {
                queue.push(next);
            }
        }
        reachable
    }

    fn merge(&mut self, other: Self) -> (StateId, StateId) {
        let id_offset = self.states.len();

        let id_exit = StateId(other.get_single_accepting_state().unwrap().0 + id_offset);

        self.states.extend(other.states);

        for (s1, trs) in other.transitions {
            let s1 = StateId(s1.0 + id_offset);
            for (s2, t) in trs.0 {
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

    pub fn attach(&mut self, other: Self) {
        let (id_entry, _) = self.merge(other);
        self.add_epsilon_transition(self.starting_state(), id_entry);
    }

    pub fn map_accepted_token<U: Clone>(self, token: U) -> FiniteAutomaton<Option<U>, T> {
        let states = (0..self.states.len())
            .map(StateId)
            .map(|s| {
                if self.is_accepting_state(s) {
                    Some(token.clone())
                } else {
                    None
                }
            })
            .collect();
        FiniteAutomaton {
            states,
            starting_state: self.starting_state,
            accepting_states: self.accepting_states,
            transitions: self.transitions,
        }
    }

    pub fn dfa_from_nfa(self) -> Self {
        let subset_dfa = self.subset_algorithm();
        Self::dfa_from_subset(subset_dfa, &self)
    }

    fn dfa_from_subset(subset_dfa: SubsetDfa<T>, nfa: &Self) -> Self {
        let mut dfa = Self::new();
        for q in subset_dfa.states {
            let s = dfa.add_state(S::default());
            for n in q {
                if nfa.is_accepting_state(n) {
                    dfa.add_accepting_state(s);
                }
            }
        }
        dfa.transitions = subset_dfa.transitions;
        dfa.starting_state = subset_dfa.starting_state;

        dfa
    }

    fn subset_algorithm(&self) -> SubsetDfa<T> {
        let n0 = self.starting_state();
        let q0 = self.epsilon_closure(btreeset![n0]);

        let mut sublist_dfa = FiniteAutomaton::new();
        let t0 = sublist_dfa.add_state(q0);
        sublist_dfa.set_starting_state(t0);

        let mut worklist = vec![t0];
        while let Some(q) = worklist.pop() {
            for c in self.chars_leaving_subset(&sublist_dfa, q) {
                let t = self.delta_epsilon_closure(c, sublist_dfa.get_state_content(q));
                if let Some(i) = sublist_dfa.find_state_by_content(&t) {
                    sublist_dfa.add_transition(q, i, c.clone());
                } else {
                    let d_next = sublist_dfa.add_state(t);
                    sublist_dfa.add_transition(q, d_next, c.clone());
                    worklist.push(d_next);
                }
            }
        }
        sublist_dfa
    }

    fn chars_leaving_subset(&self, subset_dfa: &SubsetDfa<T>, q: StateId) -> BTreeSet<&T> {
        self.delta(subset_dfa.get_state_content(q))
            .map(|(_, t)| t)
            .collect()
    }
}

impl<S: PartialEq, T: Eq + Ord> FiniteAutomaton<S, T> {
    pub fn find_state_by_content(&self, value: &S) -> Option<StateId> {
        self.states.iter().position(|q| q == value).map(StateId)
    }
}

impl Fa {
    pub fn nfa_from_regex(re: &Regex) -> Self {
        match re {
            Regex::Empty => {
                let mut fa = Self::new();
                let s = fa.add_state(());
                fa.set_accepting_state(s);
                fa
            }
            Regex::Character(ch) => {
                let mut fa = Self::new();
                let s0 = fa.add_state(());
                let s1 = fa.add_state(());
                fa.set_accepting_state(s1);
                fa.add_transition(s0, s1, *ch);
                fa
            }
            Regex::Concatenation(a, b) => {
                let nfa1 = Self::nfa_from_regex(a);
                let nfa2 = Self::nfa_from_regex(b);
                nfa1.concatenate(nfa2)
            }
            Regex::Alternation(a, b) => {
                let nfa1 = Self::nfa_from_regex(a);
                let nfa2 = Self::nfa_from_regex(b);
                nfa1.alternate(nfa2)
            }
            Regex::Closure(a) => {
                let nfa = Self::nfa_from_regex(a);
                nfa.repeat()
            }
        }
    }

    pub fn minimize(self) -> Self {
        self.reverse()
            .dfa_from_nfa()
            .reachable()
            .reverse()
            .dfa_from_nfa()
            .reachable()
    }

    fn concatenate(mut self, other: Self) -> Self {
        let (other_entry, other_exit) = self.merge(other);

        let old_accepting_state = self.get_single_accepting_state().unwrap();

        self.set_accepting_state(other_exit);

        self.add_epsilon_transition(old_accepting_state, other_entry);

        self
    }

    fn alternate(self, other: Self) -> Self {
        let mut new = Self::new();

        let entry = new.add_state(());

        let (entry1, exit1) = new.merge(self);
        let (entry2, exit2) = new.merge(other);

        let exit = new.add_state(());
        new.set_accepting_state(exit);

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
        new.set_accepting_state(exit);

        new.add_epsilon_transition(entry, old_entry);
        new.add_epsilon_transition(old_exit, exit);
        new.add_epsilon_transition(old_exit, old_entry);
        new.add_epsilon_transition(entry, exit);

        new
    }

    pub fn create_states(&mut self) {
        let max_id = self
            .transitions
            .iter()
            .flat_map(|(s0, (non_eps, eps))| {
                std::iter::once(s0)
                    .chain(non_eps.iter().map(|(s1, _)| s1))
                    .chain(eps.iter())
            })
            .chain(self.accepting_states.iter())
            .copied()
            .max()
            .unwrap_or(StateId(0))
            .max(self.starting_state())
            .0;
        self.states.resize(max_id + 1, ());
    }
}

fn set_pop<T: Clone + Eq + Ord>(set: &mut BTreeSet<T>) -> Option<T> {
    let item = set.iter().next()?.clone();
    set.take(&item)
}

type SubsetDfa<T> = FiniteAutomaton<BTreeSet<StateId>, T>;

#[macro_export]
macro_rules! finite_automaton {
    (start: $start:expr; accept: $end:expr; $($rest:tt)*) => {{
        let mut nfa = Fa::new();
        nfa.set_starting_state(StateId($start));
        nfa.set_accepting_state(StateId($end));
        finite_automaton!(nfa, $($rest)*);
        nfa.create_states();
        nfa
    }};

    ($nfa:expr, ) => {};

    ($nfa:expr, accept: $end:expr; $($rest:tt)*) => {{
        $nfa.add_accepting_state(StateId($end));
        finite_automaton!($nfa, $($rest)*);
    }};

    ($nfa:expr, ($from:expr, ) => $to:expr; $($rest:tt)*) => {{
        $nfa.add_epsilon_transition(StateId($from), StateId($to));
        finite_automaton!($nfa, $($rest)*);
    }};

    ($nfa:expr, ($from:expr, $on:ident) => $to:expr; $($rest:tt)*) => {
        finite_automaton!($nfa, ($from, stringify!($on).as_bytes()[0]) => $to; $($rest)*)
    };

    ($nfa:expr, ($from:expr, $on:expr) => $to:expr; $($rest:tt)*) => {{
        $nfa.add_transition(StateId($from), StateId($to), $on);
        finite_automaton!($nfa, $($rest)*);
    }};
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
        let nfa1 = finite_automaton! {start: 0; accept: 1; (0, x) => 1;};
        let nfa2 = finite_automaton! {start: 0; accept: 1; (0, y) => 1;};

        let nfa = nfa1.concatenate(nfa2);

        let expected = finite_automaton! {
            start: 0;
            accept: 3;
            (0, x) => 1;
            (1, ) => 2;
            (2, y) => 3;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn alternate_nfas() {
        let nfa1 = Fa::nfa_from_regex(&Regex::new("x"));
        let nfa2 = Fa::nfa_from_regex(&Regex::new("y"));

        let nfa = nfa1.alternate(nfa2);

        let expected = finite_automaton! {
            start: 0;
            accept: 5;
            (0, ) => 1;
            (0, ) => 3;
            (1, x) => 2;
            (2, ) => 5;
            (3, y) => 4;
            (4, ) => 5;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn repeat_nfa() {
        let nfa = Fa::nfa_from_regex(&Regex::new("x"));

        let nfa = nfa.repeat();

        let expected = finite_automaton! {
            start: 0;
            accept: 3;
            (0, ) => 1;
            (0, ) => 3;
            (1, x) => 2;
            (2, ) => 3;
            (2, ) => 1;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn nfa_from_empty_regex() {
        let nfa = Fa::nfa_from_regex(&Regex::new(""));
        let expected = finite_automaton! {
            start: 0;
            accept: 0;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn nfa_from_single_char_regex() {
        let nfa = Fa::nfa_from_regex(&Regex::new("x"));
        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            (0, x) => 1;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn nfa_from_concatenated_regex() {
        let nfa = Fa::nfa_from_regex(&Regex::new("xyz"));
        let expected = finite_automaton! {
            start: 0;
            accept: 5;
            (0, x) => 1;
            (1, ) => 2;
            (2, y) => 3;
            (3, ) => 4;
            (4, z) => 5;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn nfa_from_alternating_regex() {
        let nfa = Fa::nfa_from_regex(&Regex::new("(x|y)|z"));
        let expected = finite_automaton! {
            start: 0;
            accept: 9;
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
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn nfa_from_repeating_regex() {
        let nfa = Fa::nfa_from_regex(&Regex::new("x*"));
        let expected = finite_automaton! {
            start: 0;
            accept: 3;
            (0, ) => 1;
            (0, ) => 3;
            (1, x) => 2;
            (2, ) => 1;
            (2, ) => 3;
        };
        assert_eq!(nfa, expected);
    }

    #[test]
    fn epsilon_closure_of_empty_set_is_empty_set() {
        assert_eq!(Fa::new().epsilon_closure(btreeset! {}), btreeset! {});
    }

    #[test]
    fn epsilon_closure_does_not_include_nodes_not_reachable_by_epsilon() {
        let mut nfa = Fa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        let s2 = nfa.add_state(());
        nfa.add_state(());
        nfa.add_transition(s0, s1, b'x');
        nfa.add_epsilon_transition(s1, s2);
        assert_eq!(nfa.epsilon_closure(btreeset! {s0}), btreeset! {s0});
    }

    #[test]
    fn epsilon_closure_includes_node_reachable_by_epsilon() {
        let mut nfa = Fa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        nfa.add_epsilon_transition(s0, s1);
        assert_eq!(nfa.epsilon_closure(btreeset! {s0}), btreeset! { s0, s1 });
    }

    #[test]
    fn epsilon_closure_includes_all_nodes_reachable_by_epsilon() {
        let mut nfa = Fa::new();
        let s0 = nfa.add_state(());
        let s1 = nfa.add_state(());
        let s2 = nfa.add_state(());
        let s3 = nfa.add_state(());
        nfa.add_epsilon_transition(s0, s1);
        nfa.add_epsilon_transition(s0, s2);
        nfa.add_epsilon_transition(s2, s3);
        assert_eq!(
            nfa.epsilon_closure(btreeset! {s0}),
            btreeset! {s0, s1, s2, s3}
        );
    }

    #[test]
    fn trivial_nfa_to_dfa() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 1;
            (0, a) => 1;
        };

        let dfa = Fa::dfa_from_nfa(nfa);

        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            (0, a) => 1;
        };
        assert_eq!(dfa, expected);
    }

    #[test]
    fn dfa_can_have_multiple_accepting_states() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 5;
            (0, ) => 1;
            (0, ) => 3;
            (1, a) => 2;
            (3, b) => 4;
            (2, ) => 5;
            (4, ) => 5;
        };

        let dfa = Fa::dfa_from_nfa(nfa);

        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            accept: 2;
            (0, a) => 1;
            (0, b) => 2;
        };
        assert_eq!(dfa, expected);
    }

    #[test]
    fn dfa_from_nfa_complex_example() {
        // EaC book, figure 2.7
        let nfa = finite_automaton! {
            start: 0;
            accept: 9;
            (0, a) => 1;
            (1, ) => 2;
            (2, ) => 3;
            (2, ) => 9;
            (3, ) => 4;
            (3, ) => 6;
            (4, b) => 5;
            (5, ) => 8;
            (6, c) => 7;
            (7, ) => 8;
            (8, ) => 9;
            (8, ) => 3;
        };

        let dfa = Fa::dfa_from_nfa(nfa);

        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            accept: 2;
            accept: 3;
            (0, a) => 1;
            (1, b) => 2;
            (1, c) => 3;
            (2, b) => 2;
            (2, c) => 3;
            (3, b) => 2;
            (3, c) => 3;
        };
        assert_eq!(dfa, expected);
    }

    #[test]
    fn reversing_an_empty_nfa_produces_empty_nfa() {
        let nfa = Fa::new();

        let rnfa = nfa.reverse();

        assert_eq!(rnfa.states.len(), 0);
        assert_transitions!(rnfa, {});
    }

    #[test]
    fn reversing_a_single_element_nfa_produces_same_nfa() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 0;
        };

        let rnfa = nfa.clone().reverse();
        assert_eq!(rnfa, nfa);
    }

    #[test]
    fn reversing_an_epsilon_transition() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 1;
            (0, ) => 1;
        };

        let rnfa = nfa.reverse();

        let expected = finite_automaton! {
            start: 1;
            accept: 0;
            (1, ) => 0;
        };
        assert_eq!(rnfa, expected);
    }

    #[test]
    fn reversing_a_normal_transition() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 1;
            (0, x) => 1;
        };

        let rnfa = nfa.reverse();

        let expected = finite_automaton! {
            start: 1;
            accept: 0;
            (1, x) => 0;
        };
        assert_eq!(rnfa, expected);
    }

    #[test]
    fn reversing_multiple_accept_states() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 1;
            accept: 2;
            (0, x) => 1;
            (0, y) => 2;
        };

        let rnfa = nfa.reverse();

        let expected = finite_automaton! {
            start: 3;
            accept: 0;
            (3, ) => 1;
            (3, ) => 2;
            (1, x) => 0;
            (2, y) => 0;
        };
        assert_eq!(rnfa, expected);
    }

    #[test]
    fn reversing_multiple_transitions() {
        // EaC book, figure 2.12
        let fa = finite_automaton! {
            start: 0;
            accept: 1;
            accept: 2;
            accept: 3;
            (0, a) => 1;
            (1, b) => 2;
            (1, c) => 3;
            (2, b) => 2;
            (2, c) => 3;
            (3, b) => 2;
            (3, c) => 3;
        };

        let rfa = fa.reverse();

        let expected = finite_automaton! {
            start: 4;
            accept: 0;
            (1, a) => 0;
            (2, b) => 1;
            (3, c) => 1;
            (2, b) => 2;
            (3, c) => 2;
            (2, b) => 3;
            (3, c) => 3;
            (4, ) => 1;
            (4, ) => 2;
            (4, ) => 3;
        };
        assert_eq!(rfa, expected);
    }

    #[test]
    fn reachable_keeps_states_connected_to_start() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 2;
            (0, ) => 1;
            (1, x) => 2;
        };

        let rnfa = nfa.clone().reachable();

        assert_eq!(rnfa, nfa);
    }

    #[test]
    fn reachable_removes_states_not_connected_to_start() {
        let nfa = finite_automaton! {
            start: 0;
            accept: 0;
            (1, x) => 2;
            (2, ) => 1;
        };

        let rnfa = nfa.reachable();

        let expected = finite_automaton! {
            start: 0;
            accept: 0;
        };
        assert_eq!(rnfa, expected);
    }

    #[test]
    fn reachable_works_with_self_connections() {
        let fa = finite_automaton! {
            start: 0;
            accept: 1;
            (0, a) => 1;
            (0, b) => 2;
            (0, c) => 2;
            (2, a) => 1;
            (2, b) => 2;
            (2, c) => 2;
        };

        let rfa = fa.reachable();

        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            (0, a) => 1;
            (0, b) => 2;
            (0, c) => 2;
            (2, a) => 1;
            (2, b) => 2;
            (2, c) => 2;
        };
        assert_eq!(rfa, expected);
    }

    #[test]
    fn dfa_minimizing() {
        // EaC book, figure 2.12
        let dfa = finite_automaton! {
            start: 0;
            accept: 1;
            accept: 2;
            accept: 3;
            (0, a) => 1;
            (1, b) => 2;
            (1, c) => 3;
            (2, b) => 2;
            (2, c) => 3;
            (3, b) => 2;
            (3, c) => 3;
        };

        let mindfa = dfa.minimize();

        let expected = finite_automaton! {
            start: 0;
            accept: 1;
            (0, a) => 1;
            (1, b) => 1;
            (1, c) => 1;
        };
        assert_eq!(mindfa, expected);
    }
}
