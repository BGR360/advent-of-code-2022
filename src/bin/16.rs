#![doc = include_str!("../puzzles/16.md")]

use std::collections::{HashMap, HashSet};
use std::hash;
use std::ops::ControlFlow;

use advent_of_code::debugln;
use bitvec::BitArr;
use smallvec::SmallVec;

type BitArray = BitArr!(for 64, in u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValveName(pub char, pub char);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValveId(pub usize);

#[derive(Debug, Clone)]
struct Valve {
    pub name: ValveName,
    pub flow_rate: u32,
    pub connections: Vec<ValveId>,
}

#[derive(Debug)]
struct Volcano {
    name_to_id: HashMap<ValveName, ValveId>,
    /// Indexed by [`ValveId`].
    valves: Vec<Valve>,
}

impl Volcano {
    pub fn new<'a>(valves: impl IntoIterator<Item = (ValveName, u32, Vec<ValveName>)>) -> Self {
        let valves: Vec<_> = valves.into_iter().collect();

        let mut name_to_id = HashMap::new();
        for (i, (valve_name, _, _)) in valves.iter().enumerate() {
            name_to_id.insert(*valve_name, ValveId(i));
        }

        let valves = valves
            .into_iter()
            .map(|(name, flow_rate, connections)| Valve {
                name,
                flow_rate,
                connections: connections
                    .into_iter()
                    .map(|name| name_to_id[&name])
                    .collect(),
            })
            .collect();

        Self { name_to_id, valves }
    }

    #[inline]
    pub fn valve(&self, id: ValveId) -> Option<&Valve> {
        self.valves.get(id.0)
    }

    #[inline]
    pub fn valve_by_name(&self, name: ValveName) -> Option<&Valve> {
        let id = self.name_to_id.get(&name)?;
        self.valve(*id)
    }

    #[inline]
    pub fn first_valve(&self) -> ValveId {
        *self.name_to_id.get(&ValveName('A', 'A')).unwrap()
    }

    #[inline]
    pub fn valve_count(&self) -> usize {
        self.valves.len()
    }
}

/// A state of the world that can be visited during the search algorithm.
#[derive(Debug, Clone)]
struct State {
    /// The valve at which you are currently standing.
    pub valve: ValveId,
    /// The number of minutes that have elapsed so far.
    pub minutes_elapsed: u32,
    /// The total amount of pressure released prior to reaching this state.
    pub pressure_released: u32,
    /// Whether or not each valve is currently open
    pub valves_open: SmallVec<[bool; 64]>,
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        // Exclude pressure_released from the equality check.
        self.valve == other.valve
            && self.minutes_elapsed == other.minutes_elapsed
            && self.valves_open == other.valves_open
    }
}

impl Eq for State {}

impl hash::Hash for State {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        // Exclude pressure_released from the hash.
        self.valve.hash(state);
        self.minutes_elapsed.hash(state);
        self.valves_open.hash(state);
    }
}

#[derive(Debug)]
struct Search<'a> {
    volcano: &'a Volcano,
    /// Set of states visited so far.
    visited: HashSet<State>,
    state: State,
}

impl<'a> Search<'a> {
    pub fn new(volcano: &'a Volcano) -> Self {
        // let mut valves_open = BitArray::ZERO;
        let mut valves_open = SmallVec::new();
        valves_open.resize(volcano.valve_count(), false);
        Self {
            volcano,
            visited: HashSet::new(),
            state: State {
                valve: volcano.first_valve(),
                minutes_elapsed: 0,
                pressure_released: 0,
                valves_open,
            },
        }
    }

    pub fn execute(&mut self) {
        debugln!("Visiting state {:?}", self.state);

        let not_visited_yet = self.visited.insert(self.state.clone());
        debug_assert!(not_visited_yet);

        let n_successors = self.current_valve().connections.len();
        for i in 0..n_successors {
            let valve = self.current_valve();
            let next_valve = valve.connections[i];
            if self.valve_is_worth_opening(self.state.valve) {
                self.recurse(true, next_valve);
            }
            self.recurse(false, next_valve);
        }
    }

    pub fn visited_states(&self) -> impl Iterator<Item = &State> {
        self.visited.iter()
    }

    #[inline]
    fn recurse(&mut self, open_current_valve: bool, next_valve: ValveId) {
        self.with_saved_state(|this| this.recurse_inner(open_current_valve, next_valve));
    }

    fn recurse_inner(&mut self, open_current_valve: bool, next_valve: ValveId) -> ControlFlow<()> {
        let new_flow: u32 = self.open_valves().map(|valve| valve.flow_rate).sum();
        self.state.pressure_released += new_flow;

        self.state.minutes_elapsed += 1;
        self.check_time()?;
        if open_current_valve {
            self.state.minutes_elapsed += 1;
            // self.state.valves_open.set(self.state.valve.0, true);
            self.state.valves_open[self.state.valve.0] = true;
            self.check_time()?;
        }

        self.state.valve = next_valve;
        self.check_visited()?;

        self.execute();

        ControlFlow::Continue(())
    }

    #[inline]
    fn current_valve(&self) -> &Valve {
        self.volcano.valve(self.state.valve).unwrap()
    }

    #[inline]
    fn valve_is_worth_opening(&self, id: ValveId) -> bool {
        !self.valve_is_open(id) && self.volcano.valve(id).unwrap().flow_rate != 0
    }

    #[inline]
    fn valve_is_open(&self, id: ValveId) -> bool {
        self.state.valves_open[id.0]
    }

    #[inline]
    fn open_valves(&self) -> impl Iterator<Item = &Valve> {
        self.state
            .valves_open
            .iter()
            .enumerate()
            .filter_map(|(i, open)| {
                if *open {
                    self.volcano.valve(ValveId(i))
                } else {
                    None
                }
            })
    }

    #[inline]
    fn with_saved_state<R>(&mut self, op: impl FnOnce(&mut Self) -> R) -> R {
        let old_state = self.state.clone();

        let ret = op(self);

        self.state = old_state;

        ret
    }

    #[inline]
    fn check_time(&self) -> ControlFlow<()> {
        if self.state.minutes_elapsed >= 30 {
            ControlFlow::Break(())
        } else {
            ControlFlow::Continue(())
        }
    }

    #[inline]
    fn check_visited(&mut self) -> ControlFlow<()> {
        if let Some(visited) = self.visited.get(&self.state) {
            if visited.pressure_released >= self.state.pressure_released {
                return ControlFlow::Break(());
            }
            self.visited.remove(&self.state);
        }
        ControlFlow::Continue(())
    }
}

fn part_one(volcano: &Volcano) -> Option<u32> {
    let mut search = Search::new(volcano);
    search.execute();

    debugln!("GOT HERE");

    let best_flow = search
        .visited_states()
        .map(|state| state.pressure_released)
        .max()
        .unwrap();

    Some(best_flow)
}

fn part_two(input: &Volcano) -> Option<u32> {
    None
}

fn example_volcano() -> Volcano {
    const AA: ValveName = ValveName('A', 'A');
    const BB: ValveName = ValveName('B', 'B');
    const CC: ValveName = ValveName('C', 'C');
    const DD: ValveName = ValveName('D', 'D');
    const EE: ValveName = ValveName('E', 'E');
    const FF: ValveName = ValveName('F', 'F');
    const GG: ValveName = ValveName('G', 'G');
    const HH: ValveName = ValveName('H', 'H');
    const II: ValveName = ValveName('I', 'I');
    const JJ: ValveName = ValveName('J', 'J');

    Volcano::new([
        (AA, 0, vec![DD, II, BB]),
        (BB, 13, vec![CC, AA]),
        (CC, 2, vec![DD, BB]),
        (DD, 20, vec![CC, AA, EE]),
        (EE, 3, vec![FF, DD]),
        (FF, 0, vec![EE, GG]),
        (GG, 0, vec![FF, HH]),
        (HH, 22, vec![GG]),
        (II, 0, vec![AA, JJ]),
        (JJ, 21, vec![II]),
    ])
}

include!("../inputs/16.rs");

fn main() {
    //let input = input();
    let input = example_volcano();
    advent_of_code::solve!(1, part_one, &input);
    advent_of_code::solve!(2, part_two, &input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = example_volcano();
        assert_eq!(part_one(&input), Some(1651));
    }

    #[test]
    fn test_part_two() {
        let input = example_volcano();
        assert_eq!(part_two(&input), None);
    }
}
