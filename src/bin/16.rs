#![doc = include_str!("../puzzles/16.md")]

use std::collections::{HashMap, HashSet};

use advent_of_code::debugln;
use bitvec::BitArr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ValveName(pub char, pub char);

impl ValveName {
    pub const AA: Self = Self('A', 'A');
}

index_vec::define_index_type! {
    struct ValveId = usize;
}

#[derive(Debug, Clone)]
struct Valve {
    pub name: ValveName,
    pub flow_rate: u32,
    pub connections: Vec<ValveId>,
}

/// A vector indexed by [`ValveId`].
type ValveVec<T> = index_vec::IndexVec<ValveId, T>;

#[derive(Debug)]
struct Volcano {
    name_to_id: HashMap<ValveName, ValveId>,
    valves: ValveVec<Valve>,
}

impl Volcano {
    pub fn new(valves: impl IntoIterator<Item = (ValveName, u32, Vec<ValveName>)>) -> Self {
        let valves: Vec<_> = valves.into_iter().collect();

        let mut name_to_id = HashMap::new();
        for (i, (valve_name, _, _)) in valves.iter().enumerate() {
            name_to_id.insert(*valve_name, ValveId::from(i));
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
        self.valves.get(id)
    }

    #[inline]
    pub fn valve_by_name(&self, name: ValveName) -> Option<&Valve> {
        let id = self.name_to_id.get(&name)?;
        self.valve(*id)
    }

    #[inline]
    pub fn first_valve(&self) -> ValveId {
        *self.name_to_id.get(&ValveName::AA).unwrap()
    }

    #[inline]
    pub fn valve_count(&self) -> usize {
        self.valves.len()
    }

    #[inline]
    pub fn name_to_id(&self, name: ValveName) -> Option<ValveId> {
        self.name_to_id.get(&name).copied()
    }
}

#[derive(Debug, Clone)]
struct State {
    pub valve: ValveId,
    pub minutes_remaining: u32,
    pub total_flow: u32,
    pub open_valves: ValveVec<bool>,
}

impl State {
    pub fn advance_minute(&mut self, volcano: &Volcano) {
        assert!(self.minutes_remaining > 0);

        let additional_flow: u32 = (0..volcano.valve_count())
            .map(ValveId::from)
            .filter(|&id| self.valve_is_open(id))
            .map(|id| volcano.valve(id).unwrap().flow_rate)
            .sum();
        self.total_flow += additional_flow;

        self.minutes_remaining -= 1;
    }

    pub fn open_valve(&mut self, id: ValveId) {
        assert!(!self.valve_is_open(id));
        self.open_valves[id] = true;
    }

    pub fn valve_is_open(&self, id: ValveId) -> bool {
        self.open_valves[id]
    }
}

/// Returns the id of the valve that should be opened next given the current
/// state, along with how many minutes it will take to move to that valve and
/// open it.
fn get_best_valve_to_open(volcano: &Volcano, state: &State) -> Option<(ValveId, u32)> {
    use std::fmt;

    // Hashmap from destination id to (<ignored>, distance in minutes).
    type Distances = HashMap<ValveId, (ValveId, u32)>;

    struct DistancesPrinter<'a> {
        distances: &'a HashMap<ValveId, (ValveId, u32)>,
        volcano: &'a Volcano,
    }

    impl fmt::Display for DistancesPrinter<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut map = f.debug_map();

            for (&id, &(_ignored, distance)) in self.distances.iter() {
                let name = self.volcano.valve(id).unwrap().name;
                map.entry(&name.to_string(), &distance);
            }

            map.finish()
        }
    }

    debugln!("== Find best valve ==");
    debugln!("State: {state:?}");

    // Find the shortest path from the current valve to each of the currently
    // unopened valves (excluding the current one).
    // `distances` is a hashmap .
    let start = state.valve;
    let successors = |&id: &ValveId| -> Vec<(ValveId, u32)> {
        volcano
            .valve(id)
            .unwrap()
            .connections
            .iter()
            .filter(|&&id| !state.valve_is_open(id))
            .map(|&id| (id, 1 /* cost (1 minute) */))
            .collect()
    };
    let mut distances: Distances =
        pathfinding::directed::dijkstra::dijkstra_all(&start, successors);

    // Stick an entry in there for opening the current valve, if it's not
    // already open.
    if !state.valve_is_open(start) {
        distances.insert(start, (start, 0));
    }

    debugln!(
        "distances: {}",
        DistancesPrinter {
            distances: &distances,
            volcano
        }
    );

    // Figure out which candidate valve would yield the most total flow if we
    // went to turn it on right now.
    let best = distances
        .into_iter()
        .filter_map(|(id, (_ignored, distance))| {
            // Time to travel to the valve + one more minute to open it.
            let time_to_open = distance + 1;

            if let Some(minutes_of_flow) = state.minutes_remaining.checked_sub(time_to_open) {
                let flow_rate = volcano.valve(id).unwrap().flow_rate;
                Some((id, flow_rate * minutes_of_flow, time_to_open))
            } else {
                None
            }
        })
        .max_by(|&(_, flow_a, time_a), &(_, flow_b, time_b)| {
            // We want max by flow then min by time.
            let time_a = state.minutes_remaining - time_a;
            let time_b = state.minutes_remaining - time_b;

            (flow_a, time_a).cmp(&(flow_b, time_b))
        });

    match best {
        Some((best_id, total_flow, time_to_open)) => {
            let best_name = volcano.valve(best_id).unwrap().name;
            debugln!(
                "Best next valve: {best_name} ({best_id}), will yield {total_flow} total flow \
                after {time_to_open} minutes."
            );
            Some((best_id, time_to_open))
        }
        None => {
            debugln!("No suitable next valve to open.");
            None
        }
    }
}

fn get_max_pressure_released(volcano: &Volcano, minutes: u32) -> Option<u32> {
    let mut state = State {
        valve: volcano.name_to_id(ValveName::AA).unwrap(),
        minutes_remaining: minutes,
        total_flow: 0,
        open_valves: vec![false; volcano.valve_count()].into(),
    };

    loop {
        if let Some((valve_to_open, time_to_open)) = get_best_valve_to_open(volcano, &state) {
            // Move to the valve and open it.
            for _ in 0..time_to_open {
                state.advance_minute(volcano);
            }
            state.open_valve(valve_to_open);
            state.valve = valve_to_open;
        } else {
            break;
        }
    }

    debugln!("Final state: {state:#?}");

    Some(state.total_flow)
}

fn part_one(volcano: &Volcano) -> Option<u32> {
    get_max_pressure_released(volcano, 30)
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

fn tiny_volcano() -> Volcano {
    const AA: ValveName = ValveName('A', 'A');
    const BB: ValveName = ValveName('B', 'B');
    const CC: ValveName = ValveName('C', 'C');
    const DD: ValveName = ValveName('D', 'D');

    Volcano::new([
        (AA, 3, vec![BB, DD]),
        (BB, 13, vec![AA, CC]),
        (CC, 2, vec![BB, DD]),
        (DD, 20, vec![CC, AA]),
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
    fn test_part_one_tiny() {
        let input = tiny_volcano();
        assert_eq!(get_max_pressure_released(&input, 5), Some(63));
    }

    #[test]
    fn test_part_one_tiny2() {
        let input = tiny_volcano();
        assert_eq!(
            get_max_pressure_released(&input, 10),
            Some(3 * 3 + 5 * 13 + 8 * 20)
        );
    }

    #[test]
    fn test_part_one_tiny3() {
        let input = tiny_volcano();
        assert_eq!(get_max_pressure_released(&input, 6), Some(1 * 13 + 4 * 20));
    }

    #[test]
    fn test_part_one_tiny4() {
        let input = tiny_volcano();
        assert_eq!(get_max_pressure_released(&input, 7), Some(2 * 13 + 5 * 20));
    }

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

mod formatting {
    use super::*;

    use std::fmt::{Display, Formatter, Result};

    impl Display for ValveName {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}{}", self.0, self.1)
        }
    }

    impl Display for ValveId {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{}", self.index())
        }
    }
}
