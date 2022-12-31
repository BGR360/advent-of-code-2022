#![doc = include_str!("../puzzles/16.md")]

use std::{
    collections::{BTreeMap, HashMap},
    hash, ops,
};

use advent_of_code::{debug, debugln};
use bitvec::BitArr;
use itertools::Itertools;
use rayon::prelude::*;
use smallvec::{smallvec, SmallVec};

type BitArray = BitArr!(for 64);

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

    #[inline]
    pub fn valve_ids(&self) -> impl Iterator<Item = ValveId> {
        (0..self.valve_count()).map(ValveId::from)
    }
}

#[derive(Debug)]
struct Distances {
    distances: BTreeMap<(ValveId, ValveId), u32>,
}

impl Distances {
    #[inline]
    pub fn get_between(&self, a: ValveId, b: ValveId) -> Option<u32> {
        let key = if b < a { (b, a) } else { (a, b) };

        self.distances.get(&key).copied()
    }

    pub fn for_volcano(volcano: &Volcano) -> Self {
        let distances = volcano
            .valve_ids()
            .par_bridge()
            .flat_map(|i| Self::for_starting_pos(volcano, i))
            .collect();
        Self { distances }
    }

    fn for_starting_pos(volcano: &Volcano, start: ValveId) -> Vec<((ValveId, ValveId), u32)> {
        let successors = |&id: &ValveId| -> Vec<(ValveId, u32)> {
            volcano
                .valve(id)
                .unwrap()
                .connections
                .iter()
                .map(|&id| (id, 1 /* cost (1 minute) */))
                .collect()
        };
        let mut distances = pathfinding::directed::dijkstra::dijkstra_all(&start, successors);
        distances.insert(start, (start, 0));

        distances
            .into_iter()
            .filter_map(move |(end, (_ignored, distance))| {
                if start <= end {
                    Some(((start, end), distance))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Node {
    pub valve: ValveId,
    pub open_valves: BitArray,
    pub minutes_remaining: u32,
}

type Successors = SmallVec<[(Node, u64); 8]>;

impl Node {
    #[inline]
    pub fn is_end(&self) -> bool {
        self.minutes_remaining == 0
    }

    pub fn successors(&self, volcano: &Volcano, distances: &Distances) -> Successors {
        debugln!("Getting successors for {self:?}");

        let current_id = self.valve;

        let mut successors = Successors::new();

        let current_flow_rate = self.flow_rate(volcano);

        let make_successor = |node: Node, minutes: u32| -> (Node, u64) {
            let cost = Cost {
                minutes,
                flow_rate: current_flow_rate,
            };
            if node.minutes_remaining == 0 {
                debugln!("    succ = end");
            } else {
                debugln!("    succ = {node:?}");
            }
            debugln!(
                "    flow = {} * {} = {} ({})",
                cost.minutes,
                cost.flow_rate,
                cost.minutes * cost.flow_rate,
                cost.to_num()
            );
            (node, cost.to_num())
        };

        let valve = |id: ValveId| volcano.valve(id).unwrap();

        let worth_opening = |id: ValveId| valve(id).flow_rate != 0 && !self.valve_is_open(id);

        let time_to_open = |id: ValveId| {
            let distance = distances.get_between(current_id, id).unwrap();
            distance + 1
        };

        let mut maybe_open_valve = |id: ValveId| {
            if !worth_opening(id) {
                return;
            }

            let minutes_to_open = time_to_open(id);
            if minutes_to_open > self.minutes_remaining {
                return;
            }

            let mut open_valves = self.open_valves;
            open_valves.set(id.into(), true);

            let succ = Node {
                valve: id,
                open_valves,
                minutes_remaining: self.minutes_remaining - minutes_to_open,
            };

            successors.push(make_successor(succ, minutes_to_open));
        };

        maybe_open_valve(self.valve);

        // Travel to and open any neighboring valves that are worth opening.
        for neighbor_id in volcano.valve_ids() {
            maybe_open_valve(neighbor_id);
        }

        // End node is always reachable if nothing else is.
        if successors.is_empty() {
            let end = Node {
                minutes_remaining: 0,
                ..*self
            };
            successors.push(make_successor(end, self.minutes_remaining));
        }

        successors
    }

    #[inline]
    pub fn flow_rate(&self, volcano: &Volcano) -> u32 {
        self.open_valves()
            .map(|id| volcano.valve(id).unwrap().flow_rate)
            .sum()
    }

    #[inline]
    pub fn open_valves(&self) -> impl Iterator<Item = ValveId> + '_ {
        self.open_valves.iter_ones().map(ValveId::from)
    }

    #[inline]
    pub fn valve_is_open(&self, id: ValveId) -> bool {
        self.open_valves.get(id.raw()).map(|b| *b).unwrap_or(false)
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct Cost {
    /// Distance in minutes to the next node (including one minute to open the
    /// next valve, if this edge does not lead to the END node).
    pub minutes: u32,
    /// Flow rate maintained while traveling to the valve (and while opening it).
    pub flow_rate: u32,
}

impl Cost {
    #[inline(always)]
    pub fn to_num(self) -> u64 {
        let cost_for_one_minute = u64::from(u32::MAX - self.flow_rate);
        cost_for_one_minute * u64::from(self.minutes)
    }
}

fn valve_ids_to_strings(volcano: &Volcano, ids: &[ValveId]) -> Vec<String> {
    ids.iter()
        .map(|&id| volcano.valve(id).unwrap().name.to_string())
        .collect()
}

fn get_max_pressure_released(
    volcano: &Volcano,
    minutes: u32,
    elephant_helping: bool,
) -> Option<u32> {
    let distances = Distances::for_volcano(volcano);

    let aa = volcano.name_to_id(ValveName::AA).unwrap();
    let start = Node {
        valve: aa,
        open_valves: BitArray::ZERO,
        minutes_remaining: minutes,
    };

    let successors = |node: &Node| -> Successors { node.successors(volcano, &distances) };

    let success = |&node: &Node| node.is_end();

    let (best_path, cost) =
        pathfinding::directed::dijkstra::dijkstra(&start, successors, success).unwrap();

    let cost_with_no_pressure = u64::from(u32::MAX) * u64::from(minutes);
    let expected_flow = cost_with_no_pressure - cost;
    debugln!("Expected total flow: {expected_flow}");

    println!();
    println!("=== Best Path ===");
    println!();
    let total_flow = print_detailed_path_and_get_flow(volcano, &best_path);

    Some(total_flow)
}

fn print_detailed_path_and_get_flow(volcano: &Volcano, path: &[Node]) -> u32 {
    let name = |valve| {
        if let Some(valve) = valve {
            volcano.valve(valve).unwrap().name
        } else {
            ValveName('(', ')')
        }
    };

    let mut total_flow = 0;
    let mut prev_node = path[0];

    for &node in path {
        let minutes = node.minutes_remaining;

        let my_name = name(Some(node.valve));
        // let elephant_name = name(node.elephant_valve);

        let open_valves: Vec<String> = node
            .open_valves()
            .map(|id| volcano.valve(id).unwrap().name.to_string())
            .collect();

        let additional_flow =
            (prev_node.minutes_remaining - minutes) * prev_node.flow_rate(volcano);

        println!(
            "t={minutes:>2}, f={total_flow:>4}, v0={my_name}, \
            {open_valves:?} -> +{additional_flow}"
        );

        total_flow += additional_flow;
        prev_node = node;
    }

    let mut total_flow = 0;

    for (&node_a, &node_b) in path.iter().tuple_windows() {
        let delta_minutes = node_a.minutes_remaining - node_b.minutes_remaining;
        let flow_rate = node_a.flow_rate(volcano);
        total_flow += delta_minutes * flow_rate;

        println!(
            "{}",
            formatting::EdgePrinter {
                a: node_a,
                b: node_b,
                volcano,
            }
        );
    }

    total_flow
}

fn part_one(volcano: &Volcano) -> Option<u32> {
    get_max_pressure_released(volcano, 30, false /* elephant_helping */)
}

fn part_two(volcano: &Volcano) -> Option<u32> {
    get_max_pressure_released(volcano, 26, true /* elephant_helping */)
}

include!("../inputs/16.rs");

fn main() {
    let input = input();
    advent_of_code::solve!(1, part_one, &input);
    advent_of_code::solve!(2, part_two, &input);
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn test_part_one_tiny() {
        let input = tiny_volcano();
        assert_eq!(
            get_max_pressure_released(&input, 5, false /* elephant_helping */),
            Some(63)
        );
    }

    #[test]
    fn test_part_one_tiny2() {
        let input = tiny_volcano();
        assert_eq!(
            get_max_pressure_released(&input, 10, false /* elephant_helping */),
            Some(3 * 3 + 5 * 13 + 8 * 20)
        );
    }

    #[test]
    fn test_part_one_tiny3() {
        let input = tiny_volcano();
        assert_eq!(
            get_max_pressure_released(&input, 6, false /* elephant_helping */),
            Some(1 * 13 + 4 * 20)
        );
    }

    #[test]
    fn test_part_one_tiny4() {
        let input = tiny_volcano();
        assert_eq!(
            get_max_pressure_released(&input, 7, false /* elephant_helping */),
            Some(2 * 13 + 5 * 20)
        );
    }

    #[test]
    fn test_part_one() {
        let input = example_volcano();
        assert_eq!(part_one(&input), Some(1651));
    }

    #[test]
    fn test_part_two() {
        let input = example_volcano();
        assert_eq!(part_two(&input), Some(1707));
    }
}

mod formatting {
    use super::*;

    use std::fmt::{Debug, Display, Formatter, Result};

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

    impl Debug for Node {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            f.debug_struct("Node")
                .field("t", &self.minutes_remaining)
                .field("v", &self.valve)
                .field("open", &self.open_valves().map(|id| id.raw()).collect_vec())
                .finish()
        }
    }

    pub(super) struct EdgePrinter<'a> {
        pub a: Node,
        pub b: Node,
        pub volcano: &'a Volcano,
    }

    impl Display for EdgePrinter<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            let name = |id| self.volcano.valve(id).unwrap().name;

            let a_name = name(self.a.valve);
            let b_name = name(self.b.valve);

            let current_minute = self.a.minutes_remaining;
            let delta_minutes = self.a.minutes_remaining - self.b.minutes_remaining;
            let flow_rate = self.a.flow_rate(self.volcano);
            let additional_flow = delta_minutes * flow_rate;

            let is_last = self.b.minutes_remaining == 0;

            writeln!(f, "at t={current_minute:>2}:")?;

            if is_last {
                writeln!(
                    f,
                    "    Wait {delta_minutes} minutes at valve {a_name} until time runs out."
                )?;
            } else {
                writeln!(
                    f,
                    "    Travel {delta_minutes} minutes from valve {a_name} to {b_name} and open it."
                )?;
            }

            if self.a.open_valves().count() == 0 {
                write!(f, "    No valves are open, no pressure is released.")?;
            } else {
                write!(f, "    Valves [")?;
                let mut first = true;
                for id in self.a.open_valves() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", name(id))?;
                    first = false
                }
                write!(
                    f,
                    "] release {flow_rate} pressure per min, raising the total by {additional_flow}."
                )?;
            }

            Ok(())
        }
    }
}
