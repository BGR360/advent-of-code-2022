#![doc = include_str!("../puzzles/16.md")]

use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    hash, ops,
};

use advent_of_code::{debug, debugln};
use bitvec::{vec::IntoIter, BitArr};
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

type Succs<NodeT, CostT> = SmallVec<[(NodeT, CostT); 16]>;

trait GraphNode: Sized + Clone + Eq + hash::Hash {
    fn minutes_remaining(&self) -> u32;
    fn successors(&self, volcano: &Volcano, distances: &Distances) -> Succs<Self, Cost>;

    #[inline]
    fn is_end(&self) -> bool {
        self.minutes_remaining() == 0
    }
}

struct BestPath<NodeT> {
    pub path: Vec<NodeT>,
    pub total_flow: u32,
}

impl<NodeT: GraphNode> BestPath<NodeT> {
    pub fn calculate(volcano: &Volcano, distances: &Distances, start: &NodeT) -> Self {
        let minutes = start.minutes_remaining();

        let mut total_successors = 0;
        let successors = |node: &NodeT| -> Succs<NodeT, u64> {
            let succs = node.successors(volcano, distances);

            total_successors += succs.len();
            if total_successors % 100_000 == 0 {
                println!("== {total_successors} successors ==");
            }

            succs
                .into_iter()
                .map(|(node, cost)| (node, cost.to_num()))
                .collect()
        };

        let success = |node: &NodeT| node.is_end();

        let (best_path, cost) =
            pathfinding::directed::dijkstra::dijkstra(start, successors, success).unwrap();

        let worst_case_cost = Cost {
            minutes,
            flow_rate: 0,
        };
        let total_flow = worst_case_cost.to_num() - cost;
        println!("Total flow: {total_flow}");
        println!("Total number of nodes: {total_successors}");

        Self {
            path: best_path,
            total_flow: total_flow.try_into().unwrap(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct OpenValves(BitArray);

impl OpenValves {
    #[inline]
    pub fn contains(&self, id: ValveId) -> bool {
        self.0.get(id.raw()).map(|b| *b).unwrap_or(false)
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = ValveId> + '_ {
        self.0.iter_ones().map(ValveId::from)
    }

    #[inline]
    pub fn with_opened(mut self, id: ValveId) -> Self {
        self.0.set(id.raw(), true);
        self
    }
}

impl Default for OpenValves {
    fn default() -> Self {
        Self(BitArray::ZERO)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Node {
    pub valve: ValveId,
    pub open_valves: OpenValves,
    pub minutes_remaining: u32,
}

impl Node {
    #[inline]
    pub fn flow_rate(&self, volcano: &Volcano) -> u32 {
        self.open_valves()
            .map(|id| volcano.valve(id).unwrap().flow_rate)
            .sum()
    }

    #[inline]
    pub fn open_valves(&self) -> impl Iterator<Item = ValveId> + '_ {
        self.open_valves.iter()
    }

    #[inline]
    pub fn valve_is_open(&self, id: ValveId) -> bool {
        self.open_valves.contains(id)
    }
}

impl GraphNode for Node {
    fn minutes_remaining(&self) -> u32 {
        self.minutes_remaining
    }

    fn successors(&self, volcano: &Volcano, distances: &Distances) -> Succs<Self, Cost> {
        debugln!("Getting successors for {self:?}");

        let current_id = self.valve;

        let mut successors = Succs::new();

        let current_flow_rate = self.flow_rate(volcano);

        let make_successor = |node: Node, minutes: u32| -> (Node, Cost) {
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
            (node, cost)
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

            let succ = Node {
                valve: id,
                open_valves: self.open_valves.with_opened(id),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Position {
    At(ValveId),
    EnRoute { to: ValveId, in_minutes: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct DoubleNode {
    pub minutes_remaining: u32,
    /// The current valve that either I or the elephant is standing at.
    /// At least one of us will be at a valve in any given state.
    pub valve: ValveId,
    /// The other party (either the elephant or me, respectively) will either be
    /// standing at a valve too, or currently on their way to one.
    pub other: Position,
    pub open_valves: OpenValves,
}

impl DoubleNode {
    #[inline]
    pub fn flow_rate(&self, volcano: &Volcano) -> u32 {
        self.open_valves()
            .map(|id| volcano.valve(id).unwrap().flow_rate)
            .sum()
    }

    #[inline]
    pub fn open_valves(&self) -> impl Iterator<Item = ValveId> + '_ {
        self.open_valves.iter()
    }
}

impl GraphNode for DoubleNode {
    fn minutes_remaining(&self) -> u32 {
        self.minutes_remaining
    }

    fn successors(&self, volcano: &Volcano, distances: &Distances) -> Succs<Self, Cost> {
        let current_flow_rate = self.flow_rate(volcano);

        let mut successors = Succs::new();

        let make_successor = |node: DoubleNode, minutes: u32| {
            let cost = Cost {
                minutes,
                flow_rate: current_flow_rate,
            };
            formatting::log_successor(node, cost, node.minutes_remaining == 0);
            (node, cost)
        };

        let single_succs = |id: ValveId| -> Succs<Node, Cost> {
            Node {
                valve: id,
                minutes_remaining: self.minutes_remaining,
                open_valves: self.open_valves,
            }
            .successors(volcano, distances)
        };

        let current_id = self.valve;
        let current_succs = single_succs(current_id);

        let other_succs = match self.other {
            Position::At(other_id) => single_succs(other_id),
            Position::EnRoute { to, in_minutes } => {
                let mut maybe_one_succ = Succs::new();
                if in_minutes <= self.minutes_remaining {
                    let node = Node {
                        valve: to,
                        open_valves: self.open_valves,
                        minutes_remaining: self.minutes_remaining - in_minutes,
                    };
                    let cost = Cost {
                        minutes: in_minutes,
                        flow_rate: current_flow_rate,
                    };
                    maybe_one_succ.push((node, cost));
                }
                maybe_one_succ
            }
        };

        for (&(a, a_cost), &(b, b_cost)) in
            current_succs.iter().cartesian_product(other_succs.iter())
        {
            let a_id = a.valve;
            let b_id = b.valve;
            let a_time = a_cost.minutes;
            let b_time = b_cost.minutes;

            let next_id: ValveId;
            let next_other: Position;
            let minutes: u32;
            let open_valves;
            match a_time.cmp(&b_time) {
                Ordering::Less => {
                    next_id = a_id;
                    minutes = a_time;
                    open_valves = self.open_valves.with_opened(a_id);
                    next_other = Position::EnRoute {
                        to: b_id,
                        in_minutes: b_time - a_time,
                    };
                }
                Ordering::Greater => {
                    next_id = b_id;
                    minutes = b_time;
                    open_valves = self.open_valves.with_opened(b_id);
                    next_other = Position::EnRoute {
                        to: a_id,
                        in_minutes: a_time - b_time,
                    };
                }
                Ordering::Equal => {
                    next_id = a_id;
                    minutes = a_time;
                    next_other = Position::At(b_id);
                    open_valves = self.open_valves.with_opened(a_id).with_opened(b_id);
                }
            }

            let node = DoubleNode {
                minutes_remaining: self.minutes_remaining - minutes,
                valve: next_id,
                other: next_other,
                open_valves,
            };
            successors.push(make_successor(node, minutes));
        }

        successors
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

    let start_valve = volcano.name_to_id(ValveName::AA).unwrap();

    if elephant_helping {
        let start = DoubleNode {
            valve: start_valve,
            other: Position::At(start_valve),
            open_valves: OpenValves::default(),
            minutes_remaining: minutes,
        };

        let best_path = BestPath::calculate(volcano, &distances, &start);

        Some(best_path.total_flow)
    } else {
        let start = Node {
            valve: start_valve,
            open_valves: OpenValves::default(),
            minutes_remaining: minutes,
        };

        let best_path = BestPath::calculate(volcano, &distances, &start);

        println!();
        println!("=== Best Path ===");
        println!();
        let total_flow = print_detailed_path_and_get_flow(volcano, &best_path.path);

        Some(total_flow)
    }
}

fn print_detailed_path_and_get_flow(volcano: &Volcano, path: &[Node]) -> u32 {
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
                .field("open", &self.open_valves)
                .finish()
        }
    }

    impl Debug for OpenValves {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            let mut list = f.debug_list();
            for i in self.0.iter_ones() {
                list.entry(&i);
            }
            list.finish()
        }
    }

    #[inline]
    pub(super) fn log_successor<NodeT: Debug>(node: NodeT, cost: Cost, is_end: bool) {
        if is_end {
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
