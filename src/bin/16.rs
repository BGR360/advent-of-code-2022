#![doc = include_str!("../puzzles/16.md")]

use std::{collections::HashMap, hash};

use advent_of_code::debugln;
use bitvec::BitArr;
use itertools::Itertools;
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

#[derive(Debug, Clone, Copy)]
struct Node {
    pub my_valve: Option<ValveId>,
    pub elephant_valve: Option<ValveId>,
    pub open_valves: BitArray,
    pub minutes_remaining: u8,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        ((self.my_valve == other.my_valve && self.elephant_valve == other.elephant_valve)
            || (self.my_valve == other.elephant_valve && self.elephant_valve == other.my_valve))
            && self.open_valves == other.open_valves
            && self.minutes_remaining == other.minutes_remaining
    }
}

impl Eq for Node {}

impl hash::Hash for Node {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        use std::cmp::Ordering;

        #[inline]
        fn cmp_valves(a: Option<ValveId>, b: Option<ValveId>) -> Ordering {
            match (a, b) {
                (Some(a), Some(b)) => a.cmp(&b),
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            }
        }

        if cmp_valves(self.my_valve, self.elephant_valve).is_le() {
            self.my_valve.hash(state);
            self.elephant_valve.hash(state);
        } else {
            self.elephant_valve.hash(state);
            self.my_valve.hash(state);
        }

        self.open_valves.hash(state);
        self.minutes_remaining.hash(state);
    }
}

type Successors = SmallVec<[(Node, i64); 8]>;

impl Node {
    pub const END: Self = Self {
        my_valve: None,
        elephant_valve: None,
        open_valves: BitArray::ZERO,
        minutes_remaining: 0,
    };

    pub fn successors(&self, volcano: &Volcano) -> Successors {
        let Some(my_valve) = self.my_valve else {
            return Default::default();
        };

        let current_flow_rate = self.flow_rate(volcano);

        let mut successors = SmallVec::new();

        let neighbors = |valve| volcano.valve(valve).unwrap().connections.iter().copied();

        let my_neighbors: SmallVec<[ValveId; 5]> = neighbors(my_valve).collect();
        let elephant_neighbors: SmallVec<[Option<ValveId>; 5]> =
            if let Some(elephant_valve) = self.elephant_valve {
                neighbors(elephant_valve).map(Some).collect()
            } else {
                smallvec![None]
            };

        let worth_opening = |valve: ValveId| {
            !self.valve_is_open(valve) && volcano.valve(valve).unwrap().flow_rate != 0
        };

        debugln!("Successors for {self:?}");

        if let Some(remaining) = self.minutes_remaining.checked_sub(1) {
            // Open neither valve and travel to two neighbors.
            for (&my_neighbor, &elephant_neighbor) in my_neighbors
                .iter()
                .cartesian_product(elephant_neighbors.iter())
            {
                debugln!(
                    "  open_neither: my_neighbor={:?}, elephant_neighbor={:?}",
                    my_neighbor,
                    elephant_neighbor
                );
                let succ = Node {
                    my_valve: Some(my_neighbor),
                    elephant_valve: elephant_neighbor,
                    open_valves: self.open_valves,
                    minutes_remaining: remaining,
                };
                successors.push((succ, current_flow_rate.into()));
            }

            // Open my valve only, if it's worth opening.
            if worth_opening(my_valve) {
                let mut open_valves = self.open_valves;
                open_valves.set(my_valve.into(), true);

                for &elephant_neighbor in elephant_neighbors.iter() {
                    debugln!("  open_mine: elephant_neighbor={:?}", elephant_neighbor);
                    let succ = Node {
                        my_valve: Some(my_valve),
                        elephant_valve: elephant_neighbor,
                        open_valves,
                        minutes_remaining: remaining,
                    };
                    successors.push((succ, current_flow_rate.into()));
                }
            }

            if let Some(elephant_valve) = self.elephant_valve {
                // Open the elephant's valve only, if it's worth opening.
                if worth_opening(elephant_valve) {
                    let mut open_valves = self.open_valves;
                    open_valves.set(elephant_valve.into(), true);

                    for &my_neighbor in my_neighbors.iter() {
                        debugln!("  open_elephant: my_neighbor={:?}", my_neighbor);
                        let succ = Node {
                            my_valve: Some(my_neighbor),
                            elephant_valve: Some(elephant_valve),
                            open_valves,
                            minutes_remaining: remaining,
                        };
                        successors.push((succ, current_flow_rate.into()));
                    }
                }

                // Open both valves, if they're both worth opening.
                if worth_opening(elephant_valve) && worth_opening(my_valve) {
                    let mut open_valves = self.open_valves;
                    open_valves.set(my_valve.into(), true);
                    open_valves.set(elephant_valve.into(), true);

                    debugln!("  open_both");
                    let succ = Node {
                        my_valve: Some(my_valve),
                        elephant_valve: Some(elephant_valve),
                        open_valves,
                        minutes_remaining: remaining,
                    };
                    successors.push((succ, current_flow_rate.into()));
                }
            }
        } else {
            successors.push((Node::END, 0));
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

fn valve_ids_to_strings(volcano: &Volcano, ids: &[ValveId]) -> Vec<String> {
    ids.iter()
        .map(|&id| volcano.valve(id).unwrap().name.to_string())
        .collect()
}

fn get_max_pressure_released(
    volcano: &Volcano,
    minutes: u8,
    elephant_helping: bool,
) -> Option<u32> {
    let aa = volcano.name_to_id(ValveName::AA).unwrap();
    let start = Node {
        my_valve: Some(aa),
        elephant_valve: if elephant_helping { Some(aa) } else { None },
        open_valves: BitArray::ZERO,
        minutes_remaining: minutes,
    };
    let successors = |node: &Node| -> Successors {
        let mut successors = node.successors(volcano);

        // Need to formulate the problem as min-cost path for Dijkstra's.
        for (succ, cost) in successors.iter_mut() {
            *cost = if *succ == Node::END {
                // Needs to cost nothing to go to the end node.
                // 0
                u32::MAX.into()
            } else {
                (u32::MAX - u32::try_from(*cost).unwrap()).into()
            }
        }

        successors
    };
    let success = |&node: &Node| node == Node::END;

    let (best_path, cost) =
        pathfinding::directed::dijkstra::dijkstra(&start, successors, success).unwrap();

    println!();
    println!("=== Best Path ===");
    println!();
    let total_flow = print_detailed_path_and_get_flow(volcano, &best_path);

    Some(total_flow)
}

fn print_detailed_path_and_get_flow(volcano: &Volcano, path: &[Node]) -> u32 {
    let mut total_flow = 0;
    for &node in path {
        let minute = node.minutes_remaining;

        let name = |valve| {
            if let Some(valve) = valve {
                volcano.valve(valve).unwrap().name
            } else {
                ValveName('(', ')')
            }
        };
        let my_name = name(node.my_valve);
        let elephant_name = name(node.elephant_valve);

        let open_valves: Vec<String> = node
            .open_valves()
            .map(|id| volcano.valve(id).unwrap().name.to_string())
            .collect();

        let additional_flow = if node.minutes_remaining != 0 {
            node.flow_rate(volcano)
        } else {
            0
        };

        println!(
            "t={minute:>2}, f={total_flow:>4}, v0={my_name}, v1={elephant_name}, \
            {open_valves:?} -> +{additional_flow}"
        );

        total_flow += additional_flow;
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
