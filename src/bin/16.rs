#![doc = include_str!("../puzzles/16.md")]

use std::collections::HashMap;

use bitvec::BitArr;
use smallvec::SmallVec;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Node {
    pub valve: Option<ValveId>,
    pub open_valves: BitArray,
    pub minutes_remaining: u8,
}

impl Node {
    pub const END: Self = Self {
        valve: None,
        open_valves: BitArray::ZERO,
        minutes_remaining: 0,
    };

    pub fn successors(&self, volcano: &Volcano) -> SmallVec<[(Node, u64); 8]> {
        let Some(current_id) = self.valve else {
            return Default::default();
        };

        let mut successors = SmallVec::new();

        let current = volcano.valve(current_id).unwrap();
        let current_flow_rate = self.flow_rate(volcano);

        if let Some(remaining) = self.minutes_remaining.checked_sub(1) {
            for neighbor_id in current.connections.iter().copied() {
                // Travel to neighbor without opening this valve.
                let successor = Node {
                    valve: Some(neighbor_id),
                    open_valves: self.open_valves,
                    minutes_remaining: remaining,
                };
                successors.push((successor, current_flow_rate.into()))
            }

            // Open this valve without traveling to a neighbor, if it's worth
            // opening.
            if current.flow_rate != 0 {
                let mut open_valves = self.open_valves;
                open_valves.set(current_id.into(), true);
                let successor = Node {
                    valve: Some(current_id),
                    open_valves,
                    minutes_remaining: remaining,
                };
                successors.push((successor, current_flow_rate.into()));
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
}

fn valve_ids_to_strings(volcano: &Volcano, ids: &[ValveId]) -> Vec<String> {
    ids.iter()
        .map(|&id| volcano.valve(id).unwrap().name.to_string())
        .collect()
}

fn get_max_pressure_released(volcano: &Volcano, minutes: u8) -> Option<u32> {
    let start = Node {
        valve: Some(volcano.name_to_id(ValveName::AA).unwrap()),
        open_valves: BitArray::ZERO,
        minutes_remaining: minutes,
    };

    let successors = |node: &Node| -> SmallVec<[(Node, u64); 8]> {
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

    let (best_path, _inverted_cost) =
        pathfinding::directed::dijkstra::dijkstra(&start, successors, success).unwrap();

    println!();
    println!("=== Best Path ===");
    println!();

    let mut total_flow = 0;
    for &node in &best_path {
        if let Some(valve) = node.valve {
            let minute = node.minutes_remaining;
            let name = volcano.valve(valve).unwrap().name;

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
                "t={minute:>2}, f={total_flow:>4}, v={name}, {open_valves:?} -> +{additional_flow}"
            );

            total_flow += additional_flow;
        }
    }

    Some(total_flow)
}

fn part_one(volcano: &Volcano) -> Option<u32> {
    get_max_pressure_released(volcano, 30)
}

fn part_two(input: &Volcano) -> Option<u32> {
    None
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
