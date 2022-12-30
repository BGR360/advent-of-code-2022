#![doc = include_str!("../puzzles/16.md")]

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    io::Write,
};

use advent_of_code::debugln;
use bitvec::BitArr;
use itertools::Itertools;
use rayon::prelude::*;

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

fn get_max_total_flow_for_valves(
    volcano: &Volcano,
    distances: &Distances,
    valves: &[ValveId],
    mut minutes: u32,
    logging: bool,
) -> u32 {
    let mut total_flow = 0;
    let mut flow_rate = 0;

    let start = volcano.name_to_id(ValveName::AA).unwrap();
    let valves_including_start = std::iter::once(start).chain(valves.iter().copied());

    for (valve_a, valve_b) in valves_including_start.tuple_windows() {
        let minutes_a_to_b = distances.get_between(valve_a, valve_b).unwrap();
        // Add one minute to turn on the valve.
        let minutes_a_to_b = minutes_a_to_b + 1;
        // Don't exceed the remaining minutes.
        let minutes_a_to_b = minutes_a_to_b.min(minutes);

        if logging {
            debugln!("{minutes}: valve {valve_a} -> open valve {valve_b}");
            debugln!(
                "    minutes -= {minutes_a_to_b}\t\t// {} -> {}",
                minutes,
                minutes - minutes_a_to_b
            );
            debugln!(
                "    total_flow += {flow_rate} * {minutes_a_to_b} \t// {} -> {}",
                total_flow,
                total_flow + flow_rate * minutes_a_to_b
            );
        }

        // Accumulate flow on the way to valve_b
        total_flow += flow_rate * minutes_a_to_b;

        minutes -= minutes_a_to_b;
        if minutes == 0 {
            break;
        }

        // Turn on valve_b.
        let additional_flow = volcano.valve(valve_b).unwrap().flow_rate;
        if logging {
            debugln!(
                "    flow_rate += {additional_flow}\t\t// {} -> {}",
                flow_rate,
                flow_rate + additional_flow
            );
        }
        flow_rate += additional_flow;
    }

    // Accumulate flow for any leftover minutes.
    total_flow += flow_rate * minutes;

    debugln!("{valves:?}: {total_flow}");

    total_flow
}

fn valve_ids_to_strings(volcano: &Volcano, ids: &[ValveId]) -> Vec<String> {
    ids.iter()
        .map(|&id| volcano.valve(id).unwrap().name.to_string())
        .collect()
}

fn get_max_pressure_released_for_limited_sequence(
    volcano: &Volcano,
    non_zero_valves: &[ValveId],
    distances: &Distances,
    minutes: u32,
    sequence_len: usize,
) -> u32 {
    // let (best_sequence, max_total_flow) = non_zero_valves
    //     .iter()
    //     .copied()
    //     .permutations(sequence_len)
    //     .par_bridge()
    //     .map(|valves| {
    //         let total_flow = get_max_total_flow_for_valves(
    //             volcano, &distances, &valves, minutes, false, /* logging */
    //         );
    //         (valves, total_flow)
    //     })
    //     .max_by_key(|&(_, flow)| flow)
    //     .unwrap();

    // println!();
    // println!(
    //     "Best sequence: {:?}. Total flow: {max_total_flow}",
    //     valve_ids_to_strings(volcano, &best_sequence)
    // );
    // println!();

    // get_max_total_flow_for_valves(
    //     volcano,
    //     &distances,
    //     &best_sequence,
    //     minutes,
    //     true, /* logging */
    // );

    // max_total_flow

    let count = non_zero_valves
        .iter()
        .copied()
        .permutations(sequence_len)
        // .par_bridge()
        .map(|valves| {
            debugln!("{valves:?}");
            //print!(".");
        })
        .count();

    println!("{count}");

    count.try_into().unwrap()
}

fn get_max_pressure_released(volcano: &Volcano, minutes: u32) -> Option<u32> {
    let distances = Distances::for_volcano(volcano);
    println!("Distances: {distances:?}");

    let non_zero_valves: Vec<ValveId> = volcano
        .valve_ids()
        .filter(|&id| volcano.valve(id).unwrap().flow_rate != 0)
        .collect();

    println!(
        "Non-zero valves: {:?}",
        valve_ids_to_strings(volcano, &non_zero_valves)
    );

    std::io::stdout().flush().unwrap();

    let mut max_total_flow = 0;
    for sequence_len in 1..=non_zero_valves.len() {
        println!();
        println!("==== Trying with sequences of {sequence_len} valves ====");

        let best_flow_for_length = get_max_pressure_released_for_limited_sequence(
            volcano,
            &non_zero_valves,
            &distances,
            minutes,
            sequence_len,
        );

        if best_flow_for_length == max_total_flow {
            break;
        }
        if best_flow_for_length > max_total_flow {
            max_total_flow = best_flow_for_length;
        }
    }

    // Some(max_total_flow)

    // get_max_pressure_released_for_limited_sequence(
    //     volcano,
    //     &non_zero_valves,
    //     &distances,
    //     minutes,
    //     non_zero_valves.len(),
    // );

    None
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
    let input = input();
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
