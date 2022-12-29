#![doc = include_str!("../puzzles/16.md")]

use std::{collections::HashMap, ops};

use advent_of_code::debugln;
use bitvec::BitArr;

type BitArray = BitArr!(for 64, in u64);

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

/// Memoized data for the dynamic programming solution.
///
/// This puzzle can be viewed as a variation of the [0-1 Knapsack
/// Problem][knapsack].
///
/// In 0-1 Knapsack, there are `N` items with values `v[0..N]` and weights
/// `w[0..N]`, and a knapsack with capacity `W`. The goal is to fit as many
/// items into the knapsack as you can while maximizing their combined value.
///
/// In this puzzle, the "items" are the valves, and the "knapsack" is the 30
/// minutes we have to spend until the volcano blows. Traveling between valves
/// and opening them is the "weight" that takes away from our 30 minutes. The
/// flow that each valve provides is its "value" that we are trying to maximize.
///
/// ### Differences to 0-1 Knapsack
///
///
///
/// ## The Regular 0-1 Knapsack DP Solution
///
/// The dynamic programming solution for 0-1 Knapsack works as follows:
///
/// Let `m[0..N, 0..W]` be the memoized data, where `m[i, w]` is the maximum value
/// that can be attained with weight less than or equal to `w` using only items
/// `v[0..i]` (the first `i` items):
///
/// ```txt
///
/// ```
///
/// ## How This Solution Works
///
/// This solution operates on the same principle.
///
/// [knapsack]: <https://en.wikipedia.org/wiki/Knapsack_problem>
#[derive(Debug)]
struct Memo<'a> {
    pub volcano: &'a Volcano,
    pub valves: ValveVec<ValveMemo>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MemoIndex {
    pub valve: ValveId,
    pub minute: usize,
    pub range: ValveId,
}

impl<'a> Memo<'a> {
    pub fn new(volcano: &'a Volcano) -> Self {
        let mut valves = ValveVec::new();
        valves.resize(volcano.valve_count(), ValveMemo::default());

        Memo { volcano, valves }
    }

    #[inline]
    pub fn get_entry(&self, index: MemoIndex) -> Option<MemoEntry> {
        let MemoIndex {
            valve,
            minute,
            range,
        } = index;
        self.valves
            .get(valve)?
            .minutes
            .get(minute - 1)?
            .ranges
            .get(range)
            .copied()
    }
}

impl Memo<'_> {
    pub fn display(&self) -> self::formatting::MemoPrinter<'_> {
        formatting::MemoPrinter::new(self)
    }
}

/// Per-valve memoized data.
#[derive(Debug, Default, Clone)]
struct ValveMemo {
    /// Index 0 is the flow that can be attained after 1 minute when starting at
    /// this valve.
    pub minutes: Vec<MinuteMemo>,
}

impl ValveMemo {
    pub fn display<'a>(&'a self, volcano: &'a Volcano) -> self::formatting::ValveMemoPrinter<'a> {
        formatting::ValveMemoPrinter::new(volcano, self)
    }
}

#[derive(Debug, Clone)]
struct MinuteMemo {
    /// Flow that can be attained when you're only allowed to use valves
    /// `0..=i`.
    ///
    /// For example, index 0 is the flow that can be attained when only opening
    /// valve 0, index 1 is what can be attained when you can open only valves 0
    /// and 1, and the last index is what can be attained when you can open all
    /// valves.
    pub ranges: ValveVec<MemoEntry>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MemoEntry {
    pub total_flow: u32,
    pub prev: MemoIndex,
}

impl PartialOrd for MemoEntry {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.total_flow.partial_cmp(&other.total_flow)
    }
}

impl Ord for MemoEntry {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.total_flow.cmp(&other.total_flow)
    }
}

/// Minute must be at least 1.
/// The memo must already be filled out for minutes less than `minute` for all valves.
fn compute_minute_for_valve(memo: &Memo, valve: ValveId, minute: usize) -> MinuteMemo {
    debugln!("Computing minute {minute} for valve {valve}");

    let mut ranges = ValveVec::new();

    let find_best_neighbor = |max_allowed_valve: ValveId, minute: usize| -> (ValveId, u32) {
        let neighbors = &memo.volcano.valve(valve).unwrap().connections;

        let (best_valve, best_flow) = neighbors
            .iter()
            .filter(|&&id| id <= max_allowed_valve)
            .map(|&id| {
                let index = MemoIndex {
                    valve: id,
                    minute,
                    range: max_allowed_valve,
                };
                //debugln!("index: {index:?}");
                (id, memo.get_entry(index).unwrap().total_flow)
            })
            .max_by_key(|&(_id, flow)| flow)
            .unwrap_or((valve, 0));

        debugln!("candidate: valve={best_valve}, flow={best_flow}");

        (best_valve, best_flow)
    };

    // Option 1: We don't turn on this valve during this minute; do the best we
    // can with its neighbors.
    let option_1 = |max_allowed_valve| -> MemoEntry {
        let mut entry = MemoEntry {
            total_flow: 0,
            prev: MemoIndex {
                valve,
                // Subtract one minute for the time it takes to travel to the neighbor.
                minute: minute - 1,
                range: max_allowed_valve,
            },
        };
        if entry.prev.minute == 0 {
            return entry;
        }
        // if entry.prev.range == 0 {
        //     return entry;
        // }
        // entry.prev.range -= 1;

        (entry.prev.valve, entry.total_flow) =
            find_best_neighbor(entry.prev.range, entry.prev.minute);

        entry
    };

    // Option 2: We turn on this valve during this minute.
    let option_2 = |max_allowed_valve| -> MemoEntry {
        let mut index = MemoIndex {
            valve,
            // Subtract one minute for the time it takes to turn on this valve.
            minute: minute - 1,
            range: max_allowed_valve,
        };

        let entry = |total_flow, index| MemoEntry {
            total_flow,
            prev: index,
        };

        if valve > max_allowed_valve {
            return entry(0, index);
        }
        if index.minute == 0 {
            return entry(0, index);
        }

        // Calculate how much flow would result from this valve being turned on
        // during this minute.
        let flow_rate = memo.volcano.valve(valve).unwrap().flow_rate;
        let this_flow = flow_rate * u32::try_from(index.minute).unwrap();

        // Subtract one minute for the time it takes to visit a neighbor.
        index.minute -= 1;
        if index.minute == 0 {
            return entry(this_flow, index);
        }

        // See how much more flow we can get by visiting a neighbor, excluding
        // this valve.
        // if index.range == 0 {
        //     return entry(this_flow, index);
        // }
        if valve == 0 {
            return entry(this_flow, index);
        }
        index.range = valve - 1;
        let (neighbor, neighbor_flow) = find_best_neighbor(index.range, index.minute);

        index.valve = neighbor;

        entry(this_flow + neighbor_flow, index)
    };

    for i in 0..memo.valves.len() {
        let max_allowed_valve = ValveId::from(i);

        let option1 = option_1(max_allowed_valve);
        let option2 = option_2(max_allowed_valve);

        let best = if option1.total_flow >= option2.total_flow {
            option1
        } else {
            option2
        };

        ranges.push(best);
    }

    MinuteMemo { ranges }
}

fn get_max_pressure_released(volcano: &Volcano, minutes: usize) -> Option<u32> {
    let mut memo = Memo::new(volcano);

    for minute in 1..=minutes {
        for valve in 0..volcano.valve_count() {
            let valve = ValveId::from(valve);
            let minute_memo = compute_minute_for_valve(&memo, valve, minute);
            memo.valves[valve].minutes.push(minute_memo);
        }

        if minute % 5 == 0 {
            debugln!("{}", memo.display());
        }
    }

    let aa_memo = &memo.valves[volcano.name_to_id(ValveName::AA).unwrap()];
    let last_minute = aa_memo.minutes.last().unwrap();
    let best_flow = last_minute.ranges.last().unwrap().total_flow;

    debugln!("Best flow: {best_flow}");

    // Reconstruct the steps taken.

    #[derive(Debug)]
    struct Step {
        current: ValveName,
        turn_it_on: bool,
    }

    let mut steps = Vec::new();
    let mut index = MemoIndex {
        valve: volcano.name_to_id(ValveName::AA).unwrap(),
        minute: minutes,
        range: ValveId::from(volcano.valve_count() - 1),
    };
    loop {
        let entry = memo.get_entry(index).unwrap();

        let minute_diff = index.minute - entry.prev.minute;
        let step = Step {
            current: volcano.valve(index.valve).unwrap().name,
            turn_it_on: minute_diff == 2,
        };
        steps.push(step);

        index = entry.prev;
        if index.minute == 0 {
            break;
        }
    }

    debugln!("Steps: {steps:#?}");

    Some(best_flow)
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
        assert_eq!(
            get_max_pressure_released(&input, 7),
            Some(1 * 3 + 2 * 13 + 5 * 20)
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

    pub(super) struct MemoPrinter<'a> {
        memo: &'a Memo<'a>,
    }

    impl<'a> MemoPrinter<'a> {
        pub fn new(memo: &'a Memo<'a>) -> Self {
            Self { memo }
        }
    }

    impl Display for MemoPrinter<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            writeln!(f)?;
            writeln!(f, "==================================================")?;
            writeln!(f, "{:^50}", "MEMO")?;
            writeln!(f, "==================================================")?;

            for i in 0..self.memo.volcano.valve_count() {
                let valve = ValveId::from(i);
                let valve_memo = &self.memo.valves[valve];
                let valve_name = self.memo.volcano.valve(valve).unwrap().name;

                writeln!(f)?;
                writeln!(f, "=== Valve {valve_name} ===")?;
                writeln!(f)?;
                writeln!(f, "{}", valve_memo.display(self.memo.volcano))?;
            }

            Ok(())
        }
    }

    pub(super) struct ValveMemoPrinter<'a> {
        volcano: &'a Volcano,
        memo: &'a ValveMemo,
    }

    impl<'a> ValveMemoPrinter<'a> {
        pub fn new(volcano: &'a Volcano, memo: &'a ValveMemo) -> Self {
            Self { volcano, memo }
        }
    }

    impl Display for ValveMemoPrinter<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            use prettytable::Table;

            let mut table = Table::new();

            let n_minutes = self.memo.minutes.len();

            let minutes = (1..=n_minutes).map(|min| format!("{min}m"));
            table.set_titles(
                std::iter::once(String::from("Range:"))
                    .chain(minutes)
                    .collect(),
            );

            let n_valves = self.volcano.valve_count();
            let min_id = ValveId::from(0);

            let get_valve_name = |id| self.volcano.valve(id).unwrap().name;
            let get_valve_range = |max_id| {
                let first = get_valve_name(min_id);
                let last = get_valve_name(max_id);
                format!("{first}-{last}")
            };

            for i in 0..n_valves {
                let range_max = ValveId::from(i);
                let range = get_valve_range(range_max);

                let values = (0..n_minutes).map(|min| {
                    self.memo.minutes[min].ranges[range_max]
                        .total_flow
                        .to_string()
                });

                table.add_row(std::iter::once(range).chain(values).collect());
            }

            write!(f, "{table}")
        }
    }
}
