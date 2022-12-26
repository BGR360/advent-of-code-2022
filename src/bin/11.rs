#![doc = include_str!("../puzzles/11.md")]

use std::fmt;

use advent_of_code::{debug, debugln, helpers::Itertools};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Item(pub u64);

impl Item {
    pub fn reduce_by_boredom_factor(self) -> Self {
        Self(self.0 / 3)
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MonkeyId(pub usize);

impl fmt::Display for MonkeyId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
struct Monkey {
    pub items: Vec<Item>,
    pub operation: fn(Item) -> Item,
    pub divisible_test: u32,
    pub choose_monkey: fn(bool) -> MonkeyId,
}

impl fmt::Debug for Monkey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Monkey")
            .field("items", &self.items)
            .finish()
    }
}

impl Monkey {
    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        self.items.iter().copied()
    }

    pub fn throw_items(
        &mut self,
        boredom_relief: bool,
    ) -> impl Iterator<Item = (Item, MonkeyId)> + '_ {
        let items = std::mem::take(&mut self.items);

        let this = &*self;
        items.into_iter().map(move |item| {
            debugln!("  Monkey inspects an item with a worry level of {item}.");
            let item = this.adjust_worry(item, boredom_relief);
            let recipient = this.choose_recipient(item);
            (item, recipient)
        })
    }

    pub fn catch_item(&mut self, item: Item) {
        self.items.push(item);
    }

    fn adjust_worry(&self, item: Item, boredom_relief: bool) -> Item {
        let item = (self.operation)(item);
        debugln!("    Worry level increases to {item}");
        let item = if boredom_relief {
            let reduced = item.reduce_by_boredom_factor();
            debugln!("    Monkey gets bored with item. Worry level is divided by 3 to {reduced}");
            reduced
        } else {
            item
        };
        item
    }

    fn choose_recipient(&self, item: Item) -> MonkeyId {
        let dividend = self.divisible_test;
        let divisible = item.0 % u64::from(dividend) == 0;
        debugln!(
            "    Current worry level is {}divisible by {dividend}.",
            if divisible { "" } else { "not " }
        );
        let recipient = (self.choose_monkey)(divisible);
        debugln!("    Item with worry level {item} is thrown to monkey {recipient}.");
        recipient
    }
}

fn do_round(
    round_number: usize,
    monkeys: &mut [Monkey],
    inspect_counts: &mut [usize],
    boredom_relief: bool,
) {
    debugln!();
    debugln!("Round {}:", round_number);
    debugln!();

    for monkey in 0..monkeys.len() {
        debugln!("Monkey {monkey}:");

        let counts = &mut inspect_counts[monkey];
        let monkey = &mut monkeys[monkey];
        let items: Vec<(Item, MonkeyId)> = monkey.throw_items(boredom_relief).collect();
        *counts += items.len();

        for (item, MonkeyId(recipient)) in items {
            let recipient = &mut monkeys[recipient];
            recipient.catch_item(item);
        }
    }

    debugln!();
    debugln!("Round {} summary:", round_number);
    debugln!();
    for (i, monkey) in monkeys.iter().enumerate() {
        debug!("Monkey {i}: ");
        let mut first = true;
        for item in monkey.items() {
            if !first {
                debug!(", ");
            }
            debug!("{item}");

            first = false;
        }
        debugln!();
    }
}

macro_rules! monkeys {
    ($(
        Monkey $id:literal {
            Starting items: [$($item:expr),*],
            Operation: $operation:expr,
            Test: divisible by $divisible_by:expr => {
                If true: throw to monkey $if_true:expr,
                If false: throw to monkey $if_false:expr,
            },
        }
    ),*) => {
        vec![
            $(
                Monkey {
                    items: vec![$(Item($item)),*],
                    operation: |Item(old)| Item($operation(old)),
                    divisible_test: $divisible_by,
                    choose_monkey: |divisible| {
                        if divisible {
                            MonkeyId($if_true)
                        } else {
                            MonkeyId($if_false)
                        }
                    },
                }
            ),*
        ]
    };
}

fn example_monkeys() -> Vec<Monkey> {
    monkeys![
        Monkey 0 {
            Starting items: [79, 98],
            Operation: |old| old * 19,
            Test: divisible by 23 => {
                If true: throw to monkey 2,
                If false: throw to monkey 3,
            },
        },
        Monkey 1 {
            Starting items: [54, 65, 75, 74],
            Operation: |old| old + 6,
            Test: divisible by 19 => {
                If true: throw to monkey 2,
                If false: throw to monkey 0,
            },
        },
        Monkey 2 {
            Starting items: [79, 60, 97],
            Operation: |old| old * old,
            Test: divisible by 13 => {
                If true: throw to monkey 1,
                If false: throw to monkey 3,
            },
        },
        Monkey 3 {
            Starting items: [74],
            Operation: |old| old + 3,
            Test: divisible by 17 => {
                If true: throw to monkey 0,
                If false: throw to monkey 1,
            },
        }
    ]
}

fn input_monkeys() -> Vec<Monkey> {
    include!("../inputs/11.rs")
}

fn solve(monkeys: &[Monkey], num_rounds: usize, boredom_relief: bool) -> usize {
    let mut monkeys: Vec<Monkey> = monkeys.to_vec();
    let mut inspect_counts: Vec<usize> = Vec::new();
    inspect_counts.resize(monkeys.len(), 0);
    for i in 0..num_rounds {
        do_round(
            i + 1,
            &mut monkeys[..],
            &mut inspect_counts[..],
            boredom_relief,
        );
    }

    let max_two_inspect_counts = inspect_counts.iter().max_n(2);

    let monkey_business_level = max_two_inspect_counts[0] * max_two_inspect_counts[1];
    monkey_business_level
}

fn part_one(monkeys: &[Monkey]) -> Option<usize> {
    const NUM_ROUNDS: usize = 20;
    const BOREDOM_RELIEF: bool = true;

    Some(solve(monkeys, NUM_ROUNDS, BOREDOM_RELIEF))
}

fn part_two(monkeys: &[Monkey]) -> Option<usize> {
    // FIXME: need to handle arbitrarily large integers
    // const NUM_ROUNDS: usize = 10_000;
    const NUM_ROUNDS: usize = 5;
    const BOREDOM_RELIEF: bool = false;

    Some(solve(monkeys, NUM_ROUNDS, BOREDOM_RELIEF))
}

fn main() {
    let input = input_monkeys();
    advent_of_code::solve!(1, part_one, &input);
    advent_of_code::solve!(2, part_two, &input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        assert_eq!(part_one(&example_monkeys()), Some(10605));
    }

    #[test]
    fn test_part_two() {
        assert_eq!(part_two(&example_monkeys()), Some(2713310158));
    }
}
