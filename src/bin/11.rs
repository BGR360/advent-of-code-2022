#![doc = include_str!("../puzzles/11.md")]

use std::{fmt, ops};

use advent_of_code::{debug, debugln, helpers::Itertools};
use num_modular::ModularInteger;
use smallvec::SmallVec;

type ModInt = num_modular::ReducedInt<u32, num_modular::Vanilla<u32>>;

/// An integer that falls back to modular arithmetic with a fixed modulus if it
/// gets too large.
#[derive(Clone)]
struct BigInt {
    /// The value of the integer, stored as multiple equivalent reduced
    /// integers, one for each modulus.
    reduced: SmallVec<[ModInt; 8]>,

    /// Best-effort attempt to store the full value of the integer.
    /// Will be `None` if the value ever overflows.
    full: Option<u32>,
}

impl BigInt {
    /// Returns a new [`BigInt`] with the given value.
    ///
    /// The moduli should include all dividends that you intend to call
    /// [`is_divisible_by`] with.
    pub fn new(value: u32, moduli: &[u32]) -> Self {
        Self {
            reduced: moduli
                .iter()
                .map(|modulus| ModInt::new(value, modulus))
                .collect(),
            full: Some(value),
        }
    }

    /// Returns true if the value is divisible by a given number.
    ///
    /// This function will panic if the value has overflowed the inner u32 and
    /// the dividend is not one of the moduli used to construct this integer.
    #[track_caller]
    #[inline]
    pub fn is_divisible_by(&self, dividend: u32) -> bool {
        if let Some(full) = self.full {
            full % dividend == 0
        } else {
            let value = self
                .reduced
                .iter()
                .find(|&value| value.modulus() == dividend)
                .expect("the dividend must be one of the moduli used to construct the integer");
            value.residue() == 0
        }
    }

    #[inline]
    pub fn square(&mut self) {
        self.map(|x| x.checked_mul(x), |x| x.square());
    }

    #[inline]
    fn map(
        &mut self,
        full_op: impl FnOnce(u32) -> Option<u32>,
        reduced_op: impl Fn(ModInt) -> ModInt,
    ) {
        if let Some(full) = self.full {
            if let Some(mapped) = full_op(full) {
                self.full = Some(mapped);
                return;
            } else {
                self.overflow();
            }
        }
        debug_assert!(self.full.is_none());

        for reduced in self.reduced.iter_mut() {
            *reduced = reduced_op(*reduced);
        }
    }

    fn overflow(&mut self) {
        let full = self.full.unwrap();
        for reduced in self.reduced.iter_mut() {
            *reduced = reduced.convert(full);
        }
        self.full = None;
    }
}

impl ops::AddAssign<u32> for BigInt {
    fn add_assign(&mut self, rhs: u32) {
        self.map(|x| x.checked_add(rhs), |x| x + x.convert(rhs));
    }
}

impl ops::MulAssign<u32> for BigInt {
    fn mul_assign(&mut self, rhs: u32) {
        self.map(|x| x.checked_mul(rhs), |x| x * x.convert(rhs));
    }
}

impl ops::DivAssign<u32> for BigInt {
    fn div_assign(&mut self, rhs: u32) {
        self.map(|x| x.checked_div(rhs), |x| x / x.convert(rhs));
    }
}

struct ModIntDisplay(ModInt);

impl fmt::Debug for ModIntDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for ModIntDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (mod {})", self.0.residue(), self.0.modulus())
    }
}

impl fmt::Debug for BigInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("BigInt");
        if let Some(full) = self.full {
            s.field("full", &full);
        } else {
            let reduced: Vec<_> = self
                .reduced
                .iter()
                .map(|&reduced| ModIntDisplay(reduced))
                .collect();
            s.field("reduced", &reduced);
        }
        s.finish()
    }
}

impl fmt::Display for BigInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(full) = self.full {
            write!(f, "{full}")
        } else {
            write!(f, "{}", ModIntDisplay(self.reduced[0]))
        }
    }
}

/// An item with a "worry level".
#[derive(Debug, Clone)]
struct Item(pub BigInt);

impl Item {
    /// Returns a new [`Item`] with the given "worry level".
    ///
    /// The moduli should include the dividends of each monkey.
    pub fn new(worry_level: u32, monkey_moduli: &[u32]) -> Self {
        Self(BigInt::new(worry_level, monkey_moduli))
    }

    pub fn reduce_by_boredom_factor(&mut self) {
        self.0 /= 3;
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
    pub operation: fn(&mut BigInt),
    pub modulus: u32,
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
    pub fn items(&self) -> impl Iterator<Item = &Item> {
        self.items.iter()
    }

    pub fn throw_items(
        &mut self,
        boredom_relief: bool,
    ) -> impl Iterator<Item = (Item, MonkeyId)> + '_ {
        let items = std::mem::take(&mut self.items);

        let this = &*self;
        items.into_iter().map(move |mut item| {
            debugln!("  Monkey inspects an item with a worry level of {item}.");
            this.adjust_worry(&mut item, boredom_relief);
            let recipient = this.choose_recipient(&item);
            (item, recipient)
        })
    }

    pub fn catch_item(&mut self, item: Item) {
        self.items.push(item);
    }

    fn adjust_worry(&self, item: &mut Item, boredom_relief: bool) {
        (self.operation)(&mut item.0);
        debugln!("    Worry level increases to {item}");
        if boredom_relief {
            item.reduce_by_boredom_factor();
            debugln!("    Monkey gets bored with item. Worry level is divided by 3 to {item}");
        }
    }

    fn choose_recipient(&self, item: &Item) -> MonkeyId {
        let dividend = self.modulus;
        let divisible = item.0.is_divisible_by(dividend);
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

    max_two_inspect_counts[0] * max_two_inspect_counts[1]
}

fn part_one(monkeys: &[Monkey]) -> Option<usize> {
    const NUM_ROUNDS: usize = 20;
    const BOREDOM_RELIEF: bool = true;

    Some(solve(monkeys, NUM_ROUNDS, BOREDOM_RELIEF))
}

fn part_two(monkeys: &[Monkey]) -> Option<usize> {
    // FIXME: need to handle arbitrarily large integers
    const NUM_ROUNDS: usize = 10_000;
    // const NUM_ROUNDS: usize = 5;
    const BOREDOM_RELIEF: bool = false;

    Some(solve(monkeys, NUM_ROUNDS, BOREDOM_RELIEF))
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
    ),*) => {{
        let monkey_moduli = vec![
            $(
                $divisible_by
            ),*
        ];
        vec![
            $(
                Monkey {
                    items: vec![$(Item::new($item, &monkey_moduli)),*],
                    operation: |old: &mut BigInt| $operation(old),
                    modulus: $divisible_by,
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
    }};
}

fn main() {
    let monkeys = include!("../inputs/11.rs");
    advent_of_code::solve!(1, part_one, &monkeys);
    advent_of_code::solve!(2, part_two, &monkeys);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_monkeys() -> Vec<Monkey> {
        monkeys![
            Monkey 0 {
                Starting items: [79, 98],
                Operation: |old: &mut BigInt| *old *= 19,
                Test: divisible by 23 => {
                    If true: throw to monkey 2,
                    If false: throw to monkey 3,
                },
            },
            Monkey 1 {
                Starting items: [54, 65, 75, 74],
                Operation: |old: &mut BigInt| *old += 6,
                Test: divisible by 19 => {
                    If true: throw to monkey 2,
                    If false: throw to monkey 0,
                },
            },
            Monkey 2 {
                Starting items: [79, 60, 97],
                Operation: |old: &mut BigInt| old.square(),
                Test: divisible by 13 => {
                    If true: throw to monkey 1,
                    If false: throw to monkey 3,
                },
            },
            Monkey 3 {
                Starting items: [74],
                Operation: |old: &mut BigInt| *old += 3,
                Test: divisible by 17 => {
                    If true: throw to monkey 0,
                    If false: throw to monkey 1,
                },
            }
        ]
    }

    #[test]
    fn test_part_one() {
        assert_eq!(part_one(&example_monkeys()), Some(10605));
    }

    #[test]
    fn test_part_two() {
        assert_eq!(part_two(&example_monkeys()), Some(2713310158));
    }
}
