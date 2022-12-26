#![doc = include_str!("../puzzles/03.md")]

use std::{fmt, ops};

use advent_of_code::debugln;
use itertools::Itertools;

#[derive(Clone, Copy, PartialEq, Eq)]
struct Item {
    priority: u8,
}

impl TryFrom<char> for Item {
    type Error = ();

    #[inline]
    #[allow(non_upper_case_globals)]
    fn try_from(value: char) -> Result<Self, Self::Error> {
        const a: u32 = Item::LOWERCASE_A as u32;
        const z: u32 = Item::LOWERCASE_Z as u32;
        const A: u32 = Item::UPPERCASE_A as u32;
        const Z: u32 = Item::UPPERCASE_Z as u32;

        let value = value.into();
        let priority: u32 = match value {
            a..=z => (value - a) + Self::LOWERCASE_PRIORITY as u32,
            A..=Z => (value - A) + Self::UPPERCASE_PRIORITY as u32,
            _ => return Err(()),
        };

        Ok(Self {
            priority: priority.try_into().unwrap(),
        })
    }
}

impl TryFrom<u8> for Item {
    type Error = ();

    fn try_from(priority: u8) -> Result<Self, Self::Error> {
        match priority {
            Self::MIN_PRIORITY..=Self::MAX_PRIORITY => Ok(Self { priority }),
            _ => Err(()),
        }
    }
}

impl Item {
    const ALPHABET_SIZE: u8 = 26;

    const LOWERCASE_PRIORITY: u8 = 1;
    const UPPERCASE_PRIORITY: u8 = Self::LOWERCASE_PRIORITY + Self::ALPHABET_SIZE;

    const MIN_PRIORITY: u8 = Self::LOWERCASE_PRIORITY;
    const MAX_PRIORITY: u8 = Self::UPPERCASE_PRIORITY + Self::ALPHABET_SIZE - 1;

    const LOWERCASE_A: u8 = b'a';
    const LOWERCASE_Z: u8 = b'z';
    const UPPERCASE_A: u8 = b'A';
    const UPPERCASE_Z: u8 = b'Z';

    #[inline]
    pub fn priority(&self) -> u8 {
        self.priority
    }

    #[inline]
    pub fn character(&self) -> char {
        let is_lowercase =
            (Self::LOWERCASE_PRIORITY..Self::UPPERCASE_PRIORITY).contains(&self.priority);

        if is_lowercase {
            (Self::LOWERCASE_A + (self.priority - Self::LOWERCASE_PRIORITY)) as char
        } else {
            (Self::UPPERCASE_A + (self.priority - Self::UPPERCASE_PRIORITY)) as char
        }
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = self.character();
        let p = self.priority();
        write!(f, "Item('{c}' {p})")
    }
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.character())
    }
}

#[derive(Debug, Clone, Copy)]
struct Compartment<'a>(&'a str);

impl<'a> TryFrom<&'a str> for Compartment<'a> {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.chars().any(|c| Item::try_from(c).is_err()) {
            return Err(());
        }
        Ok(Self(value))
    }
}

impl Compartment<'_> {
    pub fn items(&self) -> impl Iterator<Item = Item> + '_ {
        self.0
            .chars()
            .map(|c| Item::try_from(c).expect("string should have been validated in TryFrom"))
    }
}

#[derive(Debug, Clone, Copy)]
struct Rucksack<'a> {
    compartments: [Compartment<'a>; 2],
}

impl<'a> TryFrom<&'a str> for Rucksack<'a> {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let len = value.len();
        if len % 2 != 0 {
            return Err(());
        }

        let half = len / 2;
        Ok(Self {
            compartments: [
                Compartment::try_from(&value[..half])?,
                Compartment::try_from(&value[half..])?,
            ],
        })
    }
}

impl<'a> Rucksack<'a> {
    pub fn compartments(&self) -> &[Compartment<'a>; 2] {
        &self.compartments
    }

    pub fn duplicate_item(&self) -> Option<Item> {
        let fingerprints = self.compartments.map(Fingerprint::for_compartment);

        for ((item, first_count), (_, second_count)) in fingerprints[0]
            .item_counts()
            .zip(fingerprints[1].item_counts())
        {
            if first_count != 0 && second_count != 0 {
                return Some(item);
            }
        }

        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Fingerprint {
    item_counts: [u8; Self::SIZE],
}

impl Fingerprint {
    const SIZE: usize = (Item::MAX_PRIORITY + 1) as usize;

    pub fn for_compartment(compartment: Compartment<'_>) -> Self {
        let mut item_counts = [0u8; Self::SIZE];
        for item in compartment.items() {
            let index = item.priority() as usize;
            item_counts[index] += 1;
        }

        Self { item_counts }
    }

    pub fn for_rucksack(rucksack: Rucksack<'_>) -> Self {
        Self::for_compartment(rucksack.compartments()[0])
            + Self::for_compartment(rucksack.compartments()[1])
    }

    pub fn item_counts(self) -> impl Iterator<Item = (Item, u8)> {
        self.item_counts
            .into_iter()
            .enumerate()
            .skip_while(|&(index, _)| index < Item::MIN_PRIORITY as usize)
            .map(|(index, count)| {
                //debugln!("index = {index}");
                let item = Item::try_from(index as u8).unwrap();
                (item, count)
            })
    }

    pub fn non_zero_item_counts(self) -> impl Iterator<Item = (Item, u8)> {
        self.item_counts().filter(|&(_item, count)| count > 0)
    }
}

impl ops::Add for Fingerprint {
    type Output = Self;

    #[inline]
    fn add(mut self, rhs: Self) -> Self::Output {
        for i in 0..Self::SIZE {
            self.item_counts[i] += rhs.item_counts[i];
        }
        self
    }
}

impl ops::BitAnd for Fingerprint {
    type Output = Self;

    #[inline]
    fn bitand(mut self, rhs: Self) -> Self::Output {
        for i in 0..Self::SIZE {
            self.item_counts[i] = ((self.item_counts[i] != 0) & (rhs.item_counts[i] != 0)).into();
        }
        self
    }
}

impl fmt::Debug for Fingerprint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct AsMap<'a>(&'a Fingerprint);

        impl fmt::Debug for AsMap<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_map()
                    .entries(
                        self.0
                            .non_zero_item_counts()
                            .map(|(item, count)| (item.to_string(), count)),
                    )
                    .finish()
            }
        }

        f.debug_struct("Fingerprint")
            .field("item_counts", &AsMap(self))
            .finish()
    }
}

#[derive(Debug, Clone, Copy)]
struct Group<'a> {
    badge: Item,
    _rucksacks: [Rucksack<'a>; 3],
}

impl<'a> Group<'a> {
    pub fn new(rucksacks: [Rucksack<'a>; 3]) -> Self {
        let fingerprints = rucksacks.map(Fingerprint::for_rucksack);
        debugln!("fingerprints: {fingerprints:#?}");

        let combined_fingerprint = fingerprints.into_iter().reduce(|f1, f2| f1 & f2).unwrap();
        debugln!("combined fingerprint: {combined_fingerprint:#?}");

        let common_items = combined_fingerprint
            .non_zero_item_counts()
            .map(|(item, _count)| item);

        let Ok(badge) = common_items.exactly_one() else {
            unreachable!("should be exactly one common item");
        };

        Self {
            badge,
            _rucksacks: rucksacks,
        }
    }

    pub fn badge(&self) -> Item {
        self.badge
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let sum = input
        .lines()
        .map(|line| {
            let rucksack = Rucksack::try_from(line).unwrap();
            let item = rucksack.duplicate_item().unwrap();
            item.priority() as u32
        })
        .sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u32> {
    let line_groups = input.lines().chunks(3);

    let sum = (&line_groups)
        .into_iter()
        .map(|lines| {
            let lines: [&str; 3] = lines.collect::<Vec<_>>().try_into().unwrap();
            debugln!("group: {lines:?}");

            let rucksacks: [Rucksack<'_>; 3] = lines.map(|line| Rucksack::try_from(line).unwrap());
            let group = Group::new(rucksacks);
            let badge = group.badge();

            badge.priority() as u32
        })
        .sum();

    Some(sum)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 3);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_item() {
        #[track_caller]
        fn do_it(character: char, priority: u8) {
            let item = Item::try_from(character).unwrap();
            assert_eq!(item.priority(), priority);
            assert_eq!(item.character(), character);
        }

        do_it('a', 1);
        do_it('b', 2);
        do_it('z', 26);
        do_it('A', 27);
        do_it('B', 28);
        do_it('Z', 52);
    }

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_one(&input), Some(157));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_two(&input), Some(70));
    }
}
