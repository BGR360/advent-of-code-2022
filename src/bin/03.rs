#![doc = include_str!("../puzzles/03.md")]

use std::fmt;

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

impl Item {
    const ALPHABET_SIZE: u8 = 26;

    const LOWERCASE_PRIORITY: u8 = 1;
    const UPPERCASE_PRIORITY: u8 = Self::LOWERCASE_PRIORITY + Self::ALPHABET_SIZE;

    const MAX_PRIORITY: u8 = Self::UPPERCASE_PRIORITY + Self::ALPHABET_SIZE;

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
        f.debug_tuple("Item")
            .field(&format!("'{c}' ({p})"))
            .finish()
    }
}

#[derive(Clone, Copy)]
struct Rucksack<'a> {
    compartments: [&'a str; 2],
}

impl<'a> TryFrom<&'a str> for Rucksack<'a> {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let len = value.len();
        if len % 2 != 0 {
            return Err(());
        }
        if value.chars().any(|c| Item::try_from(c).is_err()) {
            return Err(());
        }
        let half = len / 2;
        Ok(Self {
            compartments: [&value[..half], &value[half..]],
        })
    }
}

impl Rucksack<'_> {
    pub fn duplicate_item(&self) -> Option<Item> {
        let mut first_compartment_items = [false; Item::MAX_PRIORITY as usize];

        for item in Self::items(self.compartments[0]) {
            let index = item.priority() as usize;
            first_compartment_items[index] = true;
        }

        for item in Self::items(self.compartments[1]) {
            let index = item.priority() as usize;
            if first_compartment_items[index] {
                return Some(item);
            }
        }

        None
    }

    fn items(compartment: &str) -> impl Iterator<Item = Item> + '_ {
        compartment
            .chars()
            .map(|c| Item::try_from(c).expect("string should have been validated in TryFrom"))
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
    None
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
        assert_eq!(part_two(&input), None);
    }
}
