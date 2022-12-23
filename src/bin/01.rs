#![doc = include_str!("../puzzles/01.md")]

use advent_of_code::helpers::Itertools as _;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Elf {
    total_calories: u32,
}

impl Elf {
    fn from_lines<'a>(lines: impl IntoIterator<Item = &'a str>) -> Self {
        Self {
            total_calories: lines
                .into_iter()
                .map(|line| line.parse::<u32>().unwrap())
                .sum(),
        }
    }
}

/// Returns the total calories carried by the elf with the highest sum of
/// calories in their snacks.
pub fn part_one(input: &str) -> Option<u32> {
    let line_groups = input.lines().split_by(|line| line.is_empty());
    (&line_groups)
        .into_iter()
        .map(Elf::from_lines)
        .max()
        .map(|elf| elf.total_calories)
}

/// Returns the total calories carried by the three elves with the highest sum
/// of calories in their snacks.
pub fn part_two(input: &str) -> Option<u32> {
    let line_groups = input.lines().split_by(|line| line.is_empty());
    let max_elves = (&line_groups).into_iter().map(Elf::from_lines).max_n(3);

    Some(max_elves.into_iter().map(|elf| elf.total_calories).sum())
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 1);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(24000));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_two(&input), Some(45000));
    }
}
