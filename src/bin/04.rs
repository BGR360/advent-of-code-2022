#![doc = include_str!("../puzzles/04.md")]

use std::{fmt, ops::RangeInclusive};

use advent_of_code::{debugln, helpers::parse};

#[derive(Debug, Clone)]
struct Assignment {
    range: RangeInclusive<u16>,
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.range.start(), self.range.end())
    }
}

#[derive(Debug, Clone)]
struct Pair {
    assignments: [Assignment; 2],
}

impl fmt::Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{},{}", self.assignments[0], self.assignments[1])
    }
}

impl Assignment {
    pub fn intersects(&self, other: &Self) -> bool {
        let a = &self.range;
        let b = &other.range;
        a.start() <= b.end() && a.end() >= b.start()
    }

    pub fn completely_contains(&self, other: &Self) -> bool {
        let a = &self.range;
        let b = &other.range;
        a.start() <= b.start() && a.end() >= b.end()
    }
}

impl Pair {
    pub fn one_completely_contains_the_other(&self) -> bool {
        let a = &self.assignments[0];
        let b = &self.assignments[1];
        a.completely_contains(b) || b.completely_contains(a)
    }

    pub fn one_overlaps_the_other(&self) -> bool {
        self.assignments[0].intersects(&self.assignments[1])
    }
}

mod parsing {
    use super::*;

    use combine as c;

    use c::{parser::combinator::StrLike, stream::Range, ParseError, Parser, RangeStream};

    impl Assignment {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: RangeStream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
            Input::Range: Range,
            Input::Range: StrLike,
        {
            let integer =
                || c::from_str(c::parser::range::take_while1(|c: char| c.is_ascii_digit()));

            let assignment = (integer(), c::token('-'), integer());

            assignment.map(|(start, _, end)| {
                let range = start..=end;
                Assignment { range }
            })
        }
    }

    impl Pair {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: RangeStream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
            Input::Range: Range,
            Input::Range: StrLike,
        {
            let pair = (Assignment::parser(), c::token(','), Assignment::parser());

            pair.map(|(a, _, b)| Pair {
                assignments: [a, b],
            })
        }
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let count = input
        .lines()
        .map(|line| parse::from_str(line, Pair::parser()).unwrap())
        .filter(|pair| pair.one_completely_contains_the_other())
        .inspect(|pair| debugln!("completely overlapping pair: {pair}"))
        .count();
    Some(count.try_into().unwrap())
}

pub fn part_two(input: &str) -> Option<u32> {
    let count = input
        .lines()
        .map(|line| parse::from_str(line, Pair::parser()).unwrap())
        .filter(|pair| pair.one_overlaps_the_other())
        .inspect(|pair| debugln!("overlapping pair: {pair}"))
        .count();
    Some(count.try_into().unwrap())
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 4);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_one(&input), Some(2));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_two(&input), Some(4));
    }
}
