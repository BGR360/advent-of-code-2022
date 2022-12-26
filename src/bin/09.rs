#![doc = include_str!("../puzzles/09.md")]

use std::collections::HashSet;

use glam::IVec2;

use advent_of_code::helpers::parse;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl From<Direction> for IVec2 {
    fn from(value: Direction) -> Self {
        match value {
            Direction::Up => IVec2::Y,
            Direction::Right => IVec2::X,
            Direction::Down => IVec2::NEG_Y,
            Direction::Left => IVec2::NEG_X,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Motion {
    pub distance: i32,
    pub direction: Direction,
}

#[derive(Debug)]
struct World {
    head: IVec2,
    tail: IVec2,
    tail_visited: HashSet<IVec2>,
    lower_bound: IVec2,
    upper_bound: IVec2,
}

impl World {
    pub fn new() -> Self {
        const ZERO: IVec2 = IVec2::ZERO;

        let mut tail_visited = HashSet::new();
        tail_visited.insert(ZERO);

        Self {
            head: ZERO,
            tail: ZERO,
            tail_visited,
            lower_bound: ZERO,
            upper_bound: ZERO,
        }
    }

    #[inline]
    pub fn do_motion(&mut self, motion: Motion) {
        for _ in 0..motion.distance {
            self.move_head_one(motion.direction);
        }
    }

    #[inline]
    pub fn move_head_one(&mut self, dir: Direction) {
        let new_pos = self.head + IVec2::from(dir);
        self.move_head_to(new_pos);

        if !self.tail_is_touching_head() {
            self.move_tail_one();
        }

        debug_assert!(self.tail_is_touching_head());
    }

    #[inline]
    pub fn tail_is_touching_head(&self) -> bool {
        let tail_to_head = (self.head - self.tail).abs();
        tail_to_head.x <= 1 && tail_to_head.y <= 1
    }

    /// Returns the number of unique positions that the tail visited.
    pub fn tail_visited_positions(&self) -> usize {
        self.tail_visited.len()
    }

    #[inline]
    fn move_tail_one(&mut self) {
        let tail_to_head = (self.head - self.tail).clamp(IVec2::NEG_ONE, IVec2::ONE);
        let new_pos = self.tail + tail_to_head;
        self.move_tail_to(new_pos);
    }

    #[inline]
    fn move_head_to(&mut self, new_pos: IVec2) {
        self.head = new_pos;
        self.update_bounds(new_pos);
    }

    #[inline]
    fn move_tail_to(&mut self, new_pos: IVec2) {
        self.tail = new_pos;
        self.update_bounds(new_pos);
        self.tail_visited.insert(new_pos);
    }

    #[inline]
    fn update_bounds(&mut self, new_pos: IVec2) {
        self.lower_bound = self.lower_bound.min(new_pos);
        self.upper_bound = self.upper_bound.max(new_pos);
    }
}

fn motions(input: &str) -> impl Iterator<Item = Motion> + '_ {
    input
        .lines()
        .map(|line| parse::from_str(line, Motion::parser()).unwrap())
}

pub fn part_one(input: &str) -> Option<usize> {
    let mut world = World::new();
    for motion in motions(input) {
        world.do_motion(motion);
    }

    Some(world.tail_visited_positions())
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 9);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_one(&input), Some(13));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        assert_eq!(part_two(&input), None);
    }
}

mod parsing {
    use super::*;

    use advent_of_code::helpers::parse;

    mod c {
        pub use combine::{
            parser::char::{self, string},
            *,
        };
    }

    use c::{ParseError, Parser, Stream};

    impl Direction {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            c::one_of("DLRU".chars()).map(|c: char| match c {
                'D' => Direction::Down,
                'L' => Direction::Left,
                'R' => Direction::Right,
                'U' => Direction::Up,
                _ => unreachable!(),
            })
        }
    }

    impl Motion {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            // "L 5"
            (Direction::parser(), c::token(' '), parse::decimal_integer()).map(
                |(direction, _, distance)| Motion {
                    direction,
                    distance,
                },
            )
        }
    }
}
