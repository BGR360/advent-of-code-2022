#![doc = include_str!("../puzzles/09.md")]

use std::{collections::HashSet, fmt::Display};

use glam::IVec2;

use advent_of_code::{debugln, helpers::parse};

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
    /// The position of each knot in the world. The head knot is at index 0.
    knots: Vec<IVec2>,
    /// The set of all positions that the tail knot has visited.
    tail_visited: HashSet<IVec2>,
    /// The negative-most position that any knot has visited.
    lower_bound: IVec2,
    /// The positive-most position that any knot has visited.
    upper_bound: IVec2,
}

impl World {
    pub fn new(num_knots: usize) -> Self {
        const ZERO: IVec2 = IVec2::ZERO;

        let mut tail_visited = HashSet::new();
        tail_visited.insert(ZERO);

        Self {
            knots: vec![ZERO; num_knots],
            tail_visited,
            lower_bound: ZERO,
            upper_bound: ZERO,
        }
    }

    /// Returns an object that implements [`Display`] for pretty-printing the
    /// rope like so:
    ///
    /// ```txt
    /// ......
    /// ...2..
    /// .H13..
    /// .5....
    /// 6.....
    /// ```
    pub fn display_rope(&self) -> formatting::RopePrinter<'_> {
        formatting::RopePrinter::new(self)
    }

    /// Returns an object that implements [`Display`] for pretty-printing the
    /// positions that the tail knot visited, like so:
    ///
    /// ```txt
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// ..........................
    /// #.........................
    /// #.............###.........
    /// #............#...#........
    /// .#..........#.....#.......
    /// ..#..........#.....#......
    /// ...#........#.......#.....
    /// ....#......s.........#....
    /// .....#..............#.....
    /// ......#............#......
    /// .......#..........#.......
    /// ........#........#........
    /// .........########.........
    /// ```
    pub fn display_tail_path(&self) -> formatting::TailPrinter<'_> {
        formatting::TailPrinter::new(self)
    }

    /// Sets the lower-left and upper-right positions to help with printing.
    pub fn set_bounds(&mut self, lower_bound: IVec2, upper_bound: IVec2) {
        self.lower_bound = lower_bound.min(upper_bound);
        self.upper_bound = upper_bound.max(lower_bound);
    }

    pub fn do_motion(&mut self, motion: Motion) {
        for _ in 0..motion.distance {
            self.move_head_one(motion.direction);
        }
    }

    #[inline]
    pub fn move_head_one(&mut self, dir: Direction) {
        let new_pos = self.knots[0] + IVec2::from(dir);
        self.move_knot_to(0, new_pos);

        for i in 1..self.knots.len() {
            if self.knots_are_touching(i, i - 1) {
                break;
            }
            self.move_knot_towards_previous(i);
        }

        for i in 1..self.knots.len() {
            debug_assert!(self.knots_are_touching(i, i - 1));
        }
    }

    #[inline]
    pub fn knots_are_touching(&self, a: usize, b: usize) -> bool {
        let a_to_b = (self.knots[b] - self.knots[a]).abs();
        a_to_b.x <= 1 && a_to_b.y <= 1
    }

    /// Returns the number of unique positions that the tail visited.
    pub fn tail_visited_positions(&self) -> usize {
        self.tail_visited.len()
    }

    #[inline]
    fn move_knot_towards_previous(&mut self, knot: usize) {
        let prev = knot - 1;
        let knot_pos = self.knots[knot];
        let prev_pos = self.knots[prev];

        let knot_to_prev = prev_pos - knot_pos;
        let movement = knot_to_prev.clamp(IVec2::NEG_ONE, IVec2::ONE);

        let new_pos = self.knots[knot] + movement;
        self.move_knot_to(knot, new_pos);
    }

    #[inline]
    fn move_knot_to(&mut self, knot: usize, new_pos: IVec2) {
        self.knots[knot] = new_pos;
        self.update_bounds(new_pos);
        if self.is_tail(knot) {
            self.tail_visited.insert(new_pos);
        }
    }

    #[inline]
    fn is_tail(&self, knot: usize) -> bool {
        knot == self.knots.len() - 1
    }

    #[inline]
    fn update_bounds(&mut self, new_pos: IVec2) {
        self.lower_bound = self.lower_bound.min(new_pos);
        self.upper_bound = self.upper_bound.max(new_pos);
    }
}

fn run_motions(world: &mut World, input: &str) -> usize {
    fn print_heading(contents: impl Display) {
        debugln!();
        debugln!("== {contents} ==");
        debugln!();
    };

    print_heading("Initial State");
    debugln!("{}", world.display_rope());

    let motions = input
        .lines()
        .map(|line| parse::from_str(line, Motion::parser()).unwrap());

    for motion in motions {
        world.do_motion(motion);

        print_heading(motion);
        debugln!("{}", world.display_rope());
    }

    print_heading("Visited Positions");
    debugln!("{}", world.display_tail_path());

    world.tail_visited_positions()
}

pub fn part_one(input: &str) -> Option<usize> {
    let mut world = World::new(2);
    Some(run_motions(&mut world, input))
}

pub fn part_two(input: &str) -> Option<usize> {
    let mut world = World::new(10);
    Some(run_motions(&mut world, input))
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 9);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    const SMALL_BOX_LOWER_BOUND: IVec2 = IVec2::ZERO;
    const SMALL_BOX_UPPER_BOUND: IVec2 = IVec2 { x: 5, y: 4 };

    const LARGE_BOX_LOWER_BOUND: IVec2 = IVec2 { x: -11, y: -5 };
    const LARGE_BOX_UPPER_BOUND: IVec2 = IVec2 { x: 14, y: 15 };

    #[track_caller]
    fn do_test(input: &str, knots: usize, lower_bound: IVec2, upper_bound: IVec2) -> usize {
        let mut world = World::new(knots);
        world.set_bounds(lower_bound, upper_bound);
        run_motions(&mut world, input);
        world.tail_visited_positions()
    }

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 9);
        let output = do_test(&input, 2, SMALL_BOX_LOWER_BOUND, SMALL_BOX_UPPER_BOUND);
        assert_eq!(output, 13);
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        let output = do_test(&input, 10, SMALL_BOX_LOWER_BOUND, SMALL_BOX_UPPER_BOUND);
        assert_eq!(output, 1);
    }

    #[test]
    fn test_part_two_large() {
        let input = "\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
";
        let output = do_test(input, 10, LARGE_BOX_LOWER_BOUND, LARGE_BOX_UPPER_BOUND);
        assert_eq!(output, 36);
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

mod formatting {
    use super::*;

    use std::fmt::{Display, Formatter, Result};

    impl Display for Direction {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            let c = match self {
                Direction::Up => 'U',
                Direction::Right => 'R',
                Direction::Down => 'D',
                Direction::Left => 'L',
            };
            write!(f, "{c}")
        }
    }

    impl Display for Motion {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "{} {}", self.direction, self.distance)
        }
    }

    fn fmt_world(
        f: &mut Formatter,
        world: &World,
        get_char: impl Fn(IVec2) -> Option<char>,
    ) -> Result {
        let IVec2 { x: x_min, y: y_min } = world.lower_bound;
        let IVec2 { x: x_max, y: y_max } = world.upper_bound;

        let mut first_row = true;
        for y in (y_min..=y_max).rev() {
            if !first_row {
                writeln!(f)?;
            }

            for x in x_min..=x_max {
                let pos = IVec2 { x, y };

                let char_for_pos = if let Some(c) = get_char(pos) {
                    c
                } else if pos == IVec2::ZERO {
                    's'
                } else {
                    '.'
                };

                write!(f, "{char_for_pos}")?;
            }

            first_row = false;
        }
        Ok(())
    }

    pub(super) struct RopePrinter<'a> {
        world: &'a World,
    }

    impl<'a> RopePrinter<'a> {
        pub fn new(world: &'a World) -> Self {
            Self { world }
        }
    }

    impl Display for RopePrinter<'_> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            fmt_world(f, self.world, |pos| {
                for (i, knot_pos) in self.world.knots.iter().enumerate() {
                    if *knot_pos == pos {
                        return Some(match i {
                            0 => 'H',
                            _ => (b'0' + i as u8) as char,
                        });
                    }
                }
                None
            })
        }
    }

    pub(super) struct TailPrinter<'a> {
        world: &'a World,
    }

    impl<'a> TailPrinter<'a> {
        pub fn new(world: &'a World) -> Self {
            Self { world }
        }
    }

    impl Display for TailPrinter<'_> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            fmt_world(f, self.world, |pos| {
                if pos != IVec2::ZERO && self.world.tail_visited.contains(&pos) {
                    Some('#')
                } else {
                    None
                }
            })
        }
    }
}
