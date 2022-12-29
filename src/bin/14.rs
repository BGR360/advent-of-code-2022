#![doc = include_str!("../puzzles/14.md")]

use std::fmt;

use advent_of_code::{debugln, helpers::parse};

/// A position in the grid, with `x` increasing to the right and `y` increasing
/// going down.
type Pos = glam::IVec2;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Tile {
    #[default]
    Air,
    Rock,
    Sand,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Tile::Air => '.',
            Tile::Rock => '#',
            Tile::Sand => 'o',
        };
        write!(f, "{c}")
    }
}

#[derive(Debug, Clone)]
struct Grid {
    pub grid: grid::Grid<Tile>,
    pub min: Pos,
}

impl Grid {
    #[inline]
    pub fn get(&self, pos: Pos) -> Option<Tile> {
        let (row, col) = self.make_row_col(pos)?;
        self.grid.get(row, col).copied()
    }

    #[inline]
    pub fn get_mut(&mut self, pos: Pos) -> Option<&mut Tile> {
        let (row, col) = self.make_row_col(pos)?;
        self.grid.get_mut(row, col)
    }

    pub fn from_paths(paths: &[Path], min: Pos, max: Pos) -> Self {
        let rows = max.y - min.y + 1;
        let cols = max.x - min.x + 1;

        let mut this = Self {
            grid: grid::Grid::new(rows.try_into().unwrap(), cols.try_into().unwrap()),
            min,
        };

        for path in paths {
            debugln!("path: {path:?}");
            for point in path.iter_points() {
                debugln!("point: {point:?}");
                if let Some(tile) = this.get_mut(point) {
                    *tile = Tile::Rock;
                }
            }
        }

        this
    }

    #[inline(always)]
    fn make_pos(&self, row: usize, col: usize) -> Option<Pos> {
        let offset = Pos {
            x: col.try_into().ok()?,
            y: row.try_into().ok()?,
        };
        Some(self.min + offset)
    }

    #[inline(always)]
    fn make_row_col(&self, pos: Pos) -> Option<(usize, usize)> {
        let offset = pos - self.min;
        let Ok(row) = offset.y.try_into() else { return None; };
        let Ok(col) = offset.x.try_into() else { return None; };
        Some((row, col))
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first_row = true;
        for row in 0..self.grid.rows() {
            if !first_row {
                writeln!(f)?;
            }

            for col in 0..self.grid.cols() {
                write!(f, "{}", self.grid.get(row, col).unwrap())?;
            }

            first_row = false;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Path {
    pub points: Vec<Pos>,
}

impl Path {
    pub fn get(&self, point_idx: usize) -> Option<Pos> {
        self.points.get(point_idx).copied()
    }

    pub fn len(&self) -> usize {
        self.points.len()
    }

    pub fn iter_lines(&self) -> self::iterators::Lines<'_> {
        iterators::Lines::new(self)
    }

    pub fn iter_points(&self) -> self::iterators::Points<'_> {
        iterators::Points::new(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Line {
    pub start: Pos,
    pub end: Pos,
}

impl Line {
    pub fn iter_points(&self) -> self::iterators::LinePoints {
        iterators::LinePoints::new(*self)
    }
}

/// Parses the [`Path`]s from the puzzle input.
fn parse_paths(input: &str) -> Vec<Path> {
    input
        .lines()
        .map(|line| parse::from_str(line, Path::parser()).unwrap())
        .collect()
}

/// Returns the upper-leftmost [`Pos`] and lower-rightmost [`Pos`] that are
/// touched by a set of [`Path`]s.
fn path_bounds(paths: &[Path]) -> (Pos, Pos) {
    const MIN_POS: Pos = Pos {
        x: i32::MIN,
        y: i32::MIN,
    };
    const MAX_POS: Pos = Pos {
        x: i32::MAX,
        y: i32::MAX,
    };

    paths.iter().flat_map(|path| path.iter_points()).fold(
        (MAX_POS, MIN_POS),
        |(mut min, mut max), pos| {
            min.x = min.x.min(pos.x);
            min.y = min.y.min(pos.y);
            max.x = max.x.max(pos.x);
            max.y = max.y.max(pos.y);
            (min, max)
        },
    )
}

/// Drops one unit of sand from the provided source position.
///
/// Returns the final resting position of the sand, or `None` if the sand falls
/// out of the grid or reaches the sand source.
fn drop_sand(grid: &Grid, from: Pos) -> Option<Pos> {
    /// The next position a unit of sand goes to after one step.
    enum Next {
        Here(Pos),
        Stuck,
        OffGrid,
    }

    let get_next_pos = |pos| -> Next {
        // A unit of sand always falls down one step if possible.
        let down = pos + Pos::Y;

        // If the tile immediately below is blocked, the unit of sand attempts
        // to instead move diagonally one step down and to the left.
        let down_left = pos + Pos::Y - Pos::X;

        // If that tile is blocked, the unit of sand attempts to instead move
        // diagonally one step down and to the right.
        let down_right = pos + Pos::Y + Pos::X;

        for candidate in [down, down_left, down_right] {
            match grid.get(candidate) {
                Some(Tile::Air) => return Next::Here(candidate),
                None => return Next::OffGrid,
                _ => {}
            }
        }

        Next::Stuck
    };

    let mut pos = from;
    loop {
        match get_next_pos(pos) {
            Next::Here(next_pos) => pos = next_pos,
            Next::Stuck => {
                if pos == from {
                    // Sand reached the sand source.
                    return None;
                } else {
                    return Some(pos);
                }
            }
            Next::OffGrid => return None,
        }
    }
}

const SAND_SOURCE: Pos = Pos { x: 500, y: 0 };

fn get_total_units_of_sand(mut grid: Grid) -> u32 {
    debugln!();
    debugln!("==== START ====");
    debugln!();
    debugln!("{grid}");

    let mut units_of_sand = 0;
    while let Some(sand_pos) = drop_sand(&grid, SAND_SOURCE) {
        *grid.get_mut(sand_pos).unwrap() = Tile::Sand;
        units_of_sand += 1;
    }

    debugln!();
    debugln!("==== END ====");
    debugln!();
    debugln!("{grid}");

    units_of_sand
}

pub fn part_one(input: &str) -> Option<u32> {
    let paths = parse_paths(input);

    let (mut min, max) = path_bounds(&paths);
    min.y = 0;

    debugln!("min: {min}, max: {max}");

    let grid = Grid::from_paths(&paths, min, max);
    Some(get_total_units_of_sand(grid))
}

pub fn part_two(input: &str) -> Option<u32> {
    let mut paths = parse_paths(input);

    let (_min, Pos { y: y_max, .. }) = path_bounds(&paths);

    // Add the floor two units below y_max. Make it wide enough to support a
    // perfect pyramid of sand with its peak at the sand source.
    let y_max = y_max + 2;
    let x_min = SAND_SOURCE.x - y_max;
    let x_max = SAND_SOURCE.x + y_max;

    let floor_path = Path {
        points: vec![Pos { x: x_min, y: y_max }, Pos { x: x_max, y: y_max }],
    };
    paths.push(floor_path);

    let min = Pos { x: x_min, y: 0 };
    let max = Pos { x: x_max, y: y_max };

    debugln!("min: {min}, max: {max}");

    let grid = Grid::from_paths(&paths, min, max);
    Some(
        get_total_units_of_sand(grid) + 1, /* to account for the last one */
    )
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 14);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 14);
        assert_eq!(part_one(&input), Some(24));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 14);
        assert_eq!(part_two(&input), Some(93));
    }
}

mod iterators {
    use super::*;

    use std::iter::{Enumerate, FlatMap, Skip};

    use itertools::{Itertools, TupleWindows};

    /// An iterator over all the [`Line`]s that make up a [`Path`].
    #[derive(Debug, Clone)]
    pub(super) struct Lines<'a> {
        lines: TupleWindows<std::slice::Iter<'a, Pos>, (&'a Pos, &'a Pos)>,
    }

    impl<'a> Lines<'a> {
        pub fn new(path: &'a Path) -> Self {
            Self {
                lines: path.points.iter().tuple_windows(),
            }
        }
    }

    impl Iterator for Lines<'_> {
        type Item = Line;

        fn next(&mut self) -> Option<Self::Item> {
            self.lines.next().map(|(&start, &end)| Line { start, end })
        }
    }

    /// An iterator over all the positions in a [`Line`].
    #[derive(Debug, Clone)]
    pub(super) struct LinePoints {
        direction: Pos,
        end: Pos,
        next: Option<Pos>,
    }

    impl LinePoints {
        pub fn new(line: Line) -> Self {
            let Line { start, end } = line;

            let next = if start.x == end.x || start.y == end.y {
                Some(start)
            } else {
                None
            };

            let direction = (end - start).clamp(Pos::NEG_ONE, Pos::ONE);

            Self {
                direction,
                end,
                next,
            }
        }
    }

    impl Iterator for LinePoints {
        type Item = Pos;

        fn next(&mut self) -> Option<Self::Item> {
            let next = self.next;

            if let Some(next) = next {
                if next == self.end {
                    self.next = None;
                } else {
                    self.next = Some(next + self.direction);
                }
            }

            next
        }
    }

    /// An iterator over all the positions in a [`Path`].
    pub(super) struct Points<'a> {
        #[allow(clippy::type_complexity)]
        points:
            FlatMap<Enumerate<Lines<'a>>, Skip<LinePoints>, fn((usize, Line)) -> Skip<LinePoints>>,
    }

    impl<'a> Points<'a> {
        pub fn new(path: &'a Path) -> Self {
            Self {
                points: Lines::new(path).enumerate().flat_map(|(i, line)| {
                    let skip = if i == 0 { 0 } else { 1 };
                    LinePoints::new(line).skip(skip)
                }),
            }
        }
    }

    impl Iterator for Points<'_> {
        type Item = Pos;

        fn next(&mut self) -> Option<Self::Item> {
            self.points.next()
        }
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

    impl Path {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            // "498,4"
            let point = (
                parse::decimal_integer(),
                c::token(','),
                parse::decimal_integer(),
            )
                .map(|(x, _, y)| Pos { x, y });

            // " -> "
            let arrow = c::string(" -> ");

            c::sep_by1(point, arrow).map(|points| Self { points })
        }
    }
}
