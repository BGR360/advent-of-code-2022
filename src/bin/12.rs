#![doc = include_str!("../puzzles/12.md")]

use std::fmt;

use advent_of_code::debugln;
use smallvec::SmallVec;

type Grid = grid::Grid<char>;
type Pos = glam::UVec2;

struct Puzzle {
    pub grid: Grid,
    pub start: Pos,
    pub end: Pos,
}

impl Puzzle {
    /// Returns the raw character at the given position (i.e., `'S'` and `'E'`
    /// for the start and end pos).
    #[inline]
    pub fn get(&self, pos: Pos) -> Option<char> {
        let (row, col) = Self::make_row_col(pos);
        self.grid.get(row, col).copied()
    }

    /// Returns the height at the given position (i.e., `0` and `25` for the
    /// start and end pos).
    #[inline]
    pub fn height(&self, pos: Pos) -> Option<u32> {
        let c = match self.get(pos) {
            Some('S') => Some('a'),
            Some('E') => Some('z'),
            c => c,
        };
        c.map(|c| {
            let c: u8 = c.try_into().unwrap();
            c.saturating_sub(b'a').into()
        })
    }

    /// Returns true if the position is in the grid.
    #[inline]
    pub fn contains_pos(&self, pos: Pos) -> bool {
        let (row, col) = Self::make_row_col(pos);
        let n_rows = self.grid.rows();
        let n_cols = self.grid.cols();
        (0..n_rows).contains(&row) && (0..n_cols).contains(&col)
    }

    /// Returns all the neighbors of the given position.
    pub fn neighbors(&self, pos: Pos) -> SmallVec<[Pos; 4]> {
        let mut neighbors = SmallVec::new();

        let end = Self::make_pos(self.grid.rows(), self.grid.cols());

        // Left:
        if pos.x >= 1 && pos.x < end.x {
            neighbors.push(pos - Pos::X);
        }
        // Right:
        if pos.x < end.x - 1 {
            neighbors.push(pos + Pos::X);
        }
        // Up:
        if pos.y >= 1 && pos.y < end.y {
            neighbors.push(pos - Pos::Y);
        }
        // Down:
        if pos.y < end.y - 1 {
            neighbors.push(pos + Pos::Y);
        }

        for &pos in neighbors.iter() {
            debug_assert!(self.contains_pos(pos));
        }

        neighbors
    }

    /// Returns the valid neighbors given the criteria in part 1 of the puzzle:
    ///     * the elevation of the destination square can be at most one higher
    ///       than the elevation of your current square
    pub fn valid_neighbors(&self, pos: Pos) -> SmallVec<[Pos; 4]> {
        let Some(height) = self.height(pos) else {
            return SmallVec::new();
        };

        self.neighbors(pos)
            .into_iter()
            .filter(|&pos| self.height(pos).unwrap() <= height + 1)
            .collect()
    }

    /// Returns a lower bound on the cost to move from one position to another.
    ///
    /// Returns `None` if either position is not in the grid.
    #[inline]
    pub fn cost(&self, from: Pos, to: Pos) -> Option<u32> {
        if !self.contains_pos(from) || !self.contains_pos(to) {
            return None;
        }
        let diff_x = from.x.abs_diff(to.x);
        let diff_y = from.y.abs_diff(to.y);
        Some(diff_x + diff_y)
    }

    /// Parses the grid from the puzzle input.
    pub fn parse(input: &str) -> Self {
        let n_cols = input.lines().next().unwrap().len();

        let mut grid = Grid::new(0, n_cols);
        let mut start = None;
        let mut end = None;
        for (row_idx, line) in input.lines().enumerate() {
            let mut row: Vec<char> = Vec::with_capacity(n_cols);
            for (col_idx, character) in line.chars().enumerate() {
                let pos = Self::make_pos(row_idx, col_idx);

                match character {
                    'S' => start = Some(pos),
                    'E' => end = Some(pos),
                    _ => {}
                }

                row.push(character);
            }
            grid.push_row(row);
        }

        Self {
            grid,
            start: start.unwrap(),
            end: end.unwrap(),
        }
    }

    #[inline(always)]
    fn make_pos(row: usize, col: usize) -> Pos {
        Pos {
            x: col.try_into().unwrap(),
            y: row.try_into().unwrap(),
        }
    }

    #[inline(always)]
    fn make_row_col(pos: Pos) -> (usize, usize) {
        (pos.y.try_into().unwrap(), pos.x.try_into().unwrap())
    }
}

impl fmt::Display for Puzzle {
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

pub fn part_one(input: &str) -> Option<usize> {
    let puzzle = Puzzle::parse(input);
    debugln!("{puzzle}");

    let start = puzzle.start;
    let successors = |pos: &Pos| -> SmallVec<[(Pos, u32); 4]> {
        puzzle
            .valid_neighbors(*pos)
            .into_iter()
            .map(|neighbor| {
                (neighbor, 1 /* cost */)
            })
            .collect()
    };
    let cost = |pos: &Pos| -> u32 { puzzle.cost(*pos, puzzle.end).unwrap() };
    let success = |pos: &Pos| *pos == puzzle.end;

    let (path, _cost) =
        pathfinding::directed::astar::astar(&start, successors, cost, success).unwrap();

    debugln!("path: {path:?}");

    Some(path.len() - 1)
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 12);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 12);
        assert_eq!(part_one(&input), Some(31));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 12);
        assert_eq!(part_two(&input), None);
    }
}
