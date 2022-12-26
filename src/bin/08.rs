#![doc = include_str!("../puzzles/08.md")]

use advent_of_code::debugln;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Visibility {
    #[default]
    Unknown,
    Hidden,
    Visible,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Direction {
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3,
}

impl Direction {
    pub fn iter() -> impl Iterator<Item = Self> {
        [Self::Up, Self::Right, Self::Down, Self::Left].into_iter()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Tree {
    height: u8,
    /// Indexed by [`Direction`]
    visibility: [Visibility; 4],
}

impl Tree {
    pub fn new(height: u8) -> Self {
        Self {
            height,
            visibility: Default::default(),
        }
    }

    pub fn height(&self) -> u8 {
        self.height
    }

    pub fn is_visible(&self) -> bool {
        self.visibility
            .into_iter()
            .any(|v| v == Visibility::Visible)
    }

    pub fn visibility_from(&self, dir: Direction) -> Visibility {
        self.visibility[dir as usize]
    }

    pub fn set_visibility_from(&mut self, dir: Direction, vis: Visibility) {
        self.visibility[dir as usize] = vis;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid(grid::Grid<Tree>);

impl Grid {
    /// Returns a new grid of trees with unknown visibilities, and heights given
    /// by the puzzle input string.
    pub fn from_input(input: &str) -> Self {
        let n_cols = input.lines().next().unwrap().trim().len();

        let trees_row_major: Vec<Tree> = input
            .lines()
            .flat_map(|line| line.as_bytes().iter())
            .map(|c: &u8| {
                let height = c - b'0';
                Tree::new(height)
            })
            .collect();

        Self(grid::Grid::from_vec(trees_row_major, n_cols))
    }

    /// Returns an object that implements [`Display`][std::fmt::Display] for
    /// pretty-printing a grid.
    pub fn pretty_print(&self) -> formatting::GridPrinter<'_> {
        formatting::GridPrinter::new(self)
    }

    #[inline]
    pub fn row_count(&self) -> usize {
        self.0.rows()
    }

    #[inline]
    pub fn col_count(&self) -> usize {
        self.0.cols()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = Tree> + '_ {
        self.0.iter().copied()
    }

    #[inline]
    pub fn iter_row(&self, row: usize) -> impl Iterator<Item = Tree> + '_ {
        self.0.iter_row(row).copied()
    }

    pub fn calculate_visibilities(&mut self) {
        for row in 0..self.row_count() {
            let left_to_right = self.0.iter_row_mut(row);
            Self::populate_visibilities(left_to_right, Direction::Left);

            let right_to_left = self.0.iter_row_mut(row).rev();
            Self::populate_visibilities(right_to_left, Direction::Right);
        }

        for col in 0..self.col_count() {
            let up_to_down = self.0.iter_col_mut(col);
            Self::populate_visibilities(up_to_down, Direction::Up);

            let down_to_up = self.0.iter_col_mut(col).rev();
            Self::populate_visibilities(down_to_up, Direction::Down);
        }
    }

    fn populate_visibilities<'a>(trees: impl Iterator<Item = &'a mut Tree>, from: Direction) {
        let mut tallest_so_far: Option<u8> = None;
        for tree in trees {
            let height = tree.height();
            let this_tree_is_taller = tallest_so_far
                .map(|tallest| height > tallest)
                .unwrap_or(true);

            let visibility = if this_tree_is_taller {
                Visibility::Visible
            } else {
                Visibility::Hidden
            };
            tree.set_visibility_from(from, visibility);

            if this_tree_is_taller {
                tallest_so_far = Some(height);
            }
        }
    }
}

pub fn part_one(input: &str) -> Option<usize> {
    let mut grid = Grid::from_input(input);
    debugln!("{}", grid.pretty_print());

    grid.calculate_visibilities();
    debugln!("{}", grid.pretty_print());

    let visible_count = grid.iter().filter(|tree| tree.is_visible()).count();

    Some(visible_count)
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 8);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_one(&input), Some(21));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_two(&input), None);
    }
}

mod formatting {
    use super::*;

    use std::fmt::{Display, Formatter, Result};

    /*
     *  line1:   ^     ?
     *  line2: < 4 > ? 9 ?   0
     *  line3:   v     ?
     */
    struct TreePrinter {
        tree: Tree,
    }

    impl TreePrinter {
        fn visibility_char(&self, from: Direction) -> char {
            let vis = self.tree.visibility_from(from);
            match (vis, from) {
                (Visibility::Unknown, _) => '?',
                (Visibility::Hidden, _) => ' ',
                (Visibility::Visible, Direction::Up) => '^',
                (Visibility::Visible, Direction::Right) => '>',
                (Visibility::Visible, Direction::Down) => 'v',
                (Visibility::Visible, Direction::Left) => '<',
            }
        }

        fn line1(&self, f: &mut Formatter) -> Result {
            let up = self.visibility_char(Direction::Up);
            write!(f, "  {up}  ")
        }

        fn line2(&self, f: &mut Formatter) -> Result {
            let left = self.visibility_char(Direction::Left);
            let height = self.tree.height();
            let right = self.visibility_char(Direction::Right);
            write!(f, "{left} {height} {right}")
        }

        fn line3(&self, f: &mut Formatter) -> Result {
            let down = self.visibility_char(Direction::Down);
            write!(f, "  {down}  ")
        }
    }

    struct RowPrinter<'a> {
        grid: &'a Grid,
        row_idx: usize,
    }

    impl RowPrinter<'_> {
        fn write_line(
            &self,
            f: &mut Formatter,
            op: impl Fn(&mut Formatter, TreePrinter) -> Result,
        ) -> Result {
            let mut first = true;
            for tree in self.grid.iter_row(self.row_idx) {
                if !first {
                    write!(f, " ")?;
                }
                op(f, TreePrinter { tree })?;

                first = false;
            }
            Ok(())
        }
    }

    impl Display for RowPrinter<'_> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            self.write_line(f, |f, tree| tree.line1(f))?;
            writeln!(f)?;
            self.write_line(f, |f, tree| tree.line2(f))?;
            writeln!(f)?;
            self.write_line(f, |f, tree| tree.line3(f))?;
            Ok(())
        }
    }

    pub(super) struct GridPrinter<'a> {
        grid: &'a Grid,
    }

    impl<'a> GridPrinter<'a> {
        pub fn new(grid: &'a Grid) -> Self {
            Self { grid }
        }

        fn print_header(&self, f: &mut Formatter) -> Result {
            const TREE_WIDTH: usize = 5;
            let n_cols = self.grid.col_count();
            let n_spaces = n_cols - 1;
            let header_width = (n_cols * TREE_WIDTH) + n_spaces;
            writeln!(f, "{:=^width$}\n", " GRID ", width = header_width)
        }
    }

    impl Display for GridPrinter<'_> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            self.print_header(f)?;

            let mut first = true;
            for row_idx in 0..self.grid.row_count() {
                if !first {
                    writeln!(f)?;
                }

                let row = RowPrinter {
                    grid: self.grid,
                    row_idx,
                };
                writeln!(f, "{row}")?;

                first = false;
            }

            Ok(())
        }
    }
}
