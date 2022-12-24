#![doc = include_str!("../puzzles/05.md")]

use std::fmt;

use advent_of_code::{
    debugln,
    helpers::{parse, slice_pair_mut},
};
use combine::EasyParser;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Crate(pub char);

impl fmt::Display for Crate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Stack(Vec<Crate>);

impl Stack {
    #[inline]
    pub fn height(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn get(&self, height: usize) -> Option<Crate> {
        self.0.get(height).copied()
    }

    pub fn move_from_9000(&mut self, other: &mut Self, count: usize) {
        for _ in 0..count {
            self.0.push(other.0.pop().unwrap());
        }
    }

    pub fn move_from_9001(&mut self, other: &mut Self, count: usize) {
        let len = other.height();
        let start = len - count;
        let crates_to_move = &other.0[start..len];
        self.0.extend_from_slice(crates_to_move);
        other.0.resize_with(start, || unreachable!());
    }

    #[inline]
    pub fn top(&self) -> Option<Crate> {
        self.get(self.height() - 1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Ship {
    stacks: Vec<Stack>,
}

impl Ship {
    pub fn stack_pair_mut(&mut self, from: usize, to: usize) -> (&mut Stack, &mut Stack) {
        let from = from - 1;
        let to = to - 1;
        let Some((from, to)) = slice_pair_mut(&mut self.stacks[..], from, to) else {
            panic!("Invalid indices: from={from}, to={to}");
        };
        (from, to)
    }

    pub fn top_of_each_stack(&self) -> String {
        self.stacks
            .iter()
            .map(|stack| stack.top().unwrap().0)
            .collect()
    }
}

#[derive(Debug, Clone, Copy)]
struct Move {
    pub count: usize,
    pub from: usize,
    pub to: usize,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Move {} from {} to {}", self.count, self.from, self.to)
    }
}

fn for_each_move(input: &str, mut op: impl FnMut(&mut Ship, Move)) -> String {
    let (ship, input) = Ship::parser().easy_parse(input).unwrap();
    let mut ship = ship.unwrap();

    println!("BEGIN:");
    println!("{ship}");

    let moves = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| parse::from_str(line, Move::parser()).unwrap());

    for m in moves {
        debugln!("{m}");
        op(&mut ship, m);
        debugln!("{ship}");
    }

    println!("END:");
    println!("{ship}");

    let solution = ship.top_of_each_stack();
    debugln!("Solution: {solution}");

    solution
}

pub fn part_one(input: &str) -> Option<String> {
    let solution = for_each_move(input, |ship, m| {
        let (from, to) = ship.stack_pair_mut(m.from, m.to);
        to.move_from_9000(from, m.count);
    });
    Some(solution)
}

pub fn part_two(input: &str) -> Option<String> {
    let solution = for_each_move(input, |ship, m| {
        let (from, to) = ship.stack_pair_mut(m.from, m.to);
        to.move_from_9001(from, m.count);
    });
    Some(solution)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 5);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_one(&input), Some("CMZ".to_owned()));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), Some("MCD".to_owned()));
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

    // "[A]" -> Some('A')
    // "   " -> None
    fn crate_or_no_crate<Input>() -> impl Parser<Input, Output = Option<Crate>>
    where
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    {
        // "[A]"
        let krate = c::between(c::token('['), c::token(']'), c::any());

        // "   "
        let no_krate = c::string("   ");

        c::choice((krate.map(|c| Some(Crate(c))), no_krate.map(|_| None)))
    }

    // "    [A] [B]     [C]" -> [None, Some('A'), Some('B'), None, Some('C')]
    fn ship_row<Input>() -> impl Parser<Input, Output = Vec<Option<Crate>>>
    where
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    {
        c::sep_by1(crate_or_no_crate(), c::token(' '))
    }

    impl Ship {
        pub fn parser<Input>() -> impl Parser<Input, Output = Result<Self, ()>>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            let newline = || c::char::newline();
            let space = || c::token(' ');

            let row = (ship_row(), newline()).map(|(row, _newline)| row);

            let rows = c::many1(c::attempt(row));

            // " 1 "
            let stack_number =
                (space(), parse::decimal_integer::<usize, _>(), space()).map(|(_, n, _)| n);

            // " 1   2   3 "
            let stack_numbers = c::sep_by1::<Vec<_>, _, _, _>(stack_number, space());

            let ship_rows = (rows, stack_numbers).map(|(rows, _)| rows);

            ship_rows.map(Ship::try_from_rows)
        }

        fn try_from_rows(rows: Vec<Vec<Option<Crate>>>) -> Result<Self, ()> {
            let expected_len = rows[0].len();
            if !rows.iter().all(|row| row.len() == expected_len) {
                return Err(());
            }

            let n_rows = rows.len();
            let n_stacks = rows[0].len();
            let mut stacks = vec![Stack::default(); n_stacks];

            debugln!("n_rows = {n_rows}, n_stacks = {n_stacks}");

            for height in 0..n_rows {
                let row_idx = n_rows - height - 1;
                let row = &rows[row_idx];

                for stack_idx in 0..n_stacks {
                    if let Some(krate) = row[stack_idx] {
                        stacks[stack_idx].0.insert(height, krate);
                    }
                }
            }

            Ok(Self { stacks })
        }
    }

    impl Move {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            (
                c::string("move "),
                parse::decimal_integer(),
                c::string(" from "),
                parse::decimal_integer(),
                c::string(" to "),
                parse::decimal_integer(),
            )
                .map(|(_, count, _, from, _, to)| Move { count, from, to })
        }
    }
}

impl fmt::Display for Ship {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_stacks = self.stacks.len();
        let tallest = self
            .stacks
            .iter()
            .map(|stack| stack.height())
            .max()
            .unwrap();

        fn write_space_delimited<T: fmt::Display>(
            f: &mut fmt::Formatter<'_>,
            items: impl IntoIterator<Item = T>,
        ) -> fmt::Result {
            let mut first = true;

            for item in items {
                if !first {
                    write!(f, " ")?;
                }
                write!(f, "{item}")?;
                first = false;
            }
            writeln!(f)?;

            Ok(())
        }

        struct CrateOrNoCrate(Option<Crate>);

        impl fmt::Display for CrateOrNoCrate {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    Some(krate) => write!(f, "{krate}"),
                    None => write!(f, "   "),
                }
            }
        }

        for height in (0..tallest).rev() {
            let crates_at_height = self.stacks.iter().map(|stack| {
                let krate = stack.get(height);
                CrateOrNoCrate(krate)
            });
            write_space_delimited(f, crates_at_height)?;
        }

        struct StackNumber(usize);

        impl fmt::Display for StackNumber {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:^3}", self.0)
            }
        }

        let stack_numbers = (1..=n_stacks).map(StackNumber);
        write_space_delimited(f, stack_numbers)?;

        Ok(())
    }
}
