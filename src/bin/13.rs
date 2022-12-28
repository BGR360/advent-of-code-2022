#![doc = include_str!("../puzzles/13.md")]

use std::cmp::Ordering;
use std::fmt;

use advent_of_code::{
    debugln,
    helpers::{parse, Itertools},
};

#[derive(Clone, PartialEq, Eq)]
enum Value {
    Int(u32),
    List(Vec<Value>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => fmt::Debug::fmt(i, f),
            Self::List(v) => fmt::Debug::fmt(v, f),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Value {
    fn partial_cmp_inner(left: &Self, right: &Self, depth: usize) -> Option<Ordering> {
        fn int_to_list(i: u32) -> Value {
            Value::List(vec![Value::Int(i)])
        }

        let mut pre = String::new();
        if cfg!(debug_assertions) {
            for _ in 0..depth {
                pre.push_str("  ");
            }
            pre.push_str("- ");
        }

        debugln!("{pre}Compare {left} vs {right}");

        match (left, right) {
            // If both values are integers, the lower integer should come first.
            // If the left integer is lower than the right integer, the inputs
            // are in the right order. If the left integer is higher than the
            // right integer, the inputs are not in the right order. Otherwise,
            // the inputs are the same integer; continue checking the next part
            // of the input.
            (Self::Int(left), Self::Int(right)) => {
                let cmp = left.cmp(right);
                match cmp {
                    Ordering::Less => {
                        debugln!("  {pre}Left side is smaller, so inputs are in the right order")
                    }
                    Ordering::Greater => {
                        debugln!(
                            "  {pre}Right side is smaller, so inputs are *not* in the right order"
                        )
                    }
                    _ => {}
                }
                Some(cmp)
            }
            // If both values are lists, compare the first value of each list,
            // then the second value, and so on. If the left list runs out of
            // items first, the inputs are in the right order. If the right list
            // runs out of items first, the inputs are not in the right order.
            // If the lists are the same length and no comparison makes a
            // decision about the order, continue checking the next part of the
            // input.
            (Self::List(left), Self::List(right)) => {
                for (left, right) in left.iter().zip(right.iter()) {
                    match Self::partial_cmp_inner(left, right, depth + 1) {
                        Some(Ordering::Less) => return Some(Ordering::Less),
                        Some(Ordering::Greater) => return Some(Ordering::Greater),
                        _ => {}
                    }
                }

                let len_cmp = left.len().cmp(&right.len());
                match len_cmp {
                    Ordering::Less => {
                        debugln!(
                            "  {pre}Left side ran out of items, so inputs are in the right order"
                        );
                    }
                    Ordering::Greater => {
                        debugln!(
                            "  {pre}Right side ran out of items, so inputs are *not* in the right \
                            order"
                        )
                    }
                    _ => {}
                }
                Some(len_cmp)
            }
            // If exactly one value is an integer, convert the integer to a list
            // which contains that integer as its only value.
            (left @ Self::List(_), Self::Int(right)) => {
                let right = int_to_list(*right);
                debugln!("  {pre}Mixed types; convert right to {right} and retry comparison");
                Self::partial_cmp_inner(left, &right, depth + 1)
            }
            (Self::Int(left), right @ Self::List(_)) => {
                let left = int_to_list(*left);
                debugln!("  {pre}Mixed types; convert left to {left} and retry comparison");
                Self::partial_cmp_inner(&left, right, depth + 1)
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Self::partial_cmp_inner(self, other, 0)
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        Self::partial_cmp_inner(self, other, 0).unwrap()
    }
}

fn parse_value(line: &str) -> Option<Value> {
    parse::from_str(line, Value::parser()).ok()
}

fn parse_pair<'a>(lines: impl IntoIterator<Item = &'a str>) -> Option<(Value, Value)> {
    let pair = lines.into_iter().collect::<Vec<_>>();
    let [left, right] = pair[..] else {
        return None;
    };

    let left = parse_value(left)?;
    let right = parse_value(right)?;

    Some((left, right))
}

pub fn part_one(input: &str) -> Option<usize> {
    let pairs = input.lines().split_by(|line| line.is_empty());
    let mut index_sum = 0;

    for (i, pair) in (&pairs).into_iter().enumerate() {
        let index = i + 1;
        let (left, right) = parse_pair(pair).unwrap();

        debugln!();
        debugln!("== Pair {index} ==");
        if let Some(Ordering::Less) = left.partial_cmp(&right) {
            index_sum += index;
        }
    }

    Some(index_sum)
}

pub fn part_two(input: &str) -> Option<usize> {
    fn divider(i: u32) -> Value {
        Value::List(vec![Value::List(vec![Value::Int(i)])])
    }
    let divider_a = divider(2);
    let divider_b = divider(6);

    let mut packets: Vec<Value> = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| parse_value(line).unwrap())
        .collect();

    // Add in the divider packets.
    packets.push(divider_a.clone());
    packets.push(divider_b.clone());

    debugln!();
    debugln!("== SORTING ==");
    debugln!();
    packets.sort();

    debugln!();
    debugln!("== FINDING [[2]] ==");
    debugln!();
    let a_index = packets.binary_search(&divider_a).unwrap() + 1;

    debugln!();
    debugln!("== FINDING [[6]] ==");
    debugln!();
    let b_index = packets.binary_search(&divider_b).unwrap() + 1;

    Some(a_index * b_index)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 13);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 13);
        assert_eq!(part_one(&input), Some(13));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 13);
        assert_eq!(part_two(&input), Some(140));
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

    impl Value {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            value()
        }
    }

    fn value_<Input>() -> impl Parser<Input, Output = Value>
    where
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    {
        let integer = parse::decimal_integer();

        let list = c::between(
            c::token('['),
            c::token(']'),
            c::sep_by(value(), c::token(',')),
        );

        c::choice((integer.map(Value::Int), list.map(Value::List)))
    }

    c::parser! {
        fn value[Input]()(Input) -> Value
        where [Input: Stream<Token = char>]
        {
            value_()
        }
    }
}
