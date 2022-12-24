#![doc = include_str!("../puzzles/02.md")]

use advent_of_code::debugln;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Win,
    Loss,
    Draw,
}

struct Round {
    choice: Choice,
    outcome: Outcome,
}

impl Choice {
    pub fn against(&self, other: Self) -> Round {
        let outcome = match (self, other) {
            (Choice::Rock, Choice::Rock)
            | (Choice::Paper, Choice::Paper)
            | (Choice::Scissors, Choice::Scissors) => Outcome::Draw,

            (Choice::Rock, Choice::Scissors)
            | (Choice::Paper, Choice::Rock)
            | (Choice::Scissors, Choice::Paper) => Outcome::Win,

            (Choice::Rock, Choice::Paper)
            | (Choice::Paper, Choice::Scissors)
            | (Choice::Scissors, Choice::Rock) => Outcome::Loss,
        };

        debugln!("{self:?} vs. {other:?} = {outcome:?}");

        Round {
            choice: *self,
            outcome,
        }
    }
}

impl Round {
    pub fn score(&self) -> u32 {
        let choice = self.choice;
        let outcome = self.outcome;

        let choice_score = match choice {
            Choice::Rock => 1,
            Choice::Paper => 2,
            Choice::Scissors => 3,
        };
        let outcome_score = match outcome {
            Outcome::Loss => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        };

        let score = choice_score + outcome_score;
        debugln!("{choice:?} ({choice_score}) + {outcome:?} ({outcome_score}) = {score}");

        score
    }
}

mod parse {
    use super::*;

    use std::str::FromStr;

    use combine as c;

    use c::{EasyParser, ParseError, Parser, Stream};

    type EzParseError = c::easy::Errors<char, String, c::stream::PointerOffset<str>>;
    type Result<T> = std::result::Result<T, EzParseError>;

    fn from_str<'a, P>(s: &'a str, parser: P) -> Result<P::Output>
    where
        P: Parser<c::easy::Stream<&'a str>>,
    {
        (parser, c::eof())
            .map(|(output, _)| output)
            .easy_parse(s)
            .map(|(output, rest)| {
                debug_assert_eq!(rest, "");
                output
            })
            .map_err(|err| err.map_range(|s| s.to_owned()))
    }

    impl Choice {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            c::choice((
                c::one_of("AX".chars()).map(|_| Self::Rock),
                c::one_of("BY".chars()).map(|_| Self::Paper),
                c::one_of("CZ".chars()).map(|_| Self::Scissors),
            ))
        }
    }

    impl FromStr for Choice {
        type Err = EzParseError;

        fn from_str(s: &str) -> Result<Self> {
            from_str(s, Self::parser())
        }
    }

    impl Round {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            (Choice::parser(), c::token(' '), Choice::parser())
                .map(|(their_choice, _, my_choice)| my_choice.against(their_choice))
        }
    }

    impl FromStr for Round {
        type Err = EzParseError;

        fn from_str(s: &str) -> Result<Self> {
            from_str(s, Self::parser())
        }
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let score = input
        .lines()
        .map(|line| {
            debugln!("{line}:");
            let round = line.parse::<Round>().expect("input should be valid");
            round.score()
        })
        .sum();
    Some(score)
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 2);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_one(&input), Some(15));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_two(&input), None);
    }
}
