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
    pub fn for_outcome(outcome: Outcome, their_choice: Choice) -> Choice {
        for my_choice in [Choice::Rock, Choice::Paper, Choice::Scissors] {
            if Outcome::for_choices(my_choice, their_choice) == outcome {
                return my_choice;
            }
        }
        unreachable!();
    }

    pub fn beats(self, other: Choice) -> bool {
        matches!(
            (self, other),
            (Choice::Rock, Choice::Scissors)
                | (Choice::Paper, Choice::Rock)
                | (Choice::Scissors, Choice::Paper)
        )
    }
}

impl Outcome {
    pub fn for_choices(my_choice: Choice, their_choice: Choice) -> Outcome {
        if my_choice.beats(their_choice) {
            Outcome::Win
        } else if their_choice.beats(my_choice) {
            Outcome::Loss
        } else {
            Outcome::Draw
        }
    }
}

impl Round {
    pub fn new(my_choice: Choice, their_choice: Choice) -> Self {
        let outcome = Outcome::for_choices(my_choice, their_choice);

        debugln!("{my_choice:?} vs. {their_choice:?} = {outcome:?}");

        Self {
            choice: my_choice,
            outcome,
        }
    }

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

    use combine as c;

    use c::{EasyParser, ParseError, Parser, Stream};

    type EzParseError = c::easy::Errors<char, String, c::stream::PointerOffset<str>>;
    type Result<T> = std::result::Result<T, EzParseError>;

    pub fn from_str<'a, P>(s: &'a str, parser: P) -> Result<P::Output>
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

    impl Outcome {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            c::choice((
                c::token('X').map(|_| Self::Loss),
                c::token('Y').map(|_| Self::Draw),
                c::token('Z').map(|_| Self::Win),
            ))
        }
    }

    impl Round {
        pub fn part_one_parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            (Choice::parser(), c::token(' '), Choice::parser())
                .map(|(their_choice, _, my_choice)| Self::new(my_choice, their_choice))
        }

        pub fn part_two_parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            (Choice::parser(), c::token(' '), Outcome::parser()).map(
                |(their_choice, _, desired_outcome)| {
                    let my_choice = Choice::for_outcome(desired_outcome, their_choice);
                    Self::new(my_choice, their_choice)
                },
            )
        }
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let score = input
        .lines()
        .map(|line| {
            debugln!("{line}:");
            let round =
                parse::from_str(line, Round::part_one_parser()).expect("input should be valid");
            round.score()
        })
        .sum();
    Some(score)
}

pub fn part_two(input: &str) -> Option<u32> {
    let score = input
        .lines()
        .map(|line| {
            debugln!("{line}:");
            let round =
                parse::from_str(line, Round::part_two_parser()).expect("input should be valid");
            round.score()
        })
        .sum();
    Some(score)
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
        assert_eq!(part_two(&input), Some(12));
    }
}
