use std::str::FromStr;

use combine::{easy, EasyParser, ParseError, Parser, Stream};

pub type EzParseError<'a> = easy::ParseError<&'a str>;
pub type Result<'a, T> = std::result::Result<T, EzParseError<'a>>;

pub fn from_str<'a, P>(s: &'a str, parser: P) -> Result<'a, P::Output>
where
    P: Parser<easy::Stream<&'a str>>,
{
    (parser, combine::eof())
        .map(|(output, _)| output)
        .easy_parse(s)
        .map(|(output, rest)| {
            debug_assert_eq!(rest, "");
            output
        })
}

pub fn decimal_integer<T, Input>() -> impl Parser<Input, Output = T>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display,
{
    combine::from_str(combine::many1::<String, _, _>(combine::choice((
        combine::parser::char::digit(),
        combine::parser::char::char('-'),
    ))))
}

pub fn line<Input, P>(parser: P) -> impl Parser<Input, Output = P::Output>
where
    P: Parser<Input>,
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (parser, combine::parser::char::newline()).map(|(output, _newline)| output)
}
