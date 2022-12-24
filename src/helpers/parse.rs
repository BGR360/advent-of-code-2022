use combine::{easy, EasyParser, Parser};

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
