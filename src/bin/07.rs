#![doc = include_str!("../puzzles/07.md")]

use std::collections::BTreeMap;
use std::fmt;

use advent_of_code::debugln;
use combine::EasyParser;

const PART_ONE_MAX_SIZE: u64 = 100000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct File {
    pub size: u64,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Dir {
    pub entries: BTreeMap<String, Entry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Entry {
    Dir(Dir),
    File(File),
}

impl From<Dir> for Entry {
    fn from(value: Dir) -> Self {
        Self::Dir(value)
    }
}

impl From<File> for Entry {
    fn from(value: File) -> Self {
        Self::File(value)
    }
}

impl Dir {
    /// Returns a mapping from directory name to total size for this directory
    /// and all its descendants.
    pub fn total_sizes(&self) -> BTreeMap<&'_ str, u64> {
        fn inner<'a>(name: &'a str, dir: &'a Dir, totals: &mut BTreeMap<&'a str, u64>) {
            let mut total = 0;
            for (subdir_name, entry) in dir.entries.iter() {
                match entry {
                    Entry::File(File { size }) => total += size,
                    Entry::Dir(subdir) => {
                        inner(subdir_name, subdir, totals);
                        total += totals[subdir_name.as_str()];
                    }
                }
            }
            totals.insert(name, total);
        }

        let mut totals = BTreeMap::new();
        inner("/", self, &mut totals);
        totals
    }

    pub fn from_input(input: &str) -> Self {
        let mut commands = Commands::new(input);
        let this = Self::from_commands(&mut commands);
        debug_assert!(commands.next().is_none());
        debugln!("{}", DirPrettyPrinter::new(&this));
        this
    }

    fn from_commands(commands: &mut Commands<'_>) -> Self {
        let mut entries = BTreeMap::new();
        loop {
            let Some(command) = commands.next() else { break };
            match command {
                Command::Cd(name) if name == ".." => break,
                Command::Cd(name) => {
                    let subdir = Self::from_commands(commands);
                    entries.insert(name, Entry::Dir(subdir));
                }
                Command::Ls(results) => {
                    for result in results {
                        match result {
                            LsResult::Dir { .. } => {
                                /* do nothing, will be filled out elsewhere */
                            }
                            LsResult::File { name, size } => {
                                entries.insert(name, Entry::File(File { size }));
                            }
                        }
                    }
                }
            }
        }
        Self { entries }
    }
}

/// One command in the input.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    Cd(String),
    Ls(Vec<LsResult>),
}

/// One result from the `ls` [`Command`].
#[derive(Debug, Clone, PartialEq, Eq)]
enum LsResult {
    File { name: String, size: u64 },
    Dir { name: String },
}

/// An iterator over [`Command`]s in the input string.
struct Commands<'a> {
    input: &'a str,
}

impl<'a> Commands<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut this = Self { input };
        // Skip "cd /"
        this.next_command();

        this
    }

    fn next_command(&mut self) -> Option<Command> {
        match Command::parser().easy_parse(self.input) {
            Ok((command, rest)) => {
                self.input = rest;
                Some(command)
            }
            Err(err) => {
                let err = err.map_position(|p| p.translate_position(self.input));
                debugln!("Error: {err}");
                debugln!("Input: {:?}", self.input);
                None
            }
        }
    }
}

impl<'a> Iterator for Commands<'a> {
    type Item = Command;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_command()
    }
}

pub fn part_one(input: &str) -> Option<u64> {
    let root = Dir::from_input(input);
    let dir_total_sizes = root.total_sizes();

    let mut total_size_of_dirs_not_exceeding_max = 0;
    for dir_size in dir_total_sizes.values() {
        if *dir_size <= PART_ONE_MAX_SIZE {
            total_size_of_dirs_not_exceeding_max += dir_size;
        }
    }

    Some(total_size_of_dirs_not_exceeding_max)
}

pub fn part_two(input: &str) -> Option<u64> {
    const TOTAL_DISK_SPACE: u64 = 70_000_000;
    const REQUIRED_SPACE: u64 = 30_000_000;

    let root = Dir::from_input(input);
    let dir_total_sizes = root.total_sizes();
    debugln!("dir_total_sizes: {dir_total_sizes:#?}");

    // Get the total size of the whole file system.
    let fs_total_size: u64 = dir_total_sizes["/"];
    debugln!("fs_total_size: {fs_total_size}");

    // Calculate how much space we need to free.
    let current_free_space = TOTAL_DISK_SPACE.checked_sub(fs_total_size).unwrap();
    debugln!("current_free_space: {current_free_space}");
    let need_to_delete = REQUIRED_SPACE.checked_sub(current_free_space).unwrap();
    debugln!("need_to_delete: {need_to_delete}");

    // Sort the directories in increasing order by total size.
    let mut sorted_dirs: Vec<u64> = dir_total_sizes.into_values().collect();
    sorted_dirs.sort_unstable();

    // Find the first index that isn't smaller than need_to_delete.
    let index = sorted_dirs.partition_point(|&size| size < need_to_delete);

    // This is our dir!
    let size_to_delete = sorted_dirs[index];

    Some(size_to_delete)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 7);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), Some(95437));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), Some(24933642));
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

    fn name<Input>() -> impl Parser<Input, Output = String>
    where
        Input: Stream<Token = char>,
        Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
    {
        let non_ws = c::satisfy(|c: char| !c.is_whitespace());
        c::many1(non_ws)
    }

    impl LsResult {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            // "dir a"
            let dir = (c::string("dir "), name()).map(|(_, name)| LsResult::Dir { name });

            // "123 a"
            let file = (parse::decimal_integer(), c::token(' '), name())
                .map(|(size, _, name)| LsResult::File { name, size });

            let dir_or_file = c::choice((dir, file));

            parse::line(dir_or_file)
        }
    }

    impl Command {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            let dollar = c::string("$ ");

            // "cd a"
            let cd_command = (c::string("cd "), name()).map(|(_, name)| Command::Cd(name));
            let cd_command = parse::line(cd_command);

            // "ls"
            let ls_command = parse::line(c::string("ls"));

            let ls_results = c::many(LsResult::parser());

            let ls_command = (ls_command, ls_results).map(|(_, results)| Command::Ls(results));

            (dollar, c::choice((cd_command, ls_command))).map(|(_, command)| command)
        }
    }
}

#[derive(Clone, Copy)]
struct DirPrettyPrinter<'a> {
    name: &'a str,
    dir: &'a Dir,
    level: usize,
}

impl<'a> DirPrettyPrinter<'a> {
    pub fn new(dir: &'a Dir) -> Self {
        Self {
            name: "/",
            dir,
            level: 0,
        }
    }

    fn indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..(self.level + 1) {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for DirPrettyPrinter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "- {} (dir)", self.name)?;

        for (name, entry) in self.dir.entries.iter() {
            self.indent(f)?;
            match entry {
                Entry::File(File { size }) => {
                    writeln!(f, "- {} (file, size={})", name, size)?;
                }
                Entry::Dir(dir) => write!(
                    f,
                    "{}",
                    Self {
                        name,
                        dir,
                        level: self.level + 1,
                    }
                )?,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod manual_tests {
    use super::*;

    #[test]
    fn test_commands_manual() {
        let input = "\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
";
        let commands: Vec<Command> = Commands::new(input).collect();
        assert_eq!(
            commands,
            vec![
                Command::Ls(vec![
                    LsResult::Dir { name: "a".into() },
                    LsResult::File {
                        name: "b.txt".into(),
                        size: 14848514
                    },
                    LsResult::File {
                        name: "c.dat".into(),
                        size: 8504156
                    },
                    LsResult::Dir { name: "d".into() }
                ]),
                Command::Cd("a".into())
            ]
        )
    }
}
