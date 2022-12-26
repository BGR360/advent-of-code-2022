#![doc = include_str!("../puzzles/10.md")]

use advent_of_code::{debugln, helpers::parse};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Addx(i32),
    Noop,
}

impl Instruction {
    /// The number of cycles it takes to run this instruction
    pub fn cycles(&self) -> u32 {
        match self {
            Instruction::Addx(_) => 2,
            Instruction::Noop => 1,
        }
    }
}

#[derive(Debug)]
struct Cpu {
    /// The value of the CPU's only register.
    register: i32,
    /// The cycle number of the last cycle that finished.
    cycle: u32,
    /// The instruction that is currently running, if any.
    current_ins: Option<RunningInst>,
}

#[derive(Debug)]
struct RunningInst {
    inst: Instruction,
    /// The number of cycles left before the current instruction is done running.
    cycles_remaining: u32,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            register: 1,
            cycle: 0,
            current_ins: None,
        }
    }

    /// Returns the cycle number of the last cycle that finished.
    pub fn cycle(&self) -> u32 {
        self.cycle
    }

    /// Returns the current value of the register.
    pub fn register(&self) -> i32 {
        self.register
    }

    /// Returns true if the CPU is already running an instruction.
    ///
    /// Must call [`tick()`] if this returns true, otherwise must call
    /// [`tick_inst()`].
    pub fn is_busy(&self) -> bool {
        self.current_ins.is_some()
    }

    /// Advances one more cycle of the current instruction.
    ///
    /// Panics if the CPU is not currently running an instruction. Use
    /// [`tick_inst()`] instead in that case.
    pub fn tick(&mut self) {
        let current = self.current_ins.as_mut().unwrap();
        current.cycles_remaining -= 1;

        if current.cycles_remaining == 0 {
            self.finish_inst();
        }

        self.cycle += 1;
    }

    /// Feeds the next instruction into the CPU and executes one cycle of it.
    ///
    /// Panics if the CPU is currently running an instruction. Use [`tick()`]
    /// instead in that case.
    pub fn tick_inst(&mut self, inst: Instruction) {
        assert!(!self.is_busy());
        self.current_ins = Some(RunningInst {
            inst,
            cycles_remaining: inst.cycles(),
        });
        self.tick();
    }

    fn finish_inst(&mut self) {
        let inst = self.current_ins.take().unwrap().inst;

        match inst {
            Instruction::Addx(value) => self.register += value,
            Instruction::Noop => {}
        }
    }
}

#[derive(Debug)]
struct Program<'a> {
    cpu: Cpu,
    instructions: std::iter::Map<std::str::Lines<'a>, fn(&str) -> Instruction>,
}

impl<'a> Program<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cpu: Cpu::new(),
            instructions: input
                .lines()
                .map(|line| parse::from_str(line, Instruction::parser()).unwrap()),
        }
    }

    pub fn tick(&mut self) {
        if self.cpu.is_busy() {
            self.cpu.tick();
        } else if let Some(inst) = self.instructions.next() {
            self.cpu.tick_inst(inst);
        } else {
            self.cpu.tick_inst(Instruction::Noop);
        }
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }
}

pub fn part_one(input: &str) -> Option<i32> {
    const KEY_CYCLE_NUMBERS: [u32; 6] = [20, 60, 100, 140, 180, 220];

    let mut program = Program::new(input);
    let mut total_signal_strength = 0;

    loop {
        let cpu = program.cpu();
        let cycle_in_progress = cpu.cycle() + 1;
        debugln!("{cycle_in_progress}: {cpu:?}");

        if KEY_CYCLE_NUMBERS.contains(&cycle_in_progress) {
            let signal_strength = cycle_in_progress as i32 * cpu.register();
            total_signal_strength += signal_strength;
        }

        program.tick();

        if cycle_in_progress >= KEY_CYCLE_NUMBERS[KEY_CYCLE_NUMBERS.len() - 1] {
            break;
        }
    }

    Some(total_signal_strength)
}

pub fn part_two(input: &str) -> Option<String> {
    const LINE_WIDTH: usize = 40;
    const NUM_LINES: usize = 6;

    let mut program = Program::new(input);
    let mut output = String::new();

    let mut first_line = true;
    for _ in 0..NUM_LINES {
        if !first_line {
            output.push('\n');
        }

        for col in 0..LINE_WIDTH {
            let cpu = program.cpu();
            let sprite_pos = cpu.register();
            let sprite_pixels = [sprite_pos - 1, sprite_pos, sprite_pos + 1];
            if sprite_pixels.contains(&(col as i32)) {
                output.push('#');
            } else {
                output.push('.');
            }

            program.tick();
        }

        first_line = false;
    }

    Some(output)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 10);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 10);
        assert_eq!(part_one(&input), Some(13140));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 10);

        let expected_output = "\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....";

        let output = part_two(&input).unwrap();

        debugln!("Output:");
        debugln!("{output}");
        debugln!("Expected:");
        debugln!("{expected_output}");

        assert_eq!(output, expected_output);
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

    impl Instruction {
        pub fn parser<Input>() -> impl Parser<Input, Output = Self>
        where
            Input: Stream<Token = char>,
            Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
        {
            let addx = (c::string("addx "), parse::decimal_integer())
                .map(|(_, value)| Instruction::Addx(value));

            let noop = c::string("noop").map(|_| Instruction::Noop);

            c::choice((addx, noop))
        }
    }
}
