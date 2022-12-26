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

const KEY_CYCLE_NUMBERS: [u32; 6] = [20, 60, 100, 140, 180, 220];

fn instructions(input: &str) -> impl Iterator<Item = Instruction> + '_ {
    input
        .lines()
        .map(|line| parse::from_str(line, Instruction::parser()).unwrap())
}

pub fn part_one(input: &str) -> Option<i32> {
    let mut cpu = Cpu::new();
    let mut instructions = instructions(input);
    let mut total_signal_strength = 0;

    loop {
        let cycle_in_progress = cpu.cycle() + 1;
        debugln!("{cycle_in_progress}: {cpu:?}");

        if KEY_CYCLE_NUMBERS.contains(&cycle_in_progress) {
            let signal_strength = cycle_in_progress as i32 * cpu.register();
            total_signal_strength += signal_strength;
        }

        if cpu.is_busy() {
            cpu.tick();
        } else if let Some(inst) = instructions.next() {
            cpu.tick_inst(inst);
        } else {
            break;
        }

        if cycle_in_progress >= KEY_CYCLE_NUMBERS[KEY_CYCLE_NUMBERS.len() - 1] {
            break;
        }
    }

    Some(total_signal_strength)
}

pub fn part_two(input: &str) -> Option<u32> {
    None
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
        assert_eq!(part_two(&input), None);
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
