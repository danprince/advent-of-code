use std::fs;

#[derive(Debug)]
enum Instruction {
    Add(i32, i32, i32),
    Multiply(i32, i32, i32),
    Input(i32),
    Output(i32),
    JumpIfTrue(i32, i32),
    JumpIfFalse(i32, i32),
    LessThan(i32, i32, i32),
    Equals(i32, i32, i32),
    Halt,
}

#[derive(Debug)]
struct CPU {
    mem: Vec<i32>,
    ip: usize,
    input: i32,
    output: i32,
}

#[derive(Debug)]
enum Mode {
    Position,
    Immediate,
}

impl Mode {
    fn parse(num: i32) -> Mode {
        match num {
            0 => Mode::Position,
            1 => Mode::Immediate,
            _ => panic!("Unsupported mode: {}", num),
        }
    }
}

impl CPU {
    /// Initialize an empty CPU instance
    fn new() -> CPU {
        CPU {
            mem: vec![],
            input: 0,
            output: 0,
            ip: 0,
        }
    }

    /// Read the next value based on the parameter mode and increment
    /// the instruction pointer.
    fn next(&mut self, mode: Mode) -> i32 {
        let parameter = self.mem[self.ip];

        self.ip += 1;

        match mode {
            Mode::Position => self.mem[parameter as usize],
            Mode::Immediate => parameter,
        }
    }

    /// Fetch the next instruction and increment the instruction
    /// pointer.
    fn fetch(&mut self) -> Instruction {
        let instruction = self.mem[self.ip];

        self.ip += 1;

        let opcode = instruction % 100;
        let _mode_a = Mode::parse(instruction / 10000 % 10);
        let mode_b = Mode::parse(instruction / 1000 % 10);
        let mode_c = Mode::parse(instruction / 100 % 10);

        match opcode {
            1 => Instruction::Add(
                self.next(mode_c),
                self.next(mode_b),
                self.next(Mode::Immediate),
            ),
            2 => Instruction::Multiply(
                self.next(mode_c),
                self.next(mode_b),
                self.next(Mode::Immediate),
            ),
            3 => Instruction::Input(self.next(Mode::Immediate)),
            4 => Instruction::Output(self.next(Mode::Immediate)),
            5 => Instruction::JumpIfTrue(self.next(mode_c), self.next(mode_b)),
            6 => Instruction::JumpIfFalse(self.next(mode_c), self.next(mode_b)),
            7 => Instruction::LessThan(
                self.next(mode_c),
                self.next(mode_b),
                self.next(Mode::Immediate),
            ),
            8 => Instruction::Equals(
                self.next(mode_c),
                self.next(mode_b),
                self.next(Mode::Immediate),
            ),
            99 => Instruction::Halt,
            code => panic!("Unsupported opcode: {}", code),
        }
    }

    // Execute instructions until the "Halt" instruction is reached
    fn execute(&mut self) {
        loop {
            let instruction = self.fetch();

            match instruction {
                Instruction::Add(a, b, c) => {
                    self.mem[c as usize] = a + b;
                }
                Instruction::Multiply(a, b, c) => {
                    self.mem[c as usize] = a * b;
                }
                Instruction::Input(a) => {
                    self.mem[a as usize] = self.input;
                }
                Instruction::Output(a) => {
                    self.output = self.mem[a as usize];
                }
                Instruction::JumpIfTrue(a, b) => {
                    if a != 0 {
                        self.ip = b as usize;
                    }
                }
                Instruction::JumpIfFalse(a, b) => {
                    if a == 0 {
                        self.ip = b as usize;
                    }
                }
                Instruction::LessThan(a, b, c) => {
                    self.mem[c as usize] = match a < b {
                        true => 1,
                        false => 0,
                    }
                }
                Instruction::Equals(a, b, c) => {
                    self.mem[c as usize] = match a == b {
                        true => 1,
                        false => 0,
                    }
                }
                Instruction::Halt => break,
            }
        }
    }
}

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let memory: Vec<i32> = src.split(",").map(|c| c.parse::<i32>().unwrap()).collect();

    println!("Part 1: {}", {
        let mut cpu = CPU::new();
        cpu.mem = memory.clone();
        cpu.input = 1;
        cpu.execute();
        cpu.output
    });

    println!("Part 2: {}", {
        let mut cpu = CPU::new();
        cpu.mem = memory.clone();
        cpu.input = 5;
        cpu.execute();
        cpu.output
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let mut cpu = CPU::new();
        cpu.mem = vec![1002, 4, 3, 4, 33];
        cpu.execute();
    }

    #[test]
    fn test_equal_position_mode() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        cpu.input = 8;
        cpu.execute();
        assert_eq!(cpu.output, 1);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        cpu.input = 3;
        cpu.execute();
        assert_eq!(cpu.output, 0);
    }

    #[test]
    fn test_less_than_position_mode() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        cpu.input = 8;
        cpu.execute();
        assert_eq!(cpu.output, 0);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        cpu.input = 3;
        cpu.execute();
        assert_eq!(cpu.output, 1);
    }

    #[test]
    fn test_equal_immediate_mode() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1108, -1, 8, 3, 4, 3, 99];
        cpu.input = 8;
        cpu.execute();
        assert_eq!(cpu.output, 1);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1108, -1, 8, 3, 4, 3, 99];
        cpu.input = 3;
        cpu.execute();
        assert_eq!(cpu.output, 0);
    }

    #[test]
    fn test_less_than_immediate_mode() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1107, -1, 8, 3, 4, 3, 99];
        cpu.input = 8;
        cpu.execute();
        assert_eq!(cpu.output, 0);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1107, -1, 8, 3, 4, 3, 99];
        cpu.input = 3;
        cpu.execute();
        assert_eq!(cpu.output, 1);
    }

    #[test]
    fn test_jump_position() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        cpu.input = 0;
        cpu.execute();
        assert_eq!(cpu.output, 0);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        cpu.input = 33;
        cpu.execute();
        assert_eq!(cpu.output, 1);
    }

    #[test]
    fn test_jump_immediate() {
        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        cpu.input = 0;
        cpu.execute();
        assert_eq!(cpu.output, 0);

        let mut cpu = CPU::new();
        cpu.mem = vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        cpu.input = 33;
        cpu.execute();
        assert_eq!(cpu.output, 1);
    }

    #[test]
    fn part_two_example() {
        let mem = vec![
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];

        let mut cpu = CPU::new();
        cpu.mem = mem.clone();
        cpu.input = 7;
        cpu.execute();
        assert_eq!(cpu.output, 999);

        let mut cpu = CPU::new();
        cpu.mem = mem.clone();
        cpu.input = 8;
        cpu.execute();
        assert_eq!(cpu.output, 1000);

        let mut cpu = CPU::new();
        cpu.mem = mem.clone();
        cpu.input = 9;
        cpu.execute();
        assert_eq!(cpu.output, 1001);
    }
}
