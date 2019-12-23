#[derive(Debug)]
pub enum Instruction {
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
pub struct CPU {
    pub mem: Vec<i32>,
    pub ip: usize,
    pub input: i32,
    pub output: i32,
}

#[derive(Debug)]
pub enum Mode {
    Position,
    Immediate,
}

impl Mode {
    pub fn parse(num: i32) -> Mode {
        match num {
            0 => Mode::Position,
            1 => Mode::Immediate,
            _ => panic!("Unsupported mode: {}", num),
        }
    }
}

impl CPU {
    /// Initialize an empty CPU instance
    pub fn new() -> CPU {
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
    pub fn execute(&mut self) {
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

    pub fn parse(src: &str) -> Vec<i32> {
        src.split(",").map(|c| c.parse::<i32>().unwrap()).collect()
    }
}
