extern crate num_derive;
extern crate num_traits;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::collections::VecDeque;


#[derive(FromPrimitive, Debug)]
enum Opcode {
    ADD = 1,
    MUL = 2,
    IN = 3,
    OUT = 4,
    JIT = 5,
    JIF = 6,
    LT = 7,
    EQ = 8,
    HLT = 99,
}

#[derive(FromPrimitive, Debug)]
enum ParameterMode {
    Position = 0,
    Immediate = 1,
}

#[derive(Debug, PartialEq)]
pub enum Status {
    Running,
    WaitingForInput,
    Halted,
}

#[derive(Debug)]
pub struct VM {
    pub mem: Vec<i32>,
    pub ip: usize,
    pub input: VecDeque<i32>,
    pub output: VecDeque<i32>,
    pub status: Status,
}

impl Opcode {
    fn parse(code: i32) -> (Opcode, ParameterMode, ParameterMode, ParameterMode) {
        let opcode = Opcode::from_i32(code % 100).unwrap();
        let mode_a = ParameterMode::from_i32(code / 10000 % 10).unwrap();
        let mode_b = ParameterMode::from_i32(code / 1000 % 10).unwrap();
        let mode_c = ParameterMode::from_i32(code / 100 % 10).unwrap();
        (opcode, mode_a, mode_b, mode_c)
    }
}

impl VM {
    pub fn new() -> VM {
        VM {
            mem: Vec::new(),
            ip: 0,
            input: VecDeque::new(),
            output: VecDeque::new(),
            status: Status::Running,
        }
    }

    pub fn load(&mut self, program: Vec<i32>) {
        self.mem = program;
        self.ip = 0;
        self.input = VecDeque::new();
        self.output = VecDeque::new();
    }

    pub fn add_input(&mut self, value: i32) {
        self.input.push_back(value);
    }

    pub fn get_output(&mut self) -> Option<i32> {
        self.output.pop_back()
    }

    pub fn run(&mut self) -> Status {
        use Opcode::*;

        loop {
            let value = self.mem[self.ip];
            let (opcode, _mode_a, mode_b, mode_c) = Opcode::parse(value);

            match opcode {
                ADD => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b);
                    let c = self.mem[self.ip + 3] as usize;
                    self.mem[c] = a + b;
                    self.ip += 4;
                }
                MUL => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b);
                    let c = self.mem[self.ip + 3] as usize;
                    self.mem[c] = a * b;
                    self.ip += 4;
                }
                IN => match self.input.pop_front() {
                    Some(value) => {
                        let a = self.mem[self.ip + 1] as usize;
                        self.mem[a] = value;
                        self.ip += 2;
                    }
                    None => return Status::WaitingForInput,
                },
                OUT => {
                    let a = self.read(self.ip + 1, mode_c);
                    self.output.push_back(a);
                    self.ip += 2;
                }
                JIT => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b) as usize;
                    self.ip = if a != 0 { b } else { self.ip + 3 };
                }
                JIF => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b) as usize;
                    self.ip = if a == 0 { b } else { self.ip + 3 };
                }
                LT => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b);
                    let c = self.mem[self.ip + 3] as usize;
                    self.mem[c] = (a < b) as i32;
                    self.ip += 4;
                }
                EQ => {
                    let a = self.read(self.ip + 1, mode_c);
                    let b = self.read(self.ip + 2, mode_b);
                    let c = self.mem[self.ip + 3] as usize;
                    self.mem[c] = (a == b) as i32;
                    self.ip += 4;
                }
                HLT => {
                    self.ip += 1;
                    return Status::Halted;
                }
            }
        }
    }

    fn read(&mut self, addr: usize, mode: ParameterMode) -> i32 {
        let parameter = self.mem[addr];

        match mode {
            ParameterMode::Position => self.mem[parameter as usize],
            ParameterMode::Immediate => parameter,
        }
    }
}

pub fn parse(src: &str) -> Vec<i32> {
    src.split(",").map(|c| c.parse::<i32>().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_mode() {
        let mut vm = VM::new();
        vm.load(vec![1, 1, 2, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [01, 1, 2, 3, 99]);
    }

    #[test]
    fn test_immediate_mode() {
        let mut vm = VM::new();
        vm.load(vec![11101, 5, 5, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [11101, 5, 5, 10, 99]);
    }

    #[test]
    fn test_mixed_modes() {
        let mut vm = VM::new();
        vm.load(vec![101, 5, 2, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [101, 5, 2, 7, 99]);

        let mut vm = VM::new();
        vm.load(vec![1001, 1, 5, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [1001, 1, 5, 6, 99]);
    }

    #[test]
    fn test_add() {
        let mut vm = VM::new();
        vm.load(vec![1101, 10, 10, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [1101, 10, 10, 20, 99]);
    }

    #[test]
    fn test_mul() {
        let mut vm = VM::new();
        vm.load(vec![1102, 2, 10, 3, 99]);
        vm.run();
        assert_eq!(vm.mem, [1102, 2, 10, 20, 99]);
    }

    #[test]
    fn test_input() {
        let mut vm = VM::new();
        vm.load(vec![1103, 2, 0, 99]);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.mem, [1103, 2, 3, 99]);
    }

    #[test]
    fn test_input_blocking() {
        let mut vm = VM::new();
        vm.load(vec![103, 2, 0, 99]);
        assert_eq!(vm.run(), Status::WaitingForInput);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.mem, [103, 2, 3, 99]);
    }

    #[test]
    fn test_output() {
        let mut vm = VM::new();
        vm.load(vec![104, 3, 99]);
        vm.run();
        assert_eq!(vm.output, [3]);
    }

    #[test]
    fn test_halt() {
        let mut vm = VM::new();
        vm.load(vec![99]);
        assert_eq!(vm.run(), Status::Halted);
    }

    #[test]
    fn test_jump_if_false() {
        let mut vm = VM::new();
        vm.load(vec![
            3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9,
        ]);
        vm.add_input(0);
        vm.run();
        assert_eq!(vm.output, [0]);

        let mut vm = VM::new();
        vm.load(vec![
            3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9,
        ]);
        vm.add_input(33);
        vm.run();
        assert_eq!(vm.output, [1]);
    }

    #[test]
    fn test_jump_if_true() {
        let mut vm = VM::new();
        vm.load(vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]);
        vm.add_input(0);
        vm.run();
        assert_eq!(vm.output, [0]);

        let mut vm = VM::new();
        vm.load(vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]);
        vm.add_input(33);
        vm.run();
        assert_eq!(vm.output, [1]);
    }

    #[test]
    fn test_equal() {
        let mut vm = VM::new();
        vm.load(vec![3, 3, 1108, -1, 8, 3, 4, 3, 99]);
        vm.add_input(8);
        vm.run();
        assert_eq!(vm.output, [1]);

        let mut vm = VM::new();
        vm.load(vec![3, 3, 1108, -1, 8, 3, 4, 3, 99]);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.output, [0]);

        let mut vm = VM::new();
        vm.load(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]);
        vm.add_input(8);
        vm.run();
        assert_eq!(vm.output, [1]);

        let mut vm = VM::new();
        vm.load(vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.output, [0]);
    }

    #[test]
    fn test_less_than() {
        let mut vm = VM::new();
        vm.load(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]);
        vm.add_input(8);
        vm.run();
        assert_eq!(vm.output, [0]);

        let mut vm = VM::new();
        vm.load(vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.output, [1]);

        let mut vm = VM::new();
        vm.load(vec![3, 3, 1107, -1, 8, 3, 4, 3, 99]);
        vm.add_input(8);
        vm.run();
        assert_eq!(vm.output, [0]);

        let mut vm = VM::new();
        vm.load(vec![3, 3, 1107, -1, 8, 3, 4, 3, 99]);
        vm.add_input(3);
        vm.run();
        assert_eq!(vm.output, [1]);
    }
}
