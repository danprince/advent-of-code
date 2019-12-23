extern crate intcode;

use std::fs;
use intcode::CPU;

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let memory = CPU::parse(&src);

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
