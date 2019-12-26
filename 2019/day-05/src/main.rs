extern crate intcode;

use intcode::VM;
use std::fs;

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let program = intcode::parse(&src);

    println!("Part 1: {:?}", {
        let mut vm = VM::new();
        vm.load(program.clone());
        vm.add_input(1);
        vm.run();
        vm.get_output().expect("No solution for part 1")
    });

    println!("Part 2: {:?}", {
        let mut vm = VM::new();
        vm.load(program.clone());
        vm.add_input(5);
        vm.run();
        vm.get_output().expect("No solution for part 2")
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let mut vm = VM::new();
        vm.load(vec![1002, 4, 3, 4, 33]);
        vm.run();
        assert_eq!(vm.mem, [1002, 4, 3, 4, 99]);

        let mut vm = VM::new();
        vm.load(vec![1101, 100, -1, 4, 0]);
        vm.run();
        assert_eq!(vm.mem, [1101, 100, -1, 4, 99]);
    }

    #[test]
    fn part_two_example() {
        let mem = vec![
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];

        let mut vm = VM::new();
        vm.load(mem.clone());
        vm.add_input(7);
        vm.run();
        assert_eq!(vm.output, [999]);

        let mut vm = VM::new();
        vm.load(mem.clone());
        vm.add_input(8);
        vm.run();
        assert_eq!(vm.output, [1000]);

        let mut vm = VM::new();
        vm.load(mem.clone());
        vm.add_input(9);
        vm.run();
        assert_eq!(vm.output, [1001]);
    }
}
