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
        vm.output
    });

    println!("Part 2: {:?}", {
        let mut vm = VM::new();
        vm.load(program.clone());
        vm.add_input(2);
        vm.run();
        vm.output
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let program = vec![
            109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
        ];
        let mut vm = VM::new();
        vm.load(program.clone());
        vm.run();
        assert_eq!(vm.output, program);

        let mut vm = VM::new();
        vm.load(vec![1102, 34915192, 34915192, 7, 4, 7, 99, 0]);
        vm.run();
        assert_eq!(vm.get_output().unwrap().to_string().len(), 16);

        let mut vm = VM::new();
        vm.load(vec![104, 1125899906842624, 99]);
        vm.run();
        assert_eq!(vm.get_output().unwrap(), 1125899906842624);
    }
}
