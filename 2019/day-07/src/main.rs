extern crate intcode;
extern crate itertools;

use intcode::Status;
use intcode::VM;
use itertools::Itertools;
use std::collections::VecDeque;
use std::fs;

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let program = intcode::parse(&src);

    println!("Part 1: {}", {
        let (_, signal) = part_one(&program);
        signal
    });

    println!("Part 2: {}", {
        let (_, signal) = part_two(&program);
        signal
    });
}

fn part_one(program: &Vec<i32>) -> (Vec<i32>, i32) {
    (0..5)
        .permutations(5)
        .map(|phases| {
            let signal = phases.iter().fold(0, |signal, phase| {
                let mut vm = VM::new();
                vm.load(program.clone());
                vm.add_input(*phase);
                vm.add_input(signal);
                vm.run();
                vm.get_output().unwrap()
            });

            (phases, signal)
        })
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .expect("No solution for part 1")
}

fn part_two(program: &Vec<i32>) -> (Vec<i32>, i32) {
    (5..10)
        .permutations(5)
        .map(|phases| {
            let mut signal = 0;

            let mut amps: VecDeque<_> = phases
                .iter()
                .map(|phase| {
                    let mut vm = VM::new();
                    vm.load(program.clone());
                    vm.add_input(*phase);
                    vm
                })
                .collect();

            while let Some(mut vm) = amps.pop_front() {
                vm.add_input(signal);
                let status = vm.run();
                signal = vm.get_output().unwrap();

                match status {
                    Status::WaitingForInput => amps.push_back(vm),
                    Status::Halted => continue,
                    Status::Running => panic!("Invalid machine state"),
                }
            }

            (phases, signal)
        })
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .expect("No solution for part 2")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let program = vec![
            3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
        ];
        assert_eq!(part_one(&program), (vec![4, 3, 2, 1, 0], 43210));

        let program = vec![
            3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23,
            99, 0, 0,
        ];
        assert_eq!(part_one(&program), (vec![0, 1, 2, 3, 4], 54321));

        let program = vec![
            3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1,
            33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
        ];
        assert_eq!(part_one(&program), (vec![1, 0, 4, 3, 2], 65210));
    }

    #[test]
    fn part_two_examples() {
        let program = vec![
            3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1,
            28, 1005, 28, 6, 99, 0, 0, 5,
        ];
        assert_eq!(part_two(&program), (vec![9, 8, 7, 6, 5], 139629729));

        let program = vec![
            3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
            -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
            53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
        ];
        assert_eq!(part_two(&program), (vec![9, 7, 8, 5, 6], 18216));
    }
}
