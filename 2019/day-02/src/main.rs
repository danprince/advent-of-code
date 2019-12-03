use std::fs;

type Program = Vec<usize>;

mod opcodes {
    pub const ADD: usize = 1;
    pub const MUL: usize = 2;
    pub const HALT: usize = 99;
}

fn main() {
    let src = fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let program = parse_program(&src);

    println!("Part 1: {}", part_one(program.clone()));
    println!("Part 2: {}", part_two(program.clone()));
}

fn part_one(mut program: Program) -> usize {
    program[1] = 12;
    program[2] = 2;

    program = run_program(program);
    program[0]
}

fn part_two(program: Program) -> usize {
    for noun in 0..99 {
        for verb in 0..99 {
            let mut local_program = program.clone();
            local_program[1] = noun;
            local_program[2] = verb;
            local_program = run_program(local_program);

            if local_program[0] == 19690720 {
                return 100 * noun + verb;
            }
        }
    }

    panic!("Couldn't find inputs")
}

fn parse_program(src: &str) -> Program {
    src.split(",")
        .map(|c| c.parse::<usize>().unwrap())
        .collect()
}

fn run_program(mut program: Program) -> Program {
    let mut ip = 0;

    while ip < program.len() {
        let op = program[ip];

        match op {
            opcodes::ADD => {
                let a = program[ip + 1];
                let b = program[ip + 2];
                let c = program[ip + 3];
                program[c] = program[a] + program[b];
                ip += 4;
            }

            opcodes::MUL => {
                let a = program[ip + 1];
                let b = program[ip + 2];
                let c = program[ip + 3];
                program[c] = program[a] * program[b];
                ip += 4;
            }

            opcodes::HALT => break,

            opcode => panic!("Unknown opcode {}", opcode),
        }
    }

    program
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        assert_eq!(
            run_program(vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]),
            vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
        );

        assert_eq!(run_program(vec![1, 0, 0, 0, 99]), vec![2, 0, 0, 0, 99]);

        assert_eq!(run_program(vec![2, 3, 0, 3, 99]), vec![2, 3, 0, 6, 99]);

        assert_eq!(
            run_program(vec![2, 4, 4, 5, 99, 0]),
            vec![2, 4, 4, 5, 99, 9801]
        );

        assert_eq!(
            run_program(vec![1, 1, 1, 4, 99, 5, 6, 0, 99]),
            vec![30, 1, 1, 4, 2, 5, 6, 0, 99]
        );
    }
}
