extern crate intcode;

use intcode::Status;
use intcode::VM;
use std::cmp::{max, min};
use std::collections::HashMap;
use Direction::*;

enum Direction {
    Up,
    Right,
    Down,
    Left,
}

const BLACK: i64 = 0;
const WHITE: i64 = 1;

impl Direction {
    fn clockwise(&self) -> Direction {
        match self {
            Up => Right,
            Right => Down,
            Down => Left,
            Left => Up,
        }
    }

    fn counter_clockwise(&self) -> Direction {
        match self {
            Up => Left,
            Left => Down,
            Down => Right,
            Right => Up,
        }
    }
}

type Point = (i64, i64);

struct Robot {
    facing: Direction,
    position: Point,
    canvas: HashMap<Point, i64>,
    computer: VM,
}

impl Robot {
    fn new() -> Robot {
        Robot {
            facing: Up,
            position: (0, 0),
            canvas: HashMap::new(),
            computer: VM::new(),
        }
    }

    fn read(&self, point: &Point) -> i64 {
        *self.canvas.get(point).unwrap_or(&BLACK)
    }

    fn paint(&mut self, value: i64) {
        self.canvas.insert(self.position, value);
    }

    fn forward(&mut self) {
        let (x, y) = self.position;

        self.position = match self.facing {
            Up => (x, y - 1),
            Right => (x + 1, y),
            Down => (x, y + 1),
            Left => (x - 1, y),
        };
    }

    fn turn(&mut self, code: i64) {
        self.facing = match code {
            0 => self.facing.counter_clockwise(),
            1 => self.facing.clockwise(),
            _ => panic!("Invalid turn direction: {}", code),
        }
    }

    fn run_program(&mut self, program: Vec<i64>) {
        self.computer.load(program);

        loop {
            let value = self.read(&self.position);
            self.computer.add_input(value);

            let status = self.computer.run();
            let color = self.computer.output.pop_front().unwrap();
            let turn = self.computer.output.pop_front().unwrap();

            self.paint(color);
            self.turn(turn);
            self.forward();

            if let Status::Halted = status {
                break;
            }
        }
    }

    fn canvas_to_string(&self) -> String {
        let points: Vec<&Point> = self.canvas.keys().collect();

        let (min_x, min_y, max_x, max_y) = {
            let mut min_x = std::i64::MAX;
            let mut min_y = std::i64::MAX;
            let mut max_x = std::i64::MIN;
            let mut max_y = std::i64::MIN;

            for (x, y) in &points {
                min_x = min(*x, min_x);
                min_y = min(*y, min_y);
                max_x = max(*x, max_x);
                max_y = max(*y, max_y);
            }

            (min_x, min_y, max_x, max_y)
        };

        let width = (max_x - min_x + 1) as usize;
        let height = (max_y - min_y + 1) as usize;

        let mut lines = vec![vec![" "; width]; height];

        for (x, y) in points {
            let rel_x = x - min_x;
            let rel_y = y - min_y;
            let rel_point = (rel_x, rel_y);
            let value = self.read(&rel_point);

            lines[rel_y as usize][rel_x as usize] = match value {
                WHITE => "#",
                BLACK => " ",
                _ => panic!("Unsupported color!"),
            }
        }

        lines
            .iter()
            .map(|line| line.join(""))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

fn main() {
    let src = std::fs::read_to_string("./input/input.txt")
        .unwrap()
        .trim()
        .to_string();

    let program = intcode::parse(&src);

    println!("Part 1: {}", {
        let mut robot = Robot::new();
        robot.run_program(program.clone());
        robot.canvas.len()
    });

    println!("Part 2: \n{}", {
        let mut robot = Robot::new();
        robot.paint(WHITE);
        robot.run_program(program.clone());
        robot.canvas_to_string()
    });
}
