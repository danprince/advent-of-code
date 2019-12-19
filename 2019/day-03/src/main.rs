use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::iter::FromIterator;

type Point = (i32, i32);
type Wire = Vec<Point>;

fn main() {
    let input = fs::read_to_string("./input/input.txt").unwrap();
    let wires = parse_wires(&input);

    println!("Part 1: {}", part_one(wires.clone()));
    println!("Part 2: {}", part_two(wires.clone()));
}

fn intersection(mut wires: Vec<Wire>) -> HashSet<Point> {
    let tail = wires.split_off(1);
    let init = HashSet::from_iter(wires.remove(0).into_iter());

    tail.into_iter().map(HashSet::from_iter).fold(
        init,
        |mut a: HashSet<Point>, b: HashSet<Point>| {
            a.retain(|x| b.contains(x));
            a
        },
    )
}

/// Parses a list of wire definitions, separated by newlines into a
/// vector of wires
fn parse_wires(input: &str) -> Vec<Wire> {
    input.lines().map(parse_wire).collect()
}

/// Parses the input format for a wire into a vector containing every
/// point that the wire will visit
fn parse_wire(input: &str) -> Wire {
    let mut wire = Vec::new();
    let mut x = 0;
    let mut y = 0;

    for part in input.split(",") {
        let (direction, steps) = part.split_at(1);

        let (step_x, step_y) = match direction {
            "U" => (0, -1),
            "D" => (0, 1),
            "L" => (-1, 0),
            "R" => (1, 0),
            _ => panic!("Invalid direction: {}", direction),
        };

        let steps = steps.parse().unwrap();

        for _ in 0..steps {
            x += step_x;
            y += step_y;
            wire.push((x, y));
        }
    }

    wire
}

/// Create a map of points against the number of steps required
/// to first arrive at that point.
fn get_point_costs(wire: &Wire) -> HashMap<Point, i32> {
    let mut costs: HashMap<Point, i32> = HashMap::new();

    for (step, point) in wire.iter().enumerate() {
        if !costs.contains_key(point) {
            costs.insert(*point, step as i32);
        }
    }

    costs
}

fn part_one(wires: Vec<Wire>) -> i32 {
    intersection(wires)
        .iter()
        .map(|(x, y)| x.abs() + y.abs())
        .min()
        .unwrap()
}

fn part_two(wires: Vec<Wire>) -> i32 {
    let mut costs: HashMap<Point, i32> = HashMap::new();

    for wire in &wires {
        for (point, steps) in get_point_costs(wire) {
            costs
                .entry(point)
                .and_modify(|cost| *cost += steps)
                .or_insert(steps);
        }
    }

    *intersection(wires)
        .iter()
        .flat_map(|point| costs.get(point))
        .min()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        let wires = parse_wires("R8,U5,L5,D3\nU7,R6,D4,L4");
        assert_eq!(part_one(wires), 6);

        let wires =
            parse_wires("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(part_one(wires), 159);

        let wires = parse_wires(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        );
        assert_eq!(part_one(wires), 135);
    }

    #[test]
    fn part_two_examples() {
        let wires =
            parse_wires("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(part_two(wires), 610);

        let wires = parse_wires(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        );
        assert_eq!(part_two(wires), 410);
    }
}
