use std::fs;

fn main() {
    let masses: Vec<i32> = fs::read_to_string("./input/input.txt")
        .expect("Could not read input")
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .collect();

    println!("Part 1: {}", part_one(masses.clone()));
    println!("Part 2: {}", part_two(masses.clone()));
}

fn part_one(masses: Vec<i32>) -> i32 {
    masses.into_iter().map(calculate_required_fuel).sum()
}

fn part_two(masses: Vec<i32>) -> i32 {
    masses
        .into_iter()
        .map(calculate_required_fuel_recursive)
        .sum()
}

fn calculate_required_fuel(mass: i32) -> i32 {
    (mass as f64 / 3.0).floor() as i32 - 2
}

fn calculate_required_fuel_recursive(mass: i32) -> i32 {
    let fuel = calculate_required_fuel(mass);

    if fuel > 0 {
        fuel + calculate_required_fuel_recursive(fuel)
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_examples() {
        assert_eq!(calculate_required_fuel(12), 2);
        assert_eq!(calculate_required_fuel(14), 2);
        assert_eq!(calculate_required_fuel(1969), 654);
        assert_eq!(calculate_required_fuel(100756), 33583);
    }

    #[test]
    fn part_two_examples() {
        assert_eq!(calculate_required_fuel_recursive(14), 2);
        assert_eq!(calculate_required_fuel_recursive(1969), 966);
        assert_eq!(calculate_required_fuel_recursive(100756), 50346);
    }
}
