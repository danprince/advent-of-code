use std::fs;

fn main() {
    let input = fs::read_to_string("./input/input.txt").unwrap();
    let parts: Vec<_> = input.trim().split("-").collect();

    println!("{:?}", parts);
    let start: u32 = parts[0].parse().unwrap();
    let end: u32 = parts[1].parse().unwrap();

    println!("Part 1: {}", part_one(start, end));
    println!("Part 2: {}", part_two(start, end));
}

fn part_one(start: u32, end: u32) -> u32 {
    let mut total = 0;

    for i in start..end {
        if is_valid_password_pattern(i) {
            total += 1;
        }
    }

    total
}

fn part_two(start: u32, end: u32) -> u32 {
    let mut total = 0;

    for i in start..end {
        if is_valid_password_pattern_2(i) {
            total += 1;
            println!("{}", i);
        }
    }

    total
}

fn is_valid_password_pattern(num: u32) -> bool {
    let mut has_double_digit = false;
    let mut prev_digit = 0;

    let digits_as_str = num.to_string();

    let digits = digits_as_str
        .chars()
        .map(|char| char.to_digit(10).unwrap());

    for digit in digits {
        if digit == prev_digit {
            has_double_digit = true;
        }

        if digit < prev_digit {
            return false
        }

        prev_digit = digit;
    }

    has_double_digit
}

// TODO: Solve with Go style FSM where each state represents a rule?

fn is_valid_password_pattern_2(num: u32) -> bool {
    let mut has_double_digit = false;
    let mut current_run_length = 1;
    let mut prev_digit = 0;

    let digits_as_str = num.to_string();

    let digits = digits_as_str
        .chars()
        .map(|char| char.to_digit(10).unwrap());

    for digit in digits {
        if digit < prev_digit {
            return false
        }

        if digit == prev_digit {
            current_run_length += 1
        } else {
            if current_run_length == 2 {
                has_double_digit = true;
            }

            current_run_length = 1
        }

        prev_digit = digit;
    }

    if current_run_length == 2 {
        has_double_digit = true
    }

    has_double_digit
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_1_examples() {
        assert_eq!(is_valid_password_pattern(111123), true);
        assert_eq!(is_valid_password_pattern(111111), true);
        assert_eq!(is_valid_password_pattern(223450), false);
        assert_eq!(is_valid_password_pattern(123789), false);
    }

    #[test]
    fn part_2_examples() {
        assert_eq!(is_valid_password_pattern_2(112233), true);
        assert_eq!(is_valid_password_pattern_2(123444), false);
        assert_eq!(is_valid_password_pattern_2(111122), true);
    }
}
