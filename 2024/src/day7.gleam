import gleam/int
import gleam/list
import gleam/string
import simplifile.{read}

type Operator =
  fn(Int, Int) -> Int

type Equation {
  Equation(target: Int, values: List(Int))
}

const example = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

pub fn main() {
  let assert Ok(input) = read("./input/day7.txt")

  let assert 3749 = part1(example)
  let assert 66_343_330_034_722 = part1(input)

  let assert 11_387 = part2(example)
  let assert 637_696_070_419_031 = part2(input)
}

fn part1(input: String) -> Int {
  let equations = parse_equations(input)
  let operators = [int.add, int.multiply]
  let solvable = list.filter(equations, solve(_, operators))
  let targets = list.map(solvable, fn(equation) { equation.target })
  list.fold(targets, 0, int.add)
}

fn part2(input: String) -> Int {
  let equations = parse_equations(input)
  let operators = [int.add, int.multiply, concat]
  let solvable = list.filter(equations, solve(_, operators))
  let targets = list.map(solvable, fn(equation) { equation.target })
  list.fold(targets, 0, int.add)
}

fn parse_equations(input: String) -> List(Equation) {
  let lines = input |> string.trim |> string.split("\n")

  list.map(lines, fn(line) {
    let assert Ok(#(target, values)) = string.split_once(line, ": ")
    let assert Ok(target) = int.parse(target)
    let assert Ok(numbers) =
      values |> string.split(" ") |> list.try_map(int.parse)
    Equation(target, numbers)
  })
}

fn solve(equation: Equation, operators: List(Operator)) -> Bool {
  let Equation(target, values) = equation
  case values {
    [] -> False

    // If there's one item left in the list then we finished evaluating the
    // equation, check whether it's the target value.
    [value] -> value == target

    // Because all of our operators are additive or multiplicative, we can
    // prune the search space by bailing if the first value is ever greater
    // than the target value.
    [value, ..] if value > target -> False

    // If there are multiple values, then we'll begin a depth-first search
    // for each of the operators to try to find a valid solution.
    [a, b, ..values] ->
      list.any(operators, fn(operator) {
        let value = operator(a, b)
        let branch = Equation(target, [value, ..values])
        solve(branch, operators)
      })
  }
}

fn concat(a: Int, b: Int) -> Int {
  case int.parse(int.to_string(a) <> int.to_string(b)) {
    Ok(value) -> value
    _ -> panic
  }
}
