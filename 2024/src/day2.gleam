import gleam/int
import gleam/list
import gleam/string
import simplifile

type Report =
  List(Int)

const example = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

pub fn main() {
  let assert Ok(input) = simplifile.read("./input/day2.txt")

  let assert 2 = part1(example)
  let assert 220 = part1(input)

  let assert 4 = part2(example)
  let assert 296 = part2(input)
}

fn part1(input: String) -> Int {
  let assert Ok(reports) = parse_reports(input)
  list.count(reports, is_safe)
}

fn part2(input: String) -> Int {
  let assert Ok(reports) = parse_reports(input)
  list.count(reports, is_safe_with_problem_dampener)
}

fn parse_reports(input: String) -> Result(List(Report), Nil) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.try_map(parse_report)
}

fn parse_report(line: String) -> Result(Report, Nil) {
  line |> string.split(" ") |> list.try_map(int.parse)
}

fn is_safe(report: Report) -> Bool {
  let xs = report
  let ys = list.drop(report, 1)
  let steps = list.map2(ys, xs, int.subtract)
  let abs_steps = list.map(steps, int.absolute_value)
  let all_increasing = list.all(steps, fn(x) { x > 0 })
  let all_decreasing = list.all(steps, fn(x) { x < 0 })
  let safe_sizes = list.all(abs_steps, fn(x) { x >= 1 && x <= 3 })
  { all_increasing || all_decreasing } && safe_sizes
}

fn is_safe_with_problem_dampener(report: Report) -> Bool {
  let indices = list.range(from: 0, to: list.length(report) - 1)
  let variations = list.map(indices, drop_index(report, _))
  is_safe(report) || list.any(variations, is_safe)
}

fn drop_index(list: List(a), index: Int) -> List(a) {
  let before = list.take(list, index)
  let after = list.drop(list, index + 1)
  list.append(before, after)
}
