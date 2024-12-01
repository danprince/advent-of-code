import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

const example = "3   4
4   3
2   5
1   3
3   9
3   3"

pub fn main() {
  let assert Ok(input) = simplifile.read("./input/day1.txt")

  let assert 11 = part1(example)
  let assert 1_646_452 = part1(input |> string.trim())

  let assert 31 = part2(example)
  let assert 23_609_874 = part2(input |> string.trim())
}

pub fn part1(input: String) -> Int {
  let #(xs, ys) =
    input
    |> string.split("\n")
    |> list.map(parse_line)
    |> list.unzip

  list.zip(list.sort(xs, int.compare), list.sort(ys, int.compare))
  |> list.map(fn(pair) { pair.0 - pair.1 })
  |> list.map(int.absolute_value)
  |> list.fold(0, int.add)
}

pub fn part2(input: String) -> Int {
  let #(xs, ys) =
    input
    |> string.split("\n")
    |> list.map(parse_line)
    |> list.unzip

  xs
  |> list.map(fn(x) { x * list.count(ys, fn(y) { x == y }) })
  |> list.fold(0, int.add)
}

fn parse_line(line: String) -> #(Int, Int) {
  case string.split_once(line, "   ") {
    Ok(#(a, b)) -> #(
      result.unwrap(int.parse(a), 0),
      result.unwrap(int.parse(b), 0),
    )
    _ -> panic as "Could not parse line"
  }
}
