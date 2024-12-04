import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(input) = simplifile.read("./input/day3.txt")

  let example =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  let assert 161 = part1(example)
  let assert 175_700_056 = part1(input)

  let example =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  let assert 48 = part2(example)
  let assert 71_668_682 = part2(input)
}

fn part1(input: String) -> Int {
  let assert Ok(re) = regexp.from_string("mul\\((\\d+),(\\d+)\\)")
  let matches = regexp.scan(re, input)
  list.fold(matches, 0, fn(count, match) {
    let regexp.Match(_, submatches) = match
    let assert [Some(left), Some(right)] = submatches
    let assert Ok(a) = int.parse(left)
    let assert Ok(b) = int.parse(right)
    count + a * b
  })
}

fn part2(input: String) -> Int {
  let assert Ok(dont_regex) = regexp.from_string("don't\\(\\).*?(do\\(\\)|$)")
  let input = string.replace(input, "\n", "")
  let input = regexp.replace(dont_regex, input, "")
  part1(input)
}
