import std/sequtils
import std/strutils
import std/parseutils

proc part1(input: string): int =
  let lines = splitLines(input)
  let xs = map(lines, parseInt)
  for i in 1 .. xs.len - 1:
    let x = xs[i]
    let y = xs[i - 1]
    if x > y: result += 1

proc part2(input: string): int =
  let lines = splitLines(input)
  let xs = map(lines, parseInt)
  for i in 1 .. xs.len - 3:
    let x = xs[i + 0] + xs[i + 1] + xs[i + 2]
    let y = xs[i - 1] + xs[i + 0] + xs[i + 1]
    if x > y: result += 1

assert part1("199\n200\n208\n210\n200\n207\n240\n269\n260\n263") == 7
assert part2("199\n200\n208\n210\n200\n207\n240\n269\n260\n263") == 5

const input = slurp("input.txt")
echo "Part 1: ", part1(input)
echo "Part 2: ", part2(input)
