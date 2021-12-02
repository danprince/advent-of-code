import std/strutils

proc part1(input: string): int =
  var x, y = 0

  for line in input.splitLines:
    let
      parts = line.splitWhitespace
      cmd = parts[0]
      count = parts[1].parseInt

    case cmd:
      of "forward": x += count
      of "up": y -= count
      of "down": y += count

  x * y

proc part2(input: string): int =
  var x, y, aim = 0

  for line in input.splitLines:
    let
      parts = line.splitWhitespace
      cmd = parts[0]
      count = parts[1].parseInt

    case cmd:
      of "up": aim -= count
      of "down": aim += count
      of "forward":
        x += count
        y += aim * count

  x * y

assert part1("""forward 5
down 5
forward 8
up 3
down 8
forward 2""") == 150

assert part2("""forward 5
down 5
forward 8
up 3
down 8
forward 2""") == 900

const input = slurp("input.txt")
echo "Part 1: ", part1(input)
echo "Part 2: ", part2(input)
