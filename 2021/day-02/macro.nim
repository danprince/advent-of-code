import std/macros

# Takes advantage of the fact that the instructions are already valid nim
# by evaluating them at compile time with macros.
#
# Need to wrap the resulting expressions in blocks, so that the two parts
# don't interfere with each other's scopes.
#
# Solves at compile time:
#
#   nim c day-02/macro.nim

macro part1(expr: static string): int =
  let code = parseStmt(expr)

  quote do:
    block:
      var x, y = 0
      proc up(n: int) = y -= n
      proc down(n: int) = y += n
      proc forward(n: int) = x += n
      `code`
      x * y

macro part2(expr: static string): int =
  let code = parseStmt(expr)

  quote do:
    block:
      var x, y, aim = 0
      proc up(n: int) = aim -= n
      proc down(n: int) = aim += n
      proc forward(n: int) = x += n; y += aim * n
      `code`
      x * y

static:
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
