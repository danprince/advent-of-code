import std/[strutils, tables, sets] 

type
  Point = tuple[x: int, y: int]
  State = Table[Point, int]

proc parse(input: string): State =
  let lines = splitLines(input)
  for y, line in lines:
    for x, c in line:
      result[(x: x, y: y)] = parseInt($c)

iterator neighbours(pos: Point): Point =
  let (x, y) = pos
  for nx in x - 1 .. x + 1:
    for ny in y - 1 .. y + 1:
      if nx != x or ny != y:
        yield (x: nx, y: ny)

proc simulate(state: var State): int =
  var stack: seq[Point]
  var flashed: HashSet[Point]

  for pos, energy in state:
    state[pos] = energy + 1

  for pos, energy in state:
    if energy <= 9: continue
    flashed.incl(pos)
    stack.add(pos)

  while stack.len > 0:
    var pos = stack.pop()

    for next in neighbours(pos):
      if next in state and next notin flashed:
        state[next] += 1
        if state[next] > 9:
          stack.add(next)
          flashed.incl(next)

  for pos, energy in state:
    if energy > 9:
      state[pos] = 0

  flashed.len

proc part1(input: string): int =
  var state = parse(input)
  for i in 1 .. 100:
    result += simulate(state)

proc part2(input: string): int =
  var state = parse(input)
  for i in 1 .. high(int):
    let flashes = simulate(state)
    if flashes == state.len:
      return i

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 1656
  assert part2(example) == 195
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
