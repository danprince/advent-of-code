import std/[strutils, strscans, sequtils, tables, math]

type
  Point = (int, int)
  Line = (Point, Point)

proc parseLine(str: string): Line =
  var x1, y1, x2, y2: int
  discard scanf(str, "$i,$i -> $i,$i", x1, y1, x2, y2)
  ((x1, y1), (x2, y2))

iterator points(line: Line): Point =
  let (p1, p2) = line
  let (x1, y1) = p1
  let (x2, y2) = p2

  if x1 == x2:
    for y in min(y1, y2) .. max(y1, y2):
      yield (x1, y)

  elif y1 == y2:
    for x in min(x1, x2) ..  max(x1, x2):
      yield (x, y1)

  else:
    let len = abs(x2 - x1)
    let dx = sgn(x2 - x1)
    let dy = sgn(y2 - y1)
    for i in 0 .. len:
      yield (x1 + dx * i, y1 + dy * i)

proc isDiagonal(line: Line): bool =
  let (p1, p2) = line
  let (x1, y1) = p1
  let (x2, y2) = p2
  x1 != x2 and y1 != y2

proc part1(input: string): int =
  let lines = input.splitLines.map(parseLine)
  var counts = initCountTable[Point]()

  for line in lines:
    if line.isDiagonal:
      continue

    for point in line.points:
      counts.inc(point)

  for p, n in counts:
    if n >= 2:
      result += 1

proc part2(input: string): int =
  let lines = input.splitLines.map(parseLine)
  var counts = initCountTable[Point]()

  for line in lines:
    for point in line.points:
      counts.inc(point)

  for p, n in counts:
    if n >= 2:
      result += 1

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 5
  assert part2(example) == 12
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
