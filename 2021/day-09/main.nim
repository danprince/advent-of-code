import std/[strutils, sequtils, tables, sets, algorithm] 

type
  Point = tuple[x: int, y: int]
  Heightmap = Table[Point, int]

func `[]`(map: Heightmap, p: Point): int =
  getOrDefault(map, p, 9)

iterator adjacentPoints(p: Point): Point =
  let (x, y) = p
  yield (x - 1, y)
  yield (x + 1, y)
  yield (x, y - 1)
  yield (x, y + 1)

func parseHeightMap(input: string): Heightmap =
  let lines = splitLines(input)
  for y, line in lines:
    for x, ch in line:
      result[(x, y)] = parseInt($ch)

func isLowPoint(heights: Heightmap, point: Point): bool =
  result = true
  for next in adjacentPoints(point):
    if heights[point] >= heights[next]:
      return false

func findBasins(heights: HeightMap): seq[seq[Point]] =
  var visited: HashSet[Point]

  for point, height in heights:
    if height >= 9 or point in visited: continue
    var basin: seq[Point]
    var stack = @[point]
    visited.incl(point)

    while stack.len > 0:
      let point = stack.pop
      basin.add(point)

      for next in point.adjacentPoints:
        if heights[next] >= 9 or next in visited: continue
        visited.incl(next)
        stack.add(next)

    result.add(basin)
    basin = @[]

func part1(input: string): int =
  let heights = parseHeightMap(input)
  for point, height in heights:
    if isLowPoint(heights, point):
      result += 1 + height

func part2(input: string): int =
  let heights = parseHeightMap(input)
  let basins = findBasins(heights)
  var sizes = basins.mapIt(it.len)
  sort(sizes, SortOrder.Descending)
  sizes[0] * sizes[1] * sizes[2]

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 15
  assert part2(example) == 1134
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
