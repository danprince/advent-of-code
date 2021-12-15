import std/[strutils, tables, heapqueue, algorithm]
import "../common/time"

type
  Point = tuple[x: int, y: int]
  Node = tuple[point: Point, priority: int]
  Map = object
    width: int
    height: int
    risks: Table[Point, int]

func `<`(a, b: Node): bool = a.priority < b.priority

func distance(p1, p2: Point): int =
  abs(p1.x - p2.x) + abs(p1.y - p2.y)

proc parseMap(input: string): Map =
  let lines = input.splitLines
  result.height = lines.len
  result.width = lines[0].len
  for y, line in lines:
    for x, ch in line:
      result.risks[(x, y)] = parseInt($ch)

func `$`(map: Map): string =
  for y in 0 ..< map.height:
    for x in 0 ..< map.width:
      result &= $map.risks[(x, y)]
    result &= "\n"

func cost(map: Map, p: Point): int = map.risks[p]

iterator neighbours(map: Map, p: Point): Point =
  if p.x > 0: yield (p.x - 1, p.y)
  if p.y > 0: yield (p.x, p.y - 1)
  if p.x < map.width - 1: yield (p.x + 1, p.y)
  if p.y < map.height - 1: yield (p.x, p.y + 1)

proc findShortestPath(map: Map, start: Point, goal: Point): seq[Point] =
  var frontier: HeapQueue[Node]
  frontier.push((start, 0))

  var costSoFar: Table[Point, int]
  costSoFar[start] = 0

  var cameFrom: Table[Point, Point]

  while frontier.len > 0:
    let current = frontier.pop().point
    if current == goal: break

    for next in map.neighbours(current):
      let newCost = costSoFar[current] + map.cost(next)

      if next notin costSoFar or newCost < costSoFar[next]:
        let h = distance(next, goal)
        costSoFar[next] = newCost
        frontier.push((next, newCost + h))
        cameFrom[next] = current

  var current = goal

  while current in cameFrom:
    result.add(current)
    current = cameFrom[current]

  result.reverse

proc repeat(map: Map, x: int, y: int): Map =
  result.width = map.width * x
  result.height = map.height * y

  for blockX in 0 ..< x:
    for blockY in 0 ..< y:
      for point, risk in map.risks:
        let newX = point.x + blockX * map.width
        let newY = point.y + blockY * map.height
        let riskIncrease = blockX + blockY
        let originalRisk = map.risks[point]
        let newRisk = (originalRisk + riskIncrease - 1) mod 9 + 1
        result.risks[(newX, newY)] = newRisk

proc part1(input: string): int =
  let map = parseMap(input)
  let start = (0, 0)
  let goal = (map.width - 1, map.height - 1)
  for point in map.findShortestPath(start, goal):
    result += map.risks[point]

proc part2(input: string): int =
  var map = parseMap(input).repeat(5, 5)
  let start = (0, 0)
  let goal = (map.width - 1, map.height - 1)
  for point in map.findShortestPath(start, goal):
    result += map.risks[point]

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 40
  assert part2(example) == 315

  const input = slurp("input.txt")
  time do: echo "Part 1: ", part1(input)
  time do: echo "Part 2: ", part2(input)
