import std/[strutils, tables, sets] 

type
  Node = string
  Graph = Table[Node, seq[Node]]
  Search = object
    node: Node
    visits: HashSet[Node]
    duplicate: bool
  VisitRule = proc (search: Search, node: Node): bool

proc parseGraph(input: string): Graph =
  for line in input.splitLines():
    let parts = line.split("-")
    let (a, b) = (parts[0], parts[1])
    discard result.hasKeyOrPut(a, @[])
    discard result.hasKeyOrPut(b, @[])
    result[a].add(b)
    result[b].add(a)

proc isLargeCave(name: string): bool =
  name[0] >= 'A' and name[0] <= 'Z'

proc add(search: Search, node: Node): Search =
  result = search
  result.node = node
  result.visits.incl(node)
  if node.isLargeCave: return
  if result.duplicate: return
  result.duplicate = node in search.visits

proc countPaths(graph: Graph, canVisit: VisitRule): int =
  var stack = @[Search(node: "start")]
  while stack.len > 0:
    let search = stack.pop()
    for next in graph[search.node]:
      if next == "start": continue
      if next == "end": result += 1; continue
      if search.canVisit(next): stack.add(search.add(next))

proc canVisitSmallCavesOnce(search: Search, node: Node): bool =
  node.isLargeCave or node notin search.visits

proc canVisitOneSmallCaveTwice(search: Search, node: Node): bool =
  if node.isLargeCave: return true
  if node notin search.visits: return true
  not search.duplicate

proc part1(input: string): int =
  parseGraph(input).countPaths(canVisitSmallCavesOnce)

proc part2(input: string): int =
  parseGraph(input).countPaths(canVisitOneSmallCaveTwice)

when isMainModule:
  const ex1 = slurp("example-1.txt")
  const ex2 = slurp("example-2.txt")
  const ex3 = slurp("example-3.txt")

  assert part1(ex1) == 10
  assert part1(ex3) == 226
  assert part1(ex2) == 19

  assert part2(ex1) == 36
  assert part2(ex2) == 103
  assert part2(ex3) == 3509

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
