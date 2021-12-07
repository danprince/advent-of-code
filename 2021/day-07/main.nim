import std/[strutils, sequtils]

proc naiveCostOfMove(positions: seq[int], target: int): int =
  for position in positions:
    result += abs(position - target)

proc crabCostOfMove(positions: seq[int], target: int): int =
  for position in positions:
    let n = abs(position - target)
    # Uses the addition equivalent of factorial:
    # https://en.wikipedia.org/wiki/Triangular_number
    result += (n * (n + 1)) div 2

proc part1(input: string): int =
  result = high(int)
  let positions = input.split(",").map(parseInt)
  for target in min(positions) .. max(positions):
    let cost = naiveCostOfMove(positions, target)
    if cost < result: result = cost

proc part2(input: string): int =
  let positions = input.split(",").map(parseInt)
  for target in min(positions) .. max(positions):
    let cost = crabCostOfMove(positions, target)
    if cost < result: result = cost

when isMainModule:
  const example = "16,1,2,0,4,2,7,1,2,14"
  assert part1(example) == 37
  assert part2(example) == 168
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
