import std/[strutils, sequtils, tables]

const NewFishCycle = 8
const OldFishCycle = 6

proc countFishAfterDays(initial: seq[int], days: int): int =
  var population: OrderedTable[int, int]

  for i in 0 .. NewFishCycle:
    population[i] = 0

  for fish in initial:
    population[fish] += 1

  for day in 1 .. days:
    let newFish = population[0]
    for i in 1 .. NewFishCycle:
      population[i - 1] = population[i]
    population[OldFishCycle] += newFish
    population[NewFishCycle] = newFish

  for age, count in population:
    result += count

proc part1(input: string): int =
  let fish = input.split(",").map(parseInt)
  countFishAfterDays(fish, 80)

proc part2(input: string): int =
  let fish = input.split(",").map(parseInt)
  countFishAfterDays(fish, 256)

when isMainModule:
  const example = "3,4,3,1,2"
  assert part1(example) == 5934
  assert part2(example) == 26984457539
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
