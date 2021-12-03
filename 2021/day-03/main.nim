import std/[sequtils, strutils]

proc invert(c: char): char =
  if c == '0': '1' else: '0'

proc invert(str: string): string =
  str.map(invert).join

proc mostCommonBit(numbers: seq[string], index: int): char =
  var ones = 0
  for number in numbers:
    if number[index] == '1':
      ones += 1
  let zeroes = numbers.len - ones
  if ones >= zeroes: '1' else: '0'

proc findMostCommonBitwise(numbers: seq[string]): string =
  var rate = numbers[0]
  for i in 0 ..< rate.len:
    rate[i] = mostCommonBit(numbers, i)
  rate

proc findMostCommon(numbers: seq[string]): string =
  let width = numbers[0].len
  var queue = numbers
  for i in 0 ..< width:
    let bit = mostCommonBit(queue, i)
    queue = queue.filterIt(it[i] == bit)
    if queue.len == 1:
      return queue[0]

proc findLeastCommon(numbers: seq[string]): string =
  let width = numbers[0].len
  var queue = numbers
  for i in 0 ..< width:
    let bit = mostCommonBit(queue, i).invert
    queue = queue.filterIt(it[i] == bit)
    if queue.len == 1:
      return queue[0]

proc part1(input: string): int =
  let numbers = input.splitLines
  let gamma = findMostCommonBitwise(numbers)
  let epsilon = invert(gamma)
  gamma.parseBinInt * epsilon.parseBinInt

proc part2(input: string): int =
  let numbers = input.splitLines
  let oxygen = findMostCommon(numbers)
  let co2 = findLeastCommon(numbers)
  oxygen.parseBinInt * co2.parseBinInt

const example = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

assert invert("110011") == "001100"
assert findMostCommonBitwise(example.splitLines) == "10110"
assert findMostCommon(example.splitLines) == "10111"
assert findLeastCommon(example.splitLines) == "01010"
assert part1(example) == 198
assert part2(example) == 230

when isMainModule:
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
