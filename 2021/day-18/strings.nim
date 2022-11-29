import std/[strutils, sequtils, re, math]

type
  SnailfishNumber = seq[string]

func parse(src: string): SnailfishNumber =
  src
    .replace("[", ",[,")
    .replace("]", ",],")
    .replace(re",+", ",")
    .split(",")[1 ..< ^1]

func canExplode(num: SnailfishNumber): bool =
  var depth = 0
  for i in 0 ..< num.len:
    if num[i] == "]": depth -= 1
    if num[i] == "[": depth += 1
    if depth > 4: return true

func canSplit(num: SnailfishNumber): bool =
  for i in 0 ..< num.len:
    if num[i].parseInt() >= 10:
      return true

proc explode(num: SnailfishNumber): SnailfishNumber =
  result = num
  var depth = 0
  for i in 0 ..< num.len:
    let part = num[i]
    if part == "]":
      depth -= 1
      continue
    if part == "[":
      depth += 1
    if depth > 4:
      # found a pair to explode
      let left = result[i + 1]
      let right = result[i + 2]
      # remove the pair from the result
      for _ in 1 .. 4: result.delete(i)
      # explode left
      for j in countdown(i - 1, 0):
        if result[j] != "[" and result[j] != "]":
          result[j] = $(result[j].parseInt + left.parseInt)
          break
      # explode right
      for j in i ..< result.len:
        if result[j] != "[" and result[j] != "]":
          result[j] = $(result[j].parseInt + right.parseInt)
          break
      result.insert("0", i)
      break

proc split(num: SnailfishNumber): SnailfishNumber =
  result = num
  for i in 0 ..< num.len:
    let part = num[i]
    if part == "[" or part == "]": continue

    let val = part.parseInt()
    if val < 10: continue

    let left = floor(val / 2).int
    let right = ceil(val / 2).int
    result.delete(i)
    result.insert("]", i)
    result.insert($right, i)
    result.insert($left, i)
    result.insert("[", i)

func reduce(num: SnailfishNumber): SnailfishNumber =
  result = num
  while true:
    if result.canSplit():
      result = result.split()
    elif result.canExplode():
      result = result.explode()
    else:
      break

func `+`(a: SnailfishNumber, b: SnailfishNumber): SnailfishNumber =
  result = @["["].concat(a).concat(b).concat(@["]"]).reduce()

func magnitude(num: SnailfishNumber): int =
  0

proc part1(input: string): int =
  let nums = input.splitLines().map(parse)
  var acc = nums[0]
  for num in nums[1 .. ^1]:
   acc = acc + num
  acc.magnitude()

func part2(input: string): int =
  0

when isMainModule:
  assert parse("[[[[[9,8],1],2],3],4]").explode == parse("[[[[0,9],2],3],4]")
  assert parse("[7,[6,[5,[4,[3,2]]]]]").explode == parse("[7,[6,[5,[7,0]]]]")
  assert parse("[[6,[5,[4,[3,2]]]],1]").explode == parse("[[6,[5,[7,0]]],3]")
  assert parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode == parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  assert parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode == parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")

  assert parse("[10,0]").split == parse("[[5,5],0]")
  assert parse("[0,11]").split == parse("[0,[5,6]]")

  assert parse("[[9,1],[1,9]]").magnitude == 129
  assert parse("[[1,2],[[3,4],5]]").magnitude == 143
  assert parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude == 1384
  assert parse("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude == 445
  assert parse("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude == 791
  assert parse("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude == 1137
  assert parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude == 3488

  const example = slurp("example.txt")
  assert part1(example) == 4140

  #const input = slurp("input.txt")
  #echo "Part 1: ", part1(input)
  #echo "Part 2: ", part2(input)
