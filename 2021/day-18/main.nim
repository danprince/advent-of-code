import std/[strutils, sequtils, math]

type
  SnailfishNumber = ref object
    value: int
    parent: SnailfishNumber
    left: SnailfishNumber
    right: SnailfishNumber

proc parseSnailfishNumber(s: var string): SnailfishNumber =
  result = SnailfishNumber()
  if s[0].isDigit():
    var i = 0
    while s[i].isDigit(): i += 1
    let value = s[0 ..< i].parseInt()
    s = s[i .. ^1]
    result.value = value
  else:
    assert s[0] == '['
    s = s[1 .. ^1]
    result.left = s.parseSnailfishNumber()
    result.left.parent = result
    assert s[0] == ','
    s = s[1 .. ^1]
    result.right = s.parseSnailfishNumber()
    result.right.parent = result
    assert s[0] == ']'
    s = s[1 .. ^1]

proc parse(s: string): SnailfishNumber =
  var s = s
  s.parseSnailfishNumber()

proc canExplode(n: SnailfishNumber, depth = 0): bool =
  if depth >= 4: return true
  if n.left.isNil: return false
  return n.left.canExplode() or n.right.canExplode()

proc canSplit(n: SnailfishNumber): bool =
  if n.value >= 10: return true
  if n.left.isNil: return false
  return n.left.canSplit() or n.right.canSplit()

proc explode(n: SnailfishNumber, depth = 0): SnailfishNumber =
  if not n.left.isNil and depth >= 4:
    let left = n.left
    let right = n.right
    n.left = nil
    n.right = nil

proc split(n: SnailfishNumber): SnailfishNumber =
  result = n

proc magnitude(n: SnailfishNumber): int =
  0

proc part1(input: string): int =
  0

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
