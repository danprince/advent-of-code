import std/[sequtils, strutils, tables, strscans, math]

type
  Pair = (char, char)
  Polymer = Table[Pair, int]
  Rules = Table[Pair, char]

proc parsePolymer(s: string): Polymer =
  for i in 1 ..< s.len:
    let pair = (s[i - 1], s[i])
    result.mgetOrPut(pair, 0) += 1

proc parseRules(s: string): Rules =
  for line in s.splitLines:
    let (ok, a, b, c) = scanTuple(line, "$c$c -> $c")
    assert ok, "Invalid rule: " & line
    result[(a, b)] = c

proc parse(input: string): (Polymer, Rules) =
  let blocks = input.split("\n\n")
  let polymer = parsePolymer(blocks[0])
  let rules = parseRules(blocks[1])
  (polymer, rules)

proc applyRules(polymer: Polymer, rules: Rules): Polymer =
  result = polymer
  for pair, count in polymer:
    if count == 0: continue
    if pair notin rules: continue
    let (a, c) = pair
    let b = rules[pair]
    let p1 = (a, b)
    let p2 = (b, c)
    result[pair] -= count
    result.mgetOrPut(p1, 0) += count
    result.mgetOrPut(p2, 0) += count

proc score(polymer: Polymer): int =
  var counts: Table[char, int]

  for pair, count in polymer:
    let (a, b) = pair
    counts.mgetOrPut(a, 0) += count
    counts.mgetOrPut(b, 0) += count

  let vals = counts.values.toSeq

  # Characters will be counted twice as they show up in two pairs, apart from
  # the final character which will only show up as a second element. We need
  # to account for this by halving and rounding up (for the final char).
  ((vals.max - vals.min) / 2).ceil.int

proc part1(input: string): int =
  var (polymer, rules) = parse(input)
  for i in 1 .. 10:
    polymer = applyRules(polymer, rules)
  polymer.score

proc part2(input: string): int =
  var (polymer, rules) = parse(input)
  for i in 1 .. 40:
    polymer = applyRules(polymer, rules)
  polymer.score

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 1588
  assert part2(example) == 2188189693529

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
