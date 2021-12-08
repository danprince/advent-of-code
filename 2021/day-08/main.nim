import std/[strutils, sequtils, sets, strformat, tables]

type
  Digit = HashSet[char]

  Sample = tuple
    values: seq[Digit]
    outputs: seq[Digit]

proc only[T](s: openArray[T]): T =
  assert(s.len == 1, fmt"Must be only exactly one item: {s}")
  s[0]

proc only[T](s: HashSet[T]): T =
  only(s.toSeq)

proc parseSample(str: string): Sample =
  let parts = str.split(" | ")
  let values = parts[0].split(" ").mapIt(it.toHashSet)
  let outputs = parts[1].split(" ").mapIt(it.toHashSet)
  (values, outputs)

proc parseSamples(input: string): seq[Sample] =
  input.splitLines.map(parseSample)

proc findSupersets(digits: seq[Digit], s: HashSet[char]): seq[Digit] =
  ## Returns the digits that are supersets of `s`.
  digits.filterIt((it + s).len == it.len)

proc rewire(sample: Sample): Table[Digit, int] =
  let digits = sample.values.toHashSet.toSeq

  let
    # These digits have a unique length, so we can identify them immediately
    one = digits.filterIt(it.len == 2).only
    four = digits.filterIt(it.len == 4).only
    seven = digits.filterIt(it.len == 3).only
    eight = digits.filterIt(it.len == 7).only

    # These are the groups we have to narrow down
    sixSegments = digits.filterIt(it.len == 6)
    fiveSegments = digits.filterIt(it.len == 5)

    # 'a' is the only segment in 7 that is not also in 1
    a = (seven - one).only
    # 9 is the only six segment that includes 4 + 7
    nine = sixSegments.findSupersets(four + seven).only
    # 'e' is the only segment in 8 but not 9
    e = (eight - nine).only
    # 2 is the only five segment digit that includes an 'e'
    two = fiveSegments.findSupersets(toHashSet([e])).only
    # g is the only segment in nine, but not in 4 + 7
    g = (nine - (four + seven)).only
    # c is the only segment in 1 and in 2
    c = (one * two).only
    # f is the character in 1 that is not c
    f = (one - toHashSet([c])).only
    # d is the character in 2 when you remove a, c, e, g
    d = (two - toHashSet([a, c, e, g])).only
    # b is the character in 8 when you remove all others
    b = (eight - toHashSet([a, c, d, e, f, g])).only

  # Now we can figure out the remaining numbers
  let zero: Digit = toHashSet([a, b, c, e, f, g])
  let three: Digit = toHashSet([a, c, d, f, g])
  let five: Digit = toHashSet([a, b, d, f, g])
  let six: Digit = toHashSet([a, b, d, e, f, g])

  result = {
    zero: 0,
    one: 1,
    two: 2,
    three: 3,
    four: 4,
    five: 5,
    six: 6,
    seven: 7,
    eight: 8,
    nine: 9
  }.toTable

proc part1(input: string): int =
  for sample in parseSamples(input):
    for output in sample.outputs:
      case output.len:
        of 2, 4, 3, 7:
          result += 1
        else:
          discard

proc part2(input: string): int =
  for sample in parseSamples(input):
    let mappings = rewire(sample)
    let outputs = sample.outputs.mapIt(mappings[it])
    let value = outputs.join.parseInt
    result += value

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 26
  assert part2(example) == 61229
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
