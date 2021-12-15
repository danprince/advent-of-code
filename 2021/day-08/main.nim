import std/[strutils, sequtils, tables]

type
  Digit = set[char]
  Sample = tuple
    values: seq[Digit]
    outputs: seq[Digit]

proc toSet(str: string): set[char] =
  for c in str: result.incl(c)

proc parseSample(str: string): Sample =
  let parts = str.split(" | ")
  let values = parts[0].split(" ").map(toSet)
  let outputs = parts[1].split(" ").map(toSet)
  (values, outputs)

proc parseSamples(input: string): seq[Sample] =
  input.splitLines.map(parseSample)

proc decodeOutput(sample: Sample): int =
  let (values, outputs) = sample
  var digitsByLen: Table[int, Digit]

  for value in values:
    digitsByLen[value.len] = value

  let
    one = digitsByLen[2]
    seven = digitsByLen[3]
    four = digitsByLen[4]
    eight = digitsByLen[7]

  var nine: Digit

  for value in values:
    if value.len == 6 and four < value and seven < value:
      nine = value

  let
    top = seven - one
    bottom = nine - four - seven
    bottomLeft = eight - nine

  var two: Digit

  for value in values:
    if value.len == 5 and bottomLeft < value:
      two = value

  let
    topRight = one * two
    bottomRight = one - topRight
    middle = two - top - topRight - bottom - bottomLeft
    topLeft = four - topRight - middle - bottomRight

  let
    zero = eight - middle
    three = eight - topLeft - bottomLeft
    five = eight - topRight - bottomLeft
    six = eight - topRight

  let mappings = {zero: '0', one: '1', two: '2', three: '3', four: '4', five: '5', six: '6', seven: '7', eight: '8', nine: '9'}.toTable

  outputs.mapIt(mappings[it]).join.parseInt

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
    result += decodeOutput(sample)

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 26
  assert part2(example) == 61229

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
