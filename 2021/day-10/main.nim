import std/[strutils, sequtils, tables, options, algorithm] 

const opening = { '(': ')', '[': ']', '{': '}', '<': '>' }.toTable
const closing = { ')': '(', ']': '[', '}': '{', '>': '<' }.toTable
const errorScores = { ')': 3, ']': 57, '}': 1197, '>': 25137 }.toTable
const completeScores = { ')': 1, ']': 2, '}': 3, '>': 4 }.toTable

proc findCorruptedChar(line: string): Option[char] =
  var stack: seq[char]
  for c in line:
    if c in opening:
      stack.add(c)
    elif stack[^1] != closing[c]:
      return some(c)
    else:
      discard stack.pop()

proc autocomplete(line: string): string =
  var input: seq[char] = line.toSeq
  var output: seq[char]
  var pairs: seq[char]

  while input.len > 0:
    let c = input.pop()

    if c in closing:
      pairs.add(c)
    elif pairs.len > 0 and opening[c] == pairs[^1]:
      discard pairs.pop()
    else:
      output.add(opening[c])

  output.join

proc getCompletionScore(s: string): int =
  for c in s:
    result = result * 5 + completeScores[c]

proc part1(input: string): int =
  for line in splitLines(input):
    let c = findCorruptedChar(line)
    if c.isSome:
      result += errorScores[c.get()]

proc part2(input: string): int =
  var scores: seq[int]

  for line in splitLines(input):
    let c = findCorruptedChar(line)
    if c.isSome: continue

    let completion = autocomplete(line)
    let score = getCompletionScore(completion)
    scores.add(score)

  sort(scores)
  scores[scores.len div 2]

assert findCorruptedChar("{([(<{}[<>[]}>{[]{[(<()>") == some('}')
assert findCorruptedChar("[[<[([]))<([[{}[[()]]]") == some(')')
assert findCorruptedChar("[{[{({}]{}}([{[{{{}}([]") == some(']')
assert findCorruptedChar("[<(<(<(<{}))><([]([]()") == some(')')
assert findCorruptedChar("<{([([[(<>()){}]>(<<{{") == some('>')

assert autocomplete("<<<>") == ">>"
assert autocomplete("[][]({") == "})"
assert getCompletionScore("])}>") == 294

when isMainModule:
  const example = slurp("example.txt")
  echo part1(example)
  assert part1(example) == 26397
  assert part2(example) == 288957
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
