import std/[sequtils, strutils, sets, strscans]

type
  Point = tuple[x: int, y: int]
  Fold = tuple[axis: char, index: int]
  Paper = HashSet[Point]

proc parse(input: string): (Paper, seq[Fold]) =
  var paper: Paper
  var folds: seq[Fold]
  let blocks = input.split("\n\n")

  for line in blocks[0].splitLines:
    let (ok, x, y) = scanTuple(line, "$i,$i")
    assert ok, "Invalid point: " & line
    paper.incl((x, y))

  for line in blocks[1].splitLines:
    let (ok, axis, index) = scanTuple(line, "fold along $c=$i", axis, index)
    assert ok, "Invalid fold: " & line
    assert axis == 'x' or axis == 'y', "Invalid axis: " & line
    folds.add((axis, index))

  (paper, folds)

proc `$`(paper: Paper): string =
  let xs = paper.toSeq.mapIt(it.x)
  let ys = paper.toSeq.mapIt(it.y)
  for y in min(ys) .. max(ys):
    for x in min(xs) .. max(xs):
      let c = if (x, y) in paper: "#" else: "."
      result &= c
    result &= "\n"

#    3
# .#.|#.#
# ...|...
# ...|...
# 0123456

# 6 -> 0
# 4 -> 2
# 3 - (6 - 3) = 0
# 3 - (4 - 3) = 2

proc fold(paper: Paper, fold: Fold): Paper =
  let (axis, i) = fold
  for (x, y) in paper:
    var (fx, fy) = (x, y)
    if axis == 'x' and x > i: fx = i - (x - i)
    if axis == 'y' and y > i: fy = i - (y - i)
    result.incl((fx, fy))

proc part1(input: string): int =
  let (paper, folds) = parse(input)
  paper.fold(folds[0]).len

proc part2(input: string): string =
  var (paper, folds) = parse(input)
  for fold in folds: paper = paper.fold(fold)
  $paper

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 17

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: \n", part2(input)
