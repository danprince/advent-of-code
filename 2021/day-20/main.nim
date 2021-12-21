import std/[strutils, sequtils, tables, sets]

type
  Point = tuple[x: int, y: int]
  Rules = seq[char]
  Pixels = Table[Point, char]
  Image = tuple[rules: Rules, pixels: Pixels, defaultPixel: char]

proc parse(input: string): Image = 
  let blocks = input.split("\n\n")
  for c in blocks[0]:
    result.rules.add(c)
  let lines = blocks[1].splitLines()
  for y, line in lines:
    for x, c in line:
      result.pixels[(x, y)] = c
  result.defaultPixel = '.'

proc sample(image: Image, point: Point): int =
  let (cx, cy) = point
  for y in cy - 1 .. cy + 1:
    for x in cx - 1 .. cx + 1:
      result = result shl 1
      if image.pixels.getOrDefault((x, y), image.defaultPixel) == '#':
        result = result or 1

proc enhance(image: Image): Image =
  result = image
  let xs = image.pixels.keys.toSeq.mapIt(it.x)
  let ys = image.pixels.keys.toSeq.mapIt(it.y)
  for x in xs.min - 6 .. xs.max + 6:
    for y in ys.min - 6 .. ys.max + 6:
      let num = image.sample((x, y))
      result.pixels[(x, y)] = image.rules[num]
  if image.rules[0] == '#':
    result.defaultPixel = if image.defaultPixel == image.rules[0]:
      image.rules[^1]
    else:
      image.rules[0]

proc litPixels(image: Image): int =
  for point, ch in image.pixels:
    if ch == '#':
      result += 1

proc part1(input: string): int =
  var image = parse(input)
  image = image.enhance()
  image = image.enhance()
  image.litPixels

proc part2(input: string): int =
  var image = parse(input)
  for i in 1 .. 50:
    image = image.enhance()
  image.litPixels

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 35
  assert part2(example) == 3351

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
