import std/[strscans, options]

type
  Vec = tuple[x: int, y: int]
  Target = tuple[x: HSlice[int, int], y: HSlice[int, int]]

const debug = false

proc plotVelocity(vel: Vec, target: Target): Option[seq[Vec]] =
  var path: seq[Vec]
  var p = (x: 0, y: 0)
  var v = vel;

  while true:
    p = (p.x + v.x, p.y + v.y)
    if v.x > 0: v.x -= 1
    if v.x < 0: v.x += 1
    v.y -= 1
    path.add(p)
    if p.x in target.x and p.y in target.y: return some(path)
    if p.x > target.x.b: break # overshot
    if p.y < target.y.a and v.y <= 0: break # too low

  when debug:
    for y in countdown(45, -12):
      for x in 0 .. 32:
        if x == 0 and y == 0: stdout.write("S")
        elif (x, y) in path: stdout.write("#")
        elif x in target.x and y in target.y: stdout.write("T")
        else: stdout.write(".")
      stdout.write("\n")

func parseTarget(s: string): Target =
  let (ok, x1, x2, y1, y2) = scanTuple(s, "target area: x=$i..$i, y=$i..$i")
  assert ok, "Could not parse target: " & s
  (x1 .. x2, y1 .. y2)

proc part1(input: string): int =
  let target = parseTarget(input)
  # Apparently I suck at maths.
  for vx in 1..1000:
    for vy in 1 .. 1000:
      let maybePath = plotVelocity((vx, vy), target)
      if maybePath.isSome:
        for point in maybePath.get:
          if point.y > result:
            result = point.y

func part2(input: string): int =
  let target = parseTarget(input)

  for vx in -1000 .. 1000:
    for vy in -1000 .. 1000:
      let maybePath = plotVelocity((vx, vy), target)
      if maybePath.isSome:
        result += 1

when isMainModule:
  assert part1("target area: x=20..30, y=-10..-5") == 45
  assert part2("target area: x=20..30, y=-10..-5") == 112

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
