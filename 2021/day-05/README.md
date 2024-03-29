Today's problem involved mapping lines of geothermal vents on the ocean floor.

## Tables

When I see an example like the following, I'm immediately thinking that I will probably use hash maps (tables in Nim).

```txt
1..
.2.
...
```

Here's how I might represent this with an array:

```nim
[1, 0, 0,
 0, 2, 0,
 0, 0, 0]
```

For a small grid, not too bad, but I'd have to hope that `0` and `.` don't end up being distinct concepts.

And here's how I'd represent this with a table.

```nim
{(0, 0): 1,
 (1, 1): 2}.toTable
```

This allows you to represent an infinite grid (negative coordinates are fine), and the memory used is determined by the number of active values in the grid, rather than the size of the grid.

For tables with numeric values that only increase, Nim has a special case table called [`CountTable`](https://nim-lang.org/docs/tables.html#CountTable).

```nim
var counts: CountTable[(int, int)]
counts.inc((0, 0))
counts.inc((0, 0))
counts.inc((1, 1))

assert counts[(0, 0)] == 2
assert counts[(1, 1)] == 1
```

## Parsing

The input was a list of vent lines, defined by their start and end coordinates.

```txt
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
```

Since the last parsing task, I discovered the [`strscans`](https://nim-lang.org/docs/strscans.html) module, which allowed me to simplify my parsing logic.

```nim
type
  Point = (int, int)
  Line = (Point, Point)

proc parseLine(str: string): Line =
  var x1, y1, x2, y2: int
  discard scanf(str, "$i,$i -> $i,$i", x1, y1, x2, y2)
  ((x1, y1), (x2, y2))
```

The `scanf` macro has its own mini-language to describe the type of variable to parse from each part of the expression. In this case `$i` means integer. Internally, the macro does the work of finding the match and parsing it to the correct type.

It deserves more appreciation, but if you ignore a return value then the compiler will error. To make this error go away, you need to use the [`discard` statement](https://nim-lang.org/docs/tut1.html#procedures-discard-statement).

This is a great use of a static type system and it's a sign here that I'm being a bit lazy. The return value from `scanf` will tell me whether the scan/parse failed, which I _should_ be handling more correctly.

## Part 1

For the first part of today's problem we only had to consider horizontal and vertical lines of vents. The goal was to count the number of points where two or more lines intersect.

The first step was to find every point between the end points of a line. Iterators seemed like a good fit.

```nim
iterator points(line: Line): Point =
  let (p1, p2) = line
  let (x1, y1) = p1
  let (x2, y2) = p2

  if x1 == x2:
    for y in min(y1, y2) .. max(y1, y2):
      yield (x1, y)

  elif y1 == y2:
    for x in min(x1, x2) ..  max(x1, x2):
      yield (x, y1)
```

It was a little bit disappointing to discover that there is no support for nested destructuring / unpacking. Getting the individual coordinates requires three lines of unpacking. Here's what I wanted to write:

```nim
iterator points(((x1, y1), (x2, y2)): Line): Point =
```

Once we have the end points, it's just a matter of checking whether the line is horizontal or vertical and iterating from the lower bound to the higher bound, yielding a point each step of the way.

Here's how that all comes together.

```nim
proc part1(input: string): int =
  let lines = input.splitLines.map(parseLine)
  var counts = initCountTable[Point]()

  for line in lines:
    for point in line.points:
      counts.inc(point)

  for p, n in counts:
    if n >= 2:
      result += 1
```

## Part 2
The second part required you to also include diagonal lines as part of your mapping process.

Adding support for diagonals to the `points` iterator was quite a natural extension.

```nim
iterator points(line: Line): Point =
  let (p1, p2) = line
  let (x1, y1) = p1
  let (x2, y2) = p2

  if x1 == x2:
    # ...

  elif y1 == y2:
    # ... 

  else:
    let len = abs(x2 - x1)
    let dx = sgn(x2 - x1)
    let dy = sgn(y2 - y1)
    for i in 0 .. len:
      yield (x1 + dx * i, y1 + dy * i)
```

I've implemented [Bresenham's Line Drawing Algorithm](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm) enough times for this to feel like a natural solution.

We have a guarantee that the diagonals will always be 45 degrees, which means we can remove the error value from Bresenhams and just increment x and y by the slope values for each point in the line.

Adding the diagonal case to the `points` iterator breaks part one, so I had to go back and skip diagonal lines there.

```nim
proc isDiagonal(line: Line): bool =
  let (p1, p2) = line
  let (x1, y1) = p1
  let (x2, y2) = p2
  x1 != x2 and y1 != y2

proc part1(input: string): int =
  let lines = input.splitLines.map(parseLine)
  var counts = initCountTable[Point]()

  for line in lines:
    if line.isDiagonal:
      continue

    for point in line.points:
      counts.inc(point)

  for p, n in counts:
    if n >= 2:
      result += 1
```

I'm starting to feel a bit more confident with Nim. I still don't always find the type errors intuitive (especially with macros) but I'm spending more time in my editor, and less time in the documentation.
