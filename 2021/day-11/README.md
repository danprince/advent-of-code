Looks like we can also cross [Cellular Automata](https://en.wikipedia.org/wiki/Cellular_automaton) off the bingo card!

Today's puzzle involves implementing an grid of octopuses that gain energy, then eventually flash, and reset. A flashing octopus also emits energy to its immediate neighbours.

The puzzle input is a 2D grid of energy levels, where each cell represents a single octopus.

It was a quick job to parse the structure into a state table.

```nim
type
  Point = tuple[x: int, y: int]
  State = Table[Point, int]

proc parse(input: string): State =
  let lines = splitLines(input)
  for y, line in lines:
    for x, c in line:
      result[(x: x, y: y)] = parseInt($c)
```

## Part One
The first part of the puzzle was to count the number of flashes after 100 steps.

For the simulation, I recreated the `neighbours` iterator that I have used for a few other days.

```nim
iterator neighbours(pos: Point): Point =
  let (x, y) = pos
  for nx in x - 1 .. x + 1:
    for ny in y - 1 .. y + 1:
      if nx != x or ny != y:
        yield (x: nx, y: ny)
```

This time, the iterator considers diagonals.

Simulating a single time unit involves iterating over all octopuses and incrementing their energy by one.

```nim
proc simulate(state: var State): int =
  var stack: seq[Point]
  var flashed: HashSet[Point]

  for pos, energy in state:
    state[pos] = energy + 1
```

Next, we can make a note of all the octopuses who will flash.

```nim
proc simulate(state: var State): int =
  var stack: seq[Point]
  var flashed: HashSet[Point]

  for pos, energy in state:
    state[pos] = energy + 1

  for pos, energy in state:
    if energy <= 9: continue
    flashed.incl(pos)
    stack.add(pos)
```

After flashing, we need to transfer one energy to all the neighbouring octopuses, and if they flash, we need to continue the process.

I usually find these kinds of recursive solutions easier to implement with a stack, so that's what I did here.

```nim
proc simulate(state: var State): int =
  var stack: seq[Point]
  var flashed: HashSet[Point]

  # ...

  while stack.len > 0:
    var pos = stack.pop()

    for next in neighbours(pos):
      if next in state and next notin flashed:
        state[next] += 1
        if state[next] > 9:
          stack.add(next)
          flashed.incl(next)
```

The final step to reset the octopuses that flashed and return the length of the flashed set to solve the first part of the puzzle.

```nim
proc part1(input: string): int =
  var state = parse(input)
  for i in 1 .. 100:
    result += simulate(state)
```

## Part Two
The second part of the puzzle was to find the first time step where all octopuses flash together.

My implementation for part one made this straightforward.

```nim
proc part2(input: string): int =
  var state = parse(input)
  for i in 1 .. high(int):
    let flashes = simulate(state)
    if flashes == state.len:
      return i
```

I initially tried to use `inf` but in Nim, `inf` is a float, so I used `high(int)` instead, to make sure the loop kept going for as long as possible.
