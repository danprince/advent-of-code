Today's challenge was another staple of Advent of Code that is probably setting us up for later challenges.

The gist was to parse a series of directional commands for the submarine and figure out the position after applying them all.

Here's the example input:

```
forward 5
down 5
forward 8
up 3
down 8
forward 2
```

The interesting thing I noticed here is that these commands are actually already valid Nim syntax.

That makes this a great candidate for an irresponsible solution using macros.

```nim
macro part1(expr: static string): int =
  let code = parsestmt(expr)

  quote do:
    var x, y = 0 proc up(n: int) = y -= n
    proc down(n: int) = y += n
    proc forward(n: int) = x += n
    `code`
    x * y
```

The `static string` type means that this macro takes a string that must be known at compile time.

Then the `parseStmt` procedure from the `macros` package parses a string and returns the equivalent syntax nodes.

Finally, the `quote do:` statement describes the code that we want to generate when this macro is invoked. This is a much nicer way to write macros than manually arranging syntax nodes, but I suppose it's only really viable with simpler macros.

When invoked with the example instructions, you can imagine that this macro expands to the following code.

```nim
var x, y = 0
proc up(n: int) = y -= n
proc down(n: int) = y += n
proc forward(n: int) = x += n
forward 5
down 5
forward 8
up 3
down 8
forward 2
x * y
```

After adding a similar solution for part 2, I discovered that the variables names clash between the two macros. I was expecting Nim's `genSym` to generate a unique symbol for each variable at runtime, this didn't happen, but I was able to solve by wrapping both macro bodies in blocks.


```nim
macro part1(expr: static string): int =
  let code = parsestmt(expr)

  quote do:
    block:
      var x, y = 0 proc up(n: int) = y -= n
      proc down(n: int) = y += n
      proc forward(n: int) = x += n
      `code`
      x * y
```

Macros are evaluated at compile time, using a VM and a subset of the language called [NimScript](https://nim-lang.org/docs/nims.html). This VM can also run evaluate expressions marked with [`static`](https://nim-lang.org/docs/manual.html#statements-and-expressions-static-statementslashexpression), so long as their code can be evaluated at compile time.

Because I'm using [`slurp`](https://nim-lang.org/docs/system.html#slurp%2Cstring) (an alias for `staticRead`) to embed my input file into the resulting binary, it's actually possible to run the whole solution at compile time.

```nim
static:
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
```

It's surprisingly novel to see the puzzle output show up before the compiler hints finish.
