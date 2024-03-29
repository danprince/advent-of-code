Day 1 was the typical check that [you can actually program](https://blog.codinghorror.com/why-cant-programmers-program/). Both parts of the problem were scans to see how fast the depth increased below the submarine.

For the first part, I used a simple loop, starting from 1 and checking the current and previous positions to see if there had been an increase.

```nim
proc part1(input: string): int =
  let lines = splitLines(input)
  let xs = map(lines, parseInt)
  for i in 1 .. xs.len - 1:
    let x = xs[i]
    let y = xs[i - 1]
    if x > y: result += 1
```

I tend to implement my solutions as two separate functions which both take the input string (rather than a parsed version of it), even when means doing more work in each part. This makes them easier to test against the example inputs, which I tend to do with inline tests or assertions in every puzzle.

```nim
assert part1("199\n200\n208\n210\n200\n207\n240\n269\n260\n263") == 7
assert part2("199\n200\n208\n210\n200\n207\n240\n269\n260\n263") == 5

const input = slurp("input.txt")
echo "Part 1: ", part1(input)
echo "Part 2: ", part2(input)
```

The [`assert` template](https://nim-lang.org/docs/assertions.html#assert.t%2Cuntyped%2Cstring) is a great first look at Nim's metaprogramming capabilities. It looks like a keyword, but it's actually just a template. If an assertion fails, the template has full access to the syntax tree, and can print the source for the expression that failed, rather than just a generic error message.

Assertions can also be compiled out of the resulting executable with the `--assertions:off` flag to optimise your programs.

Part 2 didn't throw us any curveballs. This time we had to find whether the sum of the current window of three had increased from the previous window.

```nim
proc part2(input: string): int =
  let lines = splitLines(input)
  let xs = map(lines, parseInt)
  for i in 1 .. xs.len - 3:
    let x = xs[i + 0] + xs[i + 1] + xs[i + 2]
    let y = xs[i - 1] + xs[i + 0] + xs[i + 1]
    if x > y: result += 1
```

I didn't try to do anything clever here (although I vaguely remember learning about efficient ways to do sliding windows in last year's [Day 9](https://adventofcode.com/2020/day/9)). It seemed easiest to just repurpose the code from part one.

Feels good to be learning!
