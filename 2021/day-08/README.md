This was a great puzzle! Easily my favourite so far this year, but also the one where I was the least satisfied with my code at the end.

If you haven't already, I'd recommend [reading the full puzzle description](https://adventofcode.com/2021/day/8). I won't be able to summarise it effectively here.

## Part 1

We're given the hint that some of the digits have a unique number of segments, so it's always possible to recognise them, despite the jumbled wires.

These were `1`, `4`, `7`, and `8` (having 2, 4, 3 and 7 segments respectively).

```txt
  1:       4:      7:      8: 
 ....     ....    aaaa    aaaa 
.    c   b    c  .    c  b    c
.    c   b    c  .    c  b    c
 ....     dddd    ....    dddd 
.    f   .    f  .    f  e    f
.    f   .    f  .    f  e    f
 ....     ....    ....    gggg 
```

All we needed to do was parse the input and count the times one of these four digits appeared in the display output values.

```nim
import std/[strutils, sequtils]

type
  Digit = set[char]

  Sample = tuple
    values: seq[Digit]
    outputs: seq[Digit]
```

Each digit is a string of distinct characters, where each character corresponds to a display segment. Storing those characters in a set will make it easier to compare them later on.

I opted for `set` over `HashSet` because `char` is one of the types it supports and it has a literal syntax, which makes inline assertions and checks a little bit easier. The only downside is that there is no [`toHashSet`](https://nim-lang.org/docs/sets.html#toHashSet%2CopenArray%5BA%5D) equivalent function, so I had to write my own.

```nim
proc toSet(str: string): set[char] =
  for c in str: result.incl(c)
```

Finally, here's the logic for parsing samples and counting occurrences.

```nim
proc parseSample(str: string): Sample =
  let parts = str.split(" | ")
  let values = parts[0].split(" ").map(toSet)
  let outputs = parts[1].split(" ").map(toSet)
  (values, outputs)

proc parseSamples(input: string): seq[Sample] =
  input.splitLines.map(parseSample)

proc part1(input: string): int =
  for sample in parseSamples(input):
    for output in sample.outputs:
      case output.len:
        of 2, 4, 3, 7:
          result += 1
        else:
          discard
```

## Part 2
Here's where the real puzzle starts. We need to decode the output values using the information from the first part of the puzzle.

Here's the example display:

```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

Let's encode the example values as sets and solve it step-by-step in Nim.

```nim
let values = @[
  {'a', 'b'},
  {'a', 'b', 'd'},
  {'a', 'b', 'e', 'f'},
  {'b', 'c', 'd', 'e', 'f'},
  {'a', 'c', 'd', 'f', 'g'},
  {'a', 'b', 'c', 'd', 'f'},
  {'a', 'b', 'c', 'd', 'e', 'f'},
  {'b', 'c', 'd', 'e', 'f', 'g'},
  {'a', 'b', 'c', 'd', 'e', 'g'},
]
```

We can identify some of the digits immediately, given what we know about digits with unique numbers of segments.

```nim
var digitsByLen: Table[int, Digit]

for value in values:
  digitsByLen[value.len] = value

let
  one = digitsByLen[1]
  seven = digitsByLen[3]
  four = digitsByLen[4]
  eight = digitsByLen[7]
```

From visual inspection, I noticed that `nine` will be the only 6-segment digit that contains all of the segments from `four` and `seven`.

Or more formally, `four` and `seven` are _subsets_ of `nine`.

```nim
var nine: Digit

for value in values:
  if value.len == 6 and four < value and seven < value:
    nine = value

# nine == {'a', 'b', 'c', 'd', 'e', 'f'}
```

The `top` segment is in `seven` but not `one`.

```nim
let top = seven - one
# {'d'}
```

The `bottom` segment is in `nine`, but not `four` or `seven`.

```nim
let bottom = nine - four - seven
# {'c'}
```

The `bottomLeft` segment is in `eight` but not `nine`.

```nim
let bottomLeft = eight - nine
# {'g'}
```

`two` is the only 5-segment digit that has a `bottomLeft` segment.

```nim
var two: Digit

for value in values:
  if value.len == 5 and bottomLeft < value:
    two = value

# two == {'a', 'c', 'd', 'e', 'g'}
```

The `topRight` segment is in both `one` and `two`.

```nim
let topRight = one * two
# {'a'}
```

The `bottomRight` segment is the segment in `one` that is not `topRight`.

```nim
let bottomRight = one - topRight
# {'b'}
```

The `middle` segment is the segment in `two` that is not `top`, `topRight`, `bottomLeft`, or `bottom`.

```nim
let middle = two - top - topRight - bottom - bottomLeft
# {'e'}
```

And finally, `topLeft` is the segment in `four` that is not `topRight`, `middle` or `bottomRight`.

```nim
let topLeft = four - topRight - middle - bottomRight
# {'f'}
```

Now we can identify the other digits.

```nim
let
  zero = eight - middle
  three = eight - topLeft - bottomLeft
  five = eight - topRight - bottomLeft
  six = eight - topRight
```

We can verify that our solution matches the mapping from the description.

```nim
#  dddd
# e    a
# e    a
#  ffff
# g    b
# g    b
#  cccc

assert top == {'d'}
assert topLeft == {'e'}
assert topRight == {'a'}
assert middle == {'f'}
assert bottomLeft == {'g'}
assert bottomRight == {'b'}
assert bottom == {'c'}
```

That was a lot of code, but we're not quite done!

We still need to decode the output value for each sample.

```nim
proc decodeOutput(sample: Sample): int =
  let (values, outputs) = sample

  # ... (reuse the code we wrote to solve the example) ...

  let mappings = {
    zero: '0', one: '1', two: '2', three: '3', four: '4',
    five: '5', six: '6', seven: '7', eight: '8', nine: '9'
  }.toTable

  outputs.mapIt(mappings[it]).join.parseInt
```

Then we can get our answer.

```nim
proc part2(input: string): int =
  for sample in parseSamples(input):
    result += decodeOutput(sample)
```

## Conclusion
This was one of the more memorable Advent of Code puzzles that I've done.

I largely figured out the pieces visually then turned those visual steps into code. I'm not sure that it'll be the most elegant solution, but it was satisfying to do.

Any language with sets will do alright here, but the operator overloads for Nim made expressing some of the logic much cleaner and clearer to me.

My original solution used `HashSet` and did a lot of extra work getting elements out of sets, then constructing new ones for subsequent operations. I try to avoid improving my code retroactively whilst learning, but I couldn't help but clean this up whilst writing the article.
