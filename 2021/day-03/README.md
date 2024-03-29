I saw binary strings, got excited, and immediately fell into the trap of reading up on Nim's [`bitops`][bitops] before I made a proper start on the first part of day 3.

Bitwise operations aren't commonly used in most programming languages, so it has always felt odd to me that many languages inherited C's single character operators (such as `^`, `|`, and `&`). Nim makes the sensible decision to use verbose names (`bitnot`, `bitor`, and `bitand`) _and_ keeps them in their own module, rather than including them in `system`.

The first part of the problem required us to find the most common bit in each position, across a list of binary strings.

```
00100
11110
10110
=
10110
```

Using `bitops` to turn the strings into numbers convoluted the logic with shifts and masks to check the bit for each position. I ended up switching to comparing strings/characters directly.

I started out with a `mostCommonBit` function that would return the most common bit for a sequence of strings, given a position.

```nim
proc mostCommonBit(numbers: seq[string], index: int): char =
  var ones = 0
  for number in numbers:
    if number[index] == '1':
      ones += 1
  let zeroes = numbers.len - ones
  if ones >= zeroes: '1' else: '0'
```

I haven't talked about it yet, but despite having borrowed a lot from Python's syntax, one place where Nim differs stylistically is with identifier names. [NEP-1](https://nim-lang.org/docs/nep1.html) suggests `camelCase` (rather than `snake_case`, like Python). I tend to gravitate towards more explicit names unless I'll need to type them lots of times, and I prefer that when they're broken with punctuation. 

The final solution calls `mostCommonBit` for each position, then parses the resulting string as a binary number. The other part of the problem requires us to find the least common bit for each position, which I did with an `invert` procedure.

```nim
proc invert(c: char): char =
  if c == '0': '1' else: '0'

proc invert(str: string): string =
  str.map(invert).join
```

Nim's call syntax starts to shine here. Here's how the same proc would look without it.

```nim
proc invert(str: string): string =
  join(map(str, invert))
```

Not terrible with simple expressions, but you have to retrain your brain to read and write inside-out instead.

## Part 2
The second part put a slightly more interesting spin on the problem. Using the same logic for finding most/least common bits, we'd find the average value, by iterating through the positions and discarding any values that _didn't_ match the most/least common bit in that position.

I made a mistake thinking that I could find the most common average value, then `invert` it, but quickly found out that the inversion logic doesn't hold true for this new variant of the problem (because the inversion isn't guaranteed to be in the list of numbers).

I ended up with two similar functions that bumped the `invert` operation one level lower, instead.

```nim
proc findMostCommon(numbers: seq[string]): string =
  let width = numbers[0].len
  var queue = numbers
  for i in 0 ..< width:
    let bit = mostCommonBit(queue, i)
    queue = queue.filterIt(it[i] == bit)
    if queue.len == 1:
      return queue[0]

proc findLeastCommon(numbers: seq[string]): string =
  let width = numbers[0].len
  var queue = numbers
  for i in 0 ..< width:
    let bit = mostCommonBit(queue, i).invert
    queue = queue.filterIt(it[i] == bit)
    if queue.len == 1:
      return queue[0]
```

I wasn't super happy with this duplication, but it felt like it would have been unnatural to support both behaviours with one parameterized procedure by comparison.

## It Templates

I haven't quite made up my mind about the [`It` templates](https://nim-lang.org/docs/sequtils.html#18) from `sequtils`.

```nim
let bit = mostCommonBit(queue, i).invert
queue = queue.filterIt(it[i] == bit)
if queue.len == 1:
  return queue[0]
```

This `filterIt` macro is essentially just a shorthand way to write a more verbose proc passing version of `filter`.

```nim
let bit = mostCommonBit(queue, i).invert
queue = queue.filter(proc (it: string): bool =
  it[i] == bit)
if queue.len == 1:
  return queue[0]
```

The syntax for inline procs isn't great, but this has only really shown up in the `sequtils` module so far. It doesn't particularly feel like Nim wants you to use a functional proc-passing style. Even the docs for [`filterIt`](https://nim-lang.org/docs/sequtils.html#filterIt.t%2Cuntyped%2Cuntyped) suggest using the [`collect`](https://nim-lang.org/docs/sugar.html#collect.m%2Cuntyped%2Cuntyped) macro instead.

```nim
queue = queue.filterIt(it[i] == bit)
# vs
collect:
  for str in queue:
    if str[i] == bit: str
```

For complex expressions I can see myself using `collect`. You lose a lot of meaning when you reduce a readable name to `it`, but the `collect` syntax is clunky for shorter expressions.

The `sugar` module also includes a [`=>` macro](https://nim-lang.org/docs/sugar.html#%3D%3E.m,untyped,untyped), which might be the ideal middleground for shorter expressions.

```nim
queue = queue.filter((str) => str[i] == bit)
```

Including this syntax in the sugar module (rather than being a part of the grammar) reinforces the idea that it's not really the preferred way to do things though. I'll keep experimenting!

[bitops]: https://nim-lang.org/docs/bitops.html
