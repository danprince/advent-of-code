# Day 11

These types of puzzles are my least favourite days of Advent of Code and today gave me flashbacks to the [Chinese Remainder Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem) day from last(?) year. My intuition for mathematical problem solving is significantly worse than my intuition for general programming problems, and it sucks to be stuck looking at a problem with no real idea of what to try for part 2. Of course, this was after I tried u64, then u128, then realised that Zig doesn't have any bigger ints.

So, this was the first day this year where I had to go in search of some hints, and when I did, I felt stupid for having not spent longer thinking about it. Obviously the numbers are going to overflow with 10k iterations when you're raising the worry levels to the power two with no division to help control growth.

On a happier note, I set off parsing the input with trusty old `std.mem.tokenize` then got part way through writing some particularly gnarly hardcoded index offsets, and decided screw it, let's just write a simple lexer instead. See [lex.zig](./lex.zig) for the implementation and a quick test suite.

Here's a quick example of how it works.

```zig
var l = lex.init("1: hello + world");
const digit = try l.int(usize, 10);
_ = try l.literal(": ")
```

I usually get sidetracked when writing parsers and in future I'd also like to experiment with implementing parser combinators, and translating this [Go lexer](https://www.youtube.com/watch?v=HxaD_trXwRE) to Zig. I think there's still more fun to be had with compile time parsing too. Should be in for a treat the next time the input is complex.
