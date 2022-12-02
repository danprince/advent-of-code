# Day 2
Much quicker to get going today after working through a decent amount of [Ziglings](https://github.com/ratfactor/ziglings) yesterday.

The puzzle seemed ideal for enums, as we have three distinct signs (rock, paper and scissors) and (lose, draw, win). This creates some additional parsing overhead, but the code becomes more readable and seeing as we're doing a decent amount of switching to implement the rules, the exhaustive checks are nice (compared to just comparing raw characters).

It's also nice that you can put functions directly into enums with the same semantics as structs. This is a genius move, as it prevents a whole class of name collisions when you would otherwise need to encode the enum/struct name into its supporting functions. `parseSign` becomes `Sign.parse`. Even better, Zig has some flavour of [UFCS](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax) which means you can call these methods "statically" (e.g. `Sign.compare(a, b)`) or in method position (e.g. `a.compare(b)`).

I tried for a while to think of a smarter way to encode the rock paper scissors rules, and in other languages, I would have been more tempted to store them all in constant tables. However, I think small switch functions are probably a more natural fit for Zig.

For a low-level language, you can write some suprisingly elegant code:

```zig
const player = switch (outcome) {
    .win => opponent.loses(),
    .lose => opponent.wins(),
    .draw => opponent,
};
```

Parsing was straightforward today too and swapping to `std.mem.tokenize` has the nice advantage of ignoring the trailing newline at the end of the input file, without having to trim it first.

Overall, I ended up writing more code than I was expecting to, and I suspect you could golf it right back down by comparing characters directly, but the end result would be hard to interpret.

I often arrive at the correct puzzle input before I'm happy with the code, and I've decided to start including input regression tests, to make it easier to check that whilst I haven't broken the examples _or_ my own input, whilst I refactor.
