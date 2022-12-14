# Day 14

Fun day making a falling sand simulator. Not a huge amount to say about this puzzle with regards to Zig other than it was the first day where the [debug build](https://ziglang.org/documentation/master/#Debug) of my solution had a noticeable pause before outputting the answer.

```sh
$ zig build-exe main.zig
$ time ./main
Part 1: 793
Part 2: 24166
./main  1.16s user 0.01s system 99% cpu 1.175 total
```

Let's see how Zig's other [build modes](https://ziglang.org/documentation/master/#Build-Mode) compare.

```sh
$ zig build-exe -O ReleaseFast main.zig && time ./main
Part 1: 793
Part 2: 24166
./main  0.05s user 0.00s system 16% cpu 0.338 total

$ zig build-exe -O ReleaseSafe main.zig && time ./main
Part 1: 793
Part 2: 24166
./main  0.07s user 0.00s system 20% cpu 0.353 total

$ zig build-exe -O ReleaseSmall main.zig && time ./main
Part 1: 793
Part 2: 24166
./main  0.15s user 0.00s system 35% cpu 0.424 total

$ zig build-exe main.zig && time ./main
Part 1: 793
Part 2: 24166
./main  1.15s user 0.00s system 78% cpu 1.467 total
```

`ReleaseFast` ends up making a pretty significant difference, but the real surprise here is quite how fast `ReleaseSafe` is by comparison, considering it still has most of the safety checks compiled in.

My solution stores tiles in an `AutoHashMap(Point, Tile)` which is never going to be as efficient as using a contiguous block of memory, but it does mean that the simulation is technically running on an infinite plane (the "i" in `isize` stands for infinite, right?) and I don't have to worry about negative coordinates being a problem.

`AutoHashMap` is a dynamic data structure, and therefore takes an allocator and will reallocate memory as it grows. One obvious performance boost to try would be to set its initial capacity.

```zig
try map.ensureTotalCapacity(0x10000);
```

This change takes the fast release time down from 0.07s to 0.04s.

The optimisation that I didn't have time to try was going to be use a static bitset instead of an hash map. This would involve translating all 2D coordinates into single integers that represent a bit index within the set. Given that we know sand can only move at most 1 x coordinate away from the span location, then we can infer that the min x coordinate will be `500 - floor_level`. Use that frame of reference to translate everything back to 0, 0, then treat the bit set as a row major order representation of the map. The nature of a bitset means that you'd have to treat sand and rock as the same, but I think you could work around that.

Anyway, keen to play with bitsets and more "zero allocation" days in the future. I'm also still looking for a good opportunity to play with `@Vector` too.
