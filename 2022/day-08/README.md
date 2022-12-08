# Day 8
This puzzle was basically an exercise in why it's useful for languages to have for-loops or some kind of range structure. Not in Zig!

I also spent a lot of time battling integer signedness. In Zig, all array indexing is done with `usize` values, which creates a problem with `while` loops that count downwards. 

Consider a loop which appears to count down from 5 to 0.

```zig
var x: usize = 5;
while (x >= 0): (x -= 1) {
  std.debug.print("{d}\n", .{x});
}
```

Because the continue statement `(x -= 1)` runs after each iteration, the final iteration of the loop where `x == 0` tries to make `x == -1`, which results in an integer overflow panic (`usize` is an unsigned type).

My first thought here was to use the [saturating arithmetic operators](https://ziglang.org/documentation/master/#Table-of-Operators), to prevent the overflow, but that ends up creating an infinite loop, because the value of `x` stays at `0` indefinitely.

```zig
var x: usize = 5;
while (x >= 0): (x -|= 1) {
  std.debug.print("{d}\n", .{x});
}
```

There are all sorts of ways you can get around this, but I wasted a lot of time just trying to work with `isize` coordinates instead, which either bleeds out into the rest of the program, or you have to aggresively `@intCast` stuff back to `usize` each time you index into an array. Eventually, I ended up just moving the continue condition out of the down-counting while loops into the inside of the loop instead.

```zig
var x: usize = 5 + 1;
while (x > 0) {
  x -= 1;
  std.debug.print("{d}\n", .{x});
}
```

What a mess. In hindsight, if I was going to solve this puzzle again, then I think I'd do it with an `AutoHashMap` using hashed `[2]isize` coordinates as keys, just embracing the negative numbers.
