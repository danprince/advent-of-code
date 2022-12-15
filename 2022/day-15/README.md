# Day 15
Feels like it's been a grid heavy year. My 8AM brain once again thought it was a good idea to brute force this one, without realising that would involve around 12 billion manhattan distance computations. The tests ran for a while before I did the maths and decided that waiting wasn't the approach.

I was tempted to break out the lexer from day 11 to parse the input, but realised I could take some tokenizing shortcuts by treating `" =,:"` as the separator bytes. This leaves only words and numbers in the token stream, which is far easier to parse.

```zig
var parts = mem.tokenize(u8, line, " =,:");
for (range(3)) |_| _ = parts.next();
const sensor_x = try std.fmt.parseInt(isize, parts.next().?, 10);
for (range(1)) |_| _ = parts.next();
const sensor_y = try std.fmt.parseInt(isize, parts.next().?, 10);
for (range(5)) |_| _ = parts.next();
const beacon_x = try std.fmt.parseInt(isize, parts.next().?, 10);
for (range(1)) |_| _ = parts.next();
const beacon_y = try std.fmt.parseInt(isize, parts.next().?, 10);
```

This relies on a little `range` function that I spotted on one of my early attempts to find a traditional `for` loop in Zig.

```zig
fn range(comptime n: usize) [n]void {
    return [_]void{{}} ** n;
}
```

Zig's `for` loop iterates over any indexable collection with a length (arrays, slices, etc) and this function creates a comptime sized array of [`void`](https://ziglang.org/documentation/master/#void) (a zero bit type).

This got me interested in whether the size of the range array in question will affect the size of the stack frame at runtime, or whether the compiler is smart enough to unroll that loop automatically.

Let's consider the following program:

```zig
fn range(comptime n: usize) [n]void {
    return [_]void{{}} ** n;
}

pub fn main() u8 {
    var x: u8 = 0;
    for (range(10)) |_| x += 8;
    return x;
}
```

We're essentially hoping the compiler turns this into:

```zig
pub fn main() u8 {
    var x: u8 = 0;
    x += 8;
    x += 8;
    // ... 8 more times
    return x;
}
```

Running `zig build-exe -O ReleaseSmall -femit-asm=range.asm range.zig` will produce some assembly to investigate.

Turns out the compiler did even better. The whole loop has been flattened into a single `mov w0 #80` instruction.

In fact, the emitted assembly is identical to the assembly for this program.

```zig
pub fn main() u8 {
    var x: u8 = 0;
    var i: u8 = 0;

    while (i < 10) : (i += 1) {
        x += 8;
    }

    return x;
}
```

So, `range` is a nifty trick to have when writing repetitive code. Because arrays have fixed sizes, the `range` function can only accept comptime values.

Initially I got a little bit stuck with part 2. After realising it wasn't feasible to check every coordinate in the allowed range, I tried checking just the coordinates in the rectangles around each sensor, but this actually ended up being slower, because many of those rectangles intersect, meaning you end up doing redundant work. That should have been obvious, because we know there's only 1 coordinate this is not checked.

Eventually I realised that the undetectable coordinate must be directly adjacent to the boundary of at least one of the sensor detection ranges, and eventually I got a solution that actually finished by checking each tile around the boundaries of each sensor network.

In debug mode the solution is still pretty slow, but with `ReleaseFast` I got it down to roughly 600ms, which is good enough.
