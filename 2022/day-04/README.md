# Day 4
Nothing too radical in terms of today's solution. I thought Zig might have a `Range` construct like Nim's `HSlice`, but what I was remembering was the `Range` struct from `std.bit_sets`.

Instead, I feel like writing up some notes on my approach to error handling in Zig so far.

Zig encodes errors in return values. This isn't anything groundbreaking, but there are excellent mechanisms for dealing with them using the capturing mechanisms of `if` and `else`, with additional syntax sugar from `try` and `catch` (not comparable to `try`/`catch` in other languages). This creates some interesting dynamics when deciding how to handle parsing errors with Advent of Code, and over the years I suppose I've tried 3 broad strategies.

#### Lazy Parsing
This involves failing silently and returning a zero value. I can't actually remember if I have done this for Advent of Code, but I know I have used it in the past. It tends to be easy to write and easy to use, but it is also a brilliant way to make your programs hard to debug.

Here's the same code written with this style.

```zig
pub fn parse(str: []const u8) Range {
    var parts = std.mem.tokenize(u8, str, "-");
    const start = std.fmt.parseInt(u8, parts.next() orelse "0", 10) catch 0;
    const end = std.fmt.parseInt(u8, parts.next() orelse "0", 10) catch 0;
    return Range{ .start = start, .end = end };
}
```

If anything goes wrong when retrieving values from the iterator, we fallback to the string `"0"` and if anything goes wrong whilst parsing the integers we fall back to the value `0` for both values in the range. Callers don't ever have to worry about errors or potential panics.

#### Best Parsing
This usually involves a tolerant approach to the input, and producing high quality, contextual errors at the point of failure conditions. This is how everyone (hopefully) would write parsing code designed to run in the wild. For Advent of Code, this is usually overkill, because the inputs are well formed and parsing is (usually) only a small portion of each challenge. You also have to explicitly decide what to do with the errors in Zig. Do you propagate them all the way back up to `main`? And if not, where do you decide to terminate them?

Here's an example of today's parsing logic rewritten in this style. We take extra care to be sure that 

```zig
const ParseError = error {
  CouldNotParse,
  InvalidRange,
};

pub fn parse(str: []const u8) !Range {
    var parts = std.mem.tokenize(u8, str, "-");

    if (parts.next()) |left| {
      if (parts.next()) |right| {
        const start = try std.fmt.parseInt(u8, left, 10);
        const end = try std.fmt.parseInt(u8, right, 10);
        if (start > end) return ParseError.InvalidRange;
        return Range{ .start = start, .end = end };
      }
    }

    return ParseError.CouldNotParse;
}
```

We have to do an number of additional steps to safely capture the iterator results, handle potential errors from `parseInt`, and validate the range itself.

#### Loud Parsing
The strategy I tend to use for Advent of Code, is to fail fast and fail loudly. Panics in Zig don't need to be propagated like errors, instead they crash your entire program, which is unacceptable for many other usecases.

Here's the snippet again, in this style:

```zig
pub fn parse(str: []const u8) Range {
    var parts = std.mem.tokenize(u8, str, "-");
    const start = std.fmt.parseInt(u8, parts.next().?, 10) catch unreachable;
    const end = std.fmt.parseInt(u8, parts.next().?, 10) catch unreachable;
    return Range{ .start = start, .end = end };
}
```

In Zig, this often involves a lot of `unreachable` statements (`.?` is a shorthand for `orelse unreachable`) and reaching an unreachable statement causes a panic. There's nothing the caller can do (as far as I know) to recover once we hit the failure branch, which makes this function fairly inflexible, compared to a version which returns errors.

## VSCode & ZLS
Continuing from yesterday's struggles with VSCode and ZLS, I spent some time trying to figure out why I was unable to build ZLS from source. The error seemed to be coming from Zig itself though, rather than from the ZLS codebase. I experimented with rolling back to the stable release of Zig (0.10.0) but then discovered that ZLS won't compile with anything less than a fairly recent patch version of 0.11.0. So I recompiled Zig from `HEAD` and went again, and voila, a nice shiny `zls` executable appeared.
