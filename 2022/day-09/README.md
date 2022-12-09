# Day 9
This was a fun puzzle and Zig was a pleasure to use today. I'm trying not to let these recap posts just become rambling discussions about my specific implementation, but I'll try to hit a few important points.

## Testing Isolated Logic

The `head` and `tail` teminology was a huge clue that we're going to be working with lists, so I modelled my rope with a `Knot` struct that can `follow` another `Knot`.

```zig
const Knot = struct {
    x: isize = 0,
    y: isize = 0,

    pub fn follow(self: *Knot, other: Knot) void {
      // ...
    }
};
```

When faced with fiddly bits of logic, like deciding how a knot moves when pulled by another knot, I usually try to pull that logic out from the main solution functions so that it can be tested independently.

```zig
const Knot = struct {
    x: isize = 0,
    y: isize = 0,

    pub fn follow(self: *Knot, other: Knot) void {
      // ...
    }

    test "follows horizontally" {
        var tail = Knot{ .x = 0, .y = 0 };
        tail.follow(.{ .x = 2, .y = 0 });
        try testing.expectEqual(tail, .{ .x = 1, .y = 0 });
    }

    test "follows vertically" {
        var tail = Knot{ .x = 0, .y = 0 };
        tail.follow(.{ .x = 0, .y = 2 });
        try testing.expectEqual(tail, .{ .x = 0, .y = 1 });
    }

    test "follows diagonally" {
        var tail = Knot{ .x = 0, .y = 0 };
        tail.follow(.{ .x = 1, .y = 2 });
        try testing.expectEqual(tail, .{ .x = 1, .y = 1 });
    }
};
```

I usually write these kinds of tests as I implement logic, because testing is a faster feedback loop than running the program and checking debug messages. Running `zig test` on the current file is my default task in VSCode (see [tasks.json](../.vscode/tasks.json)) and I trigger that with <kbd>⌘+shift+b</kbd>.

If the function works and doesn't seem too prone to refactoring regressions, I'll often just remove these tests afterwards for the sake of readability.

## Parsing Iterators

With many of these puzzles, I find myself wishing Zig had generator functions, as writing iterators by hand is largely overkill. Here's what I wanted from a parse function today:

```zig
fn part1(input: []const u8) !usize {
    var motions = parse(input);
    while (motions.next()) |motion| {
        motion.dir == 'R';
        motion.steps = 4;
    }
}
```

Creating that iterator requires defining new struct type:

```zig
const Parser = struct {
    lines: std.mem.TokenIterator(u8),

    pub fn init(input: []const u8) Parser {
        return .{ .lines = mem.tokenize(u8, input, "\n") };
    }

    pub fn next(self: *Parser) ?struct { dir: u8, steps: u8 } {
        if (self.lines.next()) |line| {
            const dir = line[0];
            const steps = std.fmt.parseInt(usize, line[2..], 10) catch unreachable;
            return .{ .dir = dir, .steps = steps };
        } else return null;
    }
};

const parse = Parser.init;
```

I like that there's no magic when it comes to iterators with Zig, but I hate having to flatten function state into struct members for these kinds of custom iterators. Today it ended up being way simpler to just parse inline instead.

## Compile Time Constraints
Today's `simulate` function was the first time I actually used a `comptime` parameter in Advent of Code. This was necessary because I decided to store the knots in an array, which must be a fixed size known at compile time. I could have used a `std.SinglyLinkedList` to avoid this, but hey, `comptime` is fun.

Another nice thing about Zig's comptime semantics is that they allow you to apply more complex constraints than many type systems would allow for. In this case, ropes must be formed of at least two knots.

If we check that at runtime, everything compiles and we panic.

```zig
fn simulate(input: []const u8, comptime rope_len: usize) !usize {
    if (rope_len < 2) unreachable;
    // panic: reached unreachable code
}
```

However, we can use the `comptime` keyword again to make this into a compile error instead.

```zig
fn simulate(input: []const u8, comptime rope_len: usize) !usize {
    comptime if (rope_len < 2) unreachable;
    // error: reached unreachable code
}
```

The `@compileError` builtin even allows to customise the error we see from the compiler.

```zig
fn simulate(input: []const u8, comptime rope_len: usize) !usize {
    comptime if (rope_len < 2) @compileError("ropes must have length >= 2");
}
```
