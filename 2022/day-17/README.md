# Day 17

After getting truly defeated by day 16, I have returned to carry on with the other puzzles. Tetris is a new one for advent of code, but it's a nice extension from the falling sand day.

My favourite novelty way to implement tetris is with nothing but big integers and bitwise operations (or regular integers if your grid dimensions will allow). That didn't feel like a good option here, because we don't have a bounded grid, and Zig doesn't natively support anything above `u128`.

Instead, I decided to use the next best thing. Static bit sets. This also runs into a problem with needing a bounded grid size, but I can tweak the bitset size if it overflows.

The actual implementation isn't all that interesting, but I would like to spend a moment appreciating Zig's facilities for compile time programming. We've are given a set of tetrominoes and turning them into bitsets imperatively would be awkward and error prone. Instead, I implemented a `blockFromStr` function.

```zig
const Block = std.bit_set.IntegerBitSet(16);

fn blockFromStr(str: []const u8) Block {
    var block = Block.initEmpty();
    var index: usize = 0;
    for (str) |c| {
        if (c == '\n') continue;
        if (c == '#') block.set(index);
        index += 1;
    }
    return block;
}
```

There's nothing obvious that signifies that this function is designed to be run at compile time, which is part of what makes the whole `comptime` model great. We could enforce this restriction by using `comptime str: []const u8` but there's no real need to.

If we call this function from any `comptime` scope (top level const/var assignments are automatically comptime) then the compiler (technically an interpreter inside the compiler) is going to execute this code and embed the result directly into the binary.

```zig
const block_2 = blockFromStr(
    \\.#..
    \\###.
    \\.#..
    \\....
);
```

So instead of shipping with a 19 byte string and calling this function on startup, `block_2` begins runtime as a bitset with the appropriate bits set. This isn't a revolutionary use of compile time programming, but it is something I find myself wanting surprisingly often, when I want to express ideas in ways that are friendly for humans, but would involve doing redundant work at runtime

## Part 2
I hate these kinds of extrapolation extensions in Advent of Code. I do these puzzles because I enjoy learning to program in different programming languages, and most of the fun is learning. These kinds of puzzles take all of the interest away from the implementation and force you to spend more of your time doing abstract thinking instead.

For what it's worth, it's got to be 
