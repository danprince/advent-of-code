# Day 10

Had a bit of a rough time with the second part of this puzzle doing modulus operations with numbers that I had deliberately offset by 1 to make other parts of the puzzle easier. Spent a long time looking at an output that was very nearly correct, but took forever to figure out why some pixels were skipped.

Time to talk about problems with low level strings. I represented my display with a contiguous array of chars (`#` and `.`) which makes it easy to write to, but is missing newline characters when printed to the screen.

This would have been a trivial change if I just needed to print the output to the screen. Print a row, print a newline, repeat til done. However, I wanted to test the output, so the function needs to return a value instead.

Ok, so I need a second mutable string (in Zig this is just an array of `u8`) that is big enough to hold all of the original string, with additional room for one newline character on each row. Not too big of a problem. The size of arrays in Zig needs to be known at compile time, so that they can be stored in the stack, but this wasn't a problem, because we're already given the dimensions of the display.

```zig
const display_width = 40;
const display_height = 6;
const display_size = display_width * display_height;

const output_width = display_width + 1; // extra char for newlines
const output_height = display_height;
const output_size = output_width * output_height;

var display = [_]u8{'.'} ** display_size;
var output = [_]u8{'.'} ** output_size;
```

Now for the trickier part. I need to iterate through the rows of the display, copying bytes from to the output, adding the newlines as we go, and making sure our indexes don't get confused.

```zig
var cursor: usize = 0;
var row: usize = 0;

while (row < display_height) : (row += 1) {
    var src = display[row * display_width .. (row + 1) * display_width];
    var dst = output[cursor .. cursor + src.len];
    mem.copy(u8, dst, src);
    cursor += src.len;
    output[cursor] = '\n';
    cursor += 1;
}
```

Zig often feels expressive enough to not feel too low level (I ported a codebase from [Python to Zig](https://github.com/danprince/zig-wasm-roguelike) yesterday and most of the code had little to no increase in complexity) but occasionally you have to do something with strings that would feel trivial in garbage collected languages (joining and trimming, in this case) and instead you find yourself messing around with raw memory.

There's one final problem with this output. It will not pass an equality test with the example, because it includes a trailing newline.

```zig
test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 13140);
    try testing.expectEqualStrings(&part2(example),
        \\##..##..##..##..##..##..##..##..##..##..
        \\###...###...###...###...###...###...###.
        \\####....####....####....####....####....
        \\#####.....#####.....#####.....#####.....
        \\######......######......######......####
        \\#######.......#######.......#######.....
    );
}
```

You might be tempted (I was) to do something like this:

```zig
var output = [_]u8{'.'} ** output_size;

// ...

return output[0..output.len-1];
```

But here be dragons. This syntax (`[N..M]`) creates a slice, which is pointer into the original array and the slice length. Once the function finishes executing, the stack frame is destroyed, all the local values (including the output array) are freed, our slice contains a dangling pointer. Print it out and you'll get garbage.

So a solution would instead to decrease the length of the output array, then to skip adding the `\n` on the final iteration of the loop. However, I have a better idea.

```diff
 test "inputs" {
     const input = @embedFile("input.txt");
     try testing.expectEqual(part1(input), 14520);
     try testing.expectEqualStrings(&part2(input),
         \\###..####.###...##..####.####...##.###..
         \\#..#....#.#..#.#..#....#.#.......#.#..#.
         \\#..#...#..###..#......#..###.....#.###..
         \\###...#...#..#.#.##..#...#.......#.#..#.
         \\#....#....#..#.#..#.#....#....#..#.#..#.
         \\#....####.###...###.####.####..##..###..
+        \\
     );
 }
```

