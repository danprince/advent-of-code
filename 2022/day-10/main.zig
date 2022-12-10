const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;

const opcode_cycles = std.ComptimeStringMap(isize, .{
    .{ "addx", 2 },
    .{ "noop", 1 },
});

fn part1(input: []const u8) isize {
    var lines = mem.tokenize(u8, input, "\n");

    var cycles: isize = 1;
    var x: isize = 1;
    var signal_strengths: isize = 0;

    while (lines.next()) |line| {
        var parts = mem.tokenize(u8, line, " ");
        var opcode = parts.next().?;
        var required_cycles = opcode_cycles.get(opcode) orelse 0;

        while (required_cycles > 0) : ({
            required_cycles -= 1;
            cycles += 1;
        }) switch (cycles) {
            20, 60, 100, 140, 180, 220 => signal_strengths += cycles * x,
            else => {},
        };

        if (mem.eql(u8, opcode, "addx")) {
            x += std.fmt.parseInt(isize, parts.next().?, 10) catch unreachable;
        }
    }

    return signal_strengths;
}

const display_width = 40;
const display_height = 6;
const display_size = display_width * display_height;
const output_width = display_width + 1; // extra char for newlines
const output_height = display_height;
const output_size = output_width * output_height;

fn part2(input: []const u8) [output_size]u8 {
    var lines = mem.tokenize(u8, input, "\n");
    var display = [_]u8{'.'} ** display_size;

    var cycles: usize = 0;
    var x: isize = 1;

    while (lines.next()) |line| {
        var parts = mem.tokenize(u8, line, " ");
        var opcode = parts.next().?;
        var required_cycles = opcode_cycles.get(opcode) orelse 0;

        while (required_cycles > 0) : ({
            required_cycles -= 1;
            cycles += 1;
        }) {
            const cursor = @mod(cycles, display_width);
            if (cursor == x - 1 or cursor == x or cursor == x + 1) {
                display[cycles] = '#';
            }
        }

        if (mem.eql(u8, opcode, "addx")) {
            x += std.fmt.parseInt(isize, parts.next().?, 10) catch unreachable;
        }
    }

    var cursor: usize = 0;
    var row: usize = 0;
    var output = [_]u8{'.'} ** output_size;

    while (row < display_height) : (row += 1) {
        var src = display[row * display_width .. (row + 1) * display_width];
        var dst = output[cursor .. cursor + src.len];
        mem.copy(u8, dst, src);
        cursor += src.len;
        output[cursor] = '\n';
        cursor += 1;
    }

    return output;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {!}\n", .{part1(input)});
    print("Part 2: {!}\n", .{part2(input)});
}

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
        \\
    );
}

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
        \\
    );
}
