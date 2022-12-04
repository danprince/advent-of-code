const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

const Range = struct {
    start: u8,
    end: u8,

    /// Parse a range from a string in the form "N-M" where N is the start and M is the end.
    pub fn parse(str: []const u8) Range {
        var parts = std.mem.tokenize(u8, str, "-");
        const start = std.fmt.parseInt(u8, parts.next().?, 10) catch unreachable;
        const end = std.fmt.parseInt(u8, parts.next().?, 10) catch unreachable;
        return Range{ .start = start, .end = end };
    }

    /// Check whether the range includes a given integer.
    pub fn includes(self: *const Range, number: u8) bool {
        return number >= self.start and number <= self.end;
    }

    pub fn contains(self: *const Range, other: Range) bool {
        return (self.start <= other.start and self.end >= other.end);
    }

    /// Check whether these ranges overlap.
    pub fn overlaps(self: *const Range, other: Range) bool {
        return self.includes(other.start) or self.includes(other.end) or other.includes(self.start) or other.includes(self.end);
    }
};

fn part1(input: []const u8) u32 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var count: u32 = 0;
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(u8, line, ",");
        const elf1 = Range.parse(parts.next().?);
        const elf2 = Range.parse(parts.next().?);
        if (elf1.contains(elf2) or elf2.contains(elf1)) count += 1;
    }
    return count;
}

fn part2(input: []const u8) u32 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var count: u32 = 0;
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(u8, line, ",");
        const elf1 = Range.parse(parts.next().?);
        const elf2 = Range.parse(parts.next().?);
        if (elf1.overlaps(elf2)) count += 1;
    }
    return count;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{part1(input)});
    print("Part 2: {d}\n", .{part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 2);
    try testing.expectEqual(part2(example), 4);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 569);
    try testing.expectEqual(part2(input), 936);
}

const ParseError = error {
  CouldNotParse,
};
