const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

fn part1(input: []const u8, allocator: Allocator) !usize {
    _ = allocator;
    _ = input;
    return 0;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    _ = allocator;
    _ = input;
    return 0;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, testing.allocator), 0);
    try testing.expectEqual(part2(example, testing.allocator), 0);
}

test "inputs" {
    //const input = @embedFile("input.txt");
    //try testing.expectEqual(part1(input, testing.allocator), 0);
    //try testing.expectEqual(part2(input, testing.allocator), 0);
}
