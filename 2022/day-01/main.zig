const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const parseInt = std.fmt.parseInt;

fn part1(input: []const u8) !u32 {
    var lines = std.mem.split(u8, input, "\n");
    var max: u32 = 0;
    var curr: u32 = 0;

    while (lines.next()) |line| {
        if (line.len == 0) {
            if (curr > max) max = curr;
            curr = 0;
        } else {
            curr += try parseInt(u32, line, 10);
        }
    }

    return max;
}

fn part2(input: []const u8) !u32 {
    var lines = std.mem.split(u8, input, "\n");
    var curr: u32 = 0;
    var values = [_]u32{0} ** 3;

    while (lines.next()) |line| {
        if (line.len > 0) {
            curr += try parseInt(u32, line, 10);
        } else {
            for (values) |item, index| {
                if (curr < item) continue;
                values[index] = curr;
                curr = item;
            }
            curr = 0;
        }
    }

    var total: u32 = 0;
    for (values) |value| total += value;
    return total;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{try part1(input)});
    print("Part 2: {d}\n", .{try part2(input)});
}

test "example" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(try part1(example), 24000);
    try testing.expectEqual(try part2(example), 45000);
}

test "input" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(try part1(input), 71124);
    try testing.expectEqual(try part2(input), 204639);
}
