const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

fn findDistinctPacket(input: []const u8, len: usize) usize {
    var i: usize = 0;
    search: while (i < input.len - len): (i += 1) {
        var set = [_]bool{false} ** 26;
        var j: usize = i;

        while (j < i + len): (j += 1) {
            const ch = input[j] - 'a';
            if (set[ch]) continue :search;
            set[ch] = true;
        }

        return i + len;
    } else unreachable;
}

fn part1(input: []const u8) usize {
    return findDistinctPacket(input, 4);
}

fn part2(input: []const u8) usize {
    return findDistinctPacket(input, 14);
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{part1(input)});
    print("Part 2: {d}\n", .{part2(input)});
}

test "examples" {
    try testing.expectEqual(part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
    try testing.expectEqual(part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
    try testing.expectEqual(part1("nppdvjthqldpwncqszvftbrmjlhg"), 6);
    try testing.expectEqual(part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
    try testing.expectEqual(part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);

    try testing.expectEqual(part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
    try testing.expectEqual(part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
    try testing.expectEqual(part2("nppdvjthqldpwncqszvftbrmjlhg"), 23);
    try testing.expectEqual(part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29);
    try testing.expectEqual(part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 1855);
    try testing.expectEqual(part2(input), 3256);
}
