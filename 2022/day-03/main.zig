const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

/// The largest character we'll ever need to store in a bitset.
const max_char_size = 'z' + 1;

/// Calculates the priority for a given character.
fn priority(ch: u8) u8 {
    if (ch >= 'a' and ch <= 'z') return ch - 96;
    if (ch >= 'A' and ch <= 'Z') return ch - 38;
    return ch;
}

test "priority" {
    try testing.expectEqual(priority('a'), 1);
    try testing.expectEqual(priority('z'), 26);
    try testing.expectEqual(priority('A'), 27);
    try testing.expectEqual(priority('Z'), 52);
}

/// Converts a string to a bitset, where the bit at the index of each character
/// is set to 1 and all other bits are set to 0.
fn itemsToBitSet(items: []const u8) std.StaticBitSet(max_char_size) {
    var set = std.StaticBitSet(max_char_size).initEmpty();
    for (items) |c| set.set(c);
    return set;
}

test "itemsToBitSet" {
    try testing.expect(itemsToBitSet("abc").isSet('a') == true);
    try testing.expect(itemsToBitSet("abc").isSet('z') == false);
}

fn part1(input: []const u8) u32 {
    var rucksacks = std.mem.tokenize(u8, input, "\n");
    var total: u32 = 0;

    while (rucksacks.next()) |rucksack| {
        const size = rucksack.len / 2;
        var front = rucksack[0..size];
        var back = itemsToBitSet(rucksack[size..]);
        for (front) |ch| {
            if (back.isSet(ch)) {
                total += priority(ch);
                break;
            }
        }
    }

    return total;
}

fn part2(input: []const u8) usize {
    var rucksacks = std.mem.tokenize(u8, input, "\n");
    var total: u32 = 0;

    while (rucksacks.next()) |rucksack| {
        const r2 = itemsToBitSet(rucksacks.next().?);
        const r3 = itemsToBitSet(rucksacks.next().?);

        for (rucksack) |ch| {
            if (r2.isSet(ch) and r3.isSet(ch)) {
                total += priority(ch);
                break;
            }
        }
    }

    return total;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{part1(input)});
    print("Part 2: {d}\n", .{part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 157);
    try testing.expectEqual(part2(example), 70);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 8243);
    try testing.expectEqual(part2(input), 2631);
}
