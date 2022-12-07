const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

fn parse(input: []const u8, allocator: Allocator) ![]usize {
    var dir_sizes = std.ArrayList(usize).init(allocator);
    defer dir_sizes.deinit();

    var sizes = std.ArrayList(usize).init(allocator);
    defer sizes.deinit();

    var lines = mem.tokenize(u8, input, "\n");

    while (lines.next()) |line| {
        if (mem.eql(u8, line, "$ cd ..")) {
            const size = sizes.pop();
            const parent_size = sizes.pop();
            try sizes.append(parent_size + size);
            try dir_sizes.append(size);
        } else if (mem.startsWith(u8, line, "$ cd")) {
            try sizes.append(0);
        } else if (std.ascii.isDigit(line[0])) {
            const space_index = mem.indexOf(u8, line, " ") orelse 0;
            const size_str = line[0..space_index];
            const file_size = try std.fmt.parseUnsigned(usize, size_str, 10);
            const parent_size = sizes.pop();
            try sizes.append(parent_size + file_size);
        }
    }

    // Now walk through the dirs we didn't already pop off of the stack to
    // calculate the sizes all the way up to the root dir.
    var child_size: usize = 0;

    while (sizes.items.len > 0) {
        const size = sizes.pop();
        child_size += size;
        try dir_sizes.append(child_size);
    }

    // Copy the sizes to a new slice so that the ArrayList can be freed.
    var total_sizes = try allocator.alloc(usize, dir_sizes.items.len);
    mem.copy(usize, total_sizes, dir_sizes.items);

    // Reverse the sizes so that the root is always first
    mem.reverse(usize, total_sizes);

    return total_sizes;
}

fn part1(input: []const u8, allocator: Allocator) !usize {
    var total_size: usize = 0;
    const dir_sizes = try parse(input, allocator);
    defer allocator.free(dir_sizes);

    for (dir_sizes) |size| {
        if (size <= 100_000) {
            total_size += size;
        }
    }

    return total_size;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    const disk_size = 70_000_000;
    const desired_space = 30_000_000;
    const dir_sizes = try parse(input, allocator);
    defer allocator.free(dir_sizes);
    const used_space = dir_sizes[0];
    const unused_space = disk_size - used_space;
    var freed_space: usize = disk_size;

    for (dir_sizes) |size| {
        if (unused_space + size > desired_space and size < freed_space) {
            freed_space = size;
        }
    }

    return freed_space;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const input = @embedFile("input.txt");
    print("Part 1: {!}\n", .{part1(input, allocator)});
    print("Part 2: {!}\n", .{part2(input, allocator)});
}

test "examples" {
    const example = @embedFile("example.txt");
    const allocator = std.testing.allocator;
    try testing.expectEqual(part1(example, allocator), 95437);
    try testing.expectEqual(part2(example, allocator), 24933642);
}

test "inputs" {
    const input = @embedFile("input.txt");
    const allocator = std.testing.allocator;
    try testing.expectEqual(part1(input, allocator), 2104783);
    try testing.expectEqual(part2(input, allocator), 5883165);
}
