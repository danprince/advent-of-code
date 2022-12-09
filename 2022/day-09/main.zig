const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const math = std.math;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Knot = struct {
    x: isize = 0,
    y: isize = 0,

    pub fn follow(self: *Knot, other: Knot) void {
        const dx = other.x - self.x;
        const dy = other.y - self.y;
        const abs_dx = math.absInt(dx) catch unreachable;
        const abs_dy = math.absInt(dy) catch unreachable;

        if (abs_dx <= 1 and abs_dy <= 1) {
            // If points are equal/adjacent do nothing
        } else if (dx != 0 and dy != 0) {
            // If points are not in same col/row, move diagonally
            self.x += math.sign(dx);
            self.y += math.sign(dy);
        } else if (abs_dx > abs_dy) {
            self.x += math.sign(dx);
        } else if (abs_dy > abs_dx) {
            self.y += math.sign(dy);
        }
    }
};

fn simulate(input: []const u8, comptime rope_len: usize) !usize {
    comptime if (rope_len < 2) unreachable;

    var rope = [_]Knot{.{}} ** rope_len;
    var seen = std.AutoHashMap(Knot, usize).init(allocator);
    var head = &rope[0];
    var tail = &rope[rope_len - 1];
    var lines = mem.tokenize(u8, input, "\n");

    while (lines.next()) |line| {
        const dir = line[0];
        const steps = try std.fmt.parseInt(u8, line[2..], 10);

        var step: usize = 0;
        while (step < steps) : (step += 1) {
            switch (dir) {
                'L' => head.x -= 1,
                'R' => head.x += 1,
                'U' => head.y += 1,
                'D' => head.y -= 1,
                else => unreachable,
            }

            for (rope[1..]) |*knot, i| {
                knot.follow(rope[i]);
            }

            try seen.put(tail.*, 1);
        }
    }

    return seen.count();
}

fn part1(input: []const u8) !usize {
    return simulate(input, 2);
}

fn part2(input: []const u8) !usize {
    return simulate(input, 10);
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {!}\n", .{part1(input)});
    print("Part 2: {!}\n", .{part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 13);
    try testing.expectEqual(part2(example), 1);

    const example2 = @embedFile("example2.txt");
    try testing.expectEqual(part2(example2), 36);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 6044);
    try testing.expectEqual(part2(input), 2384);
}
