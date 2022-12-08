const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;

const Trees = struct {
    /// This grid of heights includes the \n characters from the inputs, so
    /// be careful not to interpret them as trees.
    heights: []const u8,
    cols: usize,
    rows: usize,

    pub fn at(self: *const Trees, x: usize, y: usize) u8 {
        return self.heights[x + y * self.cols];
    }

    pub fn parse(input: []const u8) Trees {
        const cols = (mem.indexOf(u8, input, "\n") orelse 0) + 1;
        const rows = mem.count(u8, input, "\n");
        return .{ .heights = input, .cols = cols, .rows = rows };
    }
};

fn part1(input: []const u8) usize {
    const trees = Trees.parse(input);
    var visible_trees: usize = 0;

    for (input) |tree, index| {
        if (tree == '\n') continue;
        const x = @mod(index, trees.cols);
        const y = index / trees.cols;
        var blocked: usize = 0;

        var x0 = x;
        while (x0 > 0) {
            x0 -= 1;
            if (trees.at(x0 - 1, y) >= tree) {
                blocked += 1;
                break;
            }
        }

        var x1 = x + 1;
        while (x1 < trees.cols - 1): (x1 += 1) {
            if (trees.at(x1, y) >= tree) {
                blocked += 1;
                break;
            }
        }

        var y0 = y;
        while (y0 > 0) {
            y0 -= 1;
            if (trees.at(x, y0) >= tree) {
                blocked += 1;
                break;
            }
        }

        var y1 = y + 1;
        while (y1 < trees.rows): (y1 += 1) {
            if (trees.at(x, y1) >= tree) {
                blocked += 1;
                break;
            }
        }

        if (blocked < 4) {
            visible_trees += 1;
        }
    }

    return visible_trees;
}

fn part2(input: []const u8) usize {
    const trees = Trees.parse(input);
    var best_score: usize = 0;

    for (trees.heights) |tree, index| {
        const x = @mod(index, trees.cols);
        const y = index / trees.cols;
        var dist_left: usize = 0;
        var dist_right: usize = 0;
        var dist_above: usize = 0;
        var dist_below: usize = 0;

        var x0 = x;
        while (x0 > 0) {
            x0 -= 1;
            dist_left += 1;
            if (trees.at(x0, y) >= tree) {
                break;
            }
        }

        var x1 = x + 1;
        while (x1 < trees.cols - 1): (x1 += 1) {
            dist_right += 1;
            if (trees.at(x1, y) >= tree) {
                break;
            }
        }

        var y0 = y;
        while (y0 > 0) {
            y0 -= 1;
            dist_above += 1;
            if (trees.at(x, y0) >= tree) {
                break;
            }
        }

        var y1 = y + 1;
        while (y1 < trees.rows): (y1 += 1) {
            dist_below += 1;
            if (trees.at(x, y1) >= tree) {
                break;
            }
        }

        const score = dist_left * dist_right * dist_above * dist_below;
        if (score > best_score) best_score = score;
    }

    return best_score;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{part1(input)});
    print("Part 2: {d}\n", .{part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 21);
    try testing.expectEqual(part2(example), 8);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 1693);
    try testing.expectEqual(part2(input), 422059);
}
