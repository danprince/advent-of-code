const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const Point = struct { x: isize, y: isize };

const von_neumann_neighbours = [_]Point{
    .{ .x = 0, .y = -1 },
    .{ .x = -1, .y = 0 },
    .{ .x = 0, .y = 1 },
    .{ .x = 1, .y = 0 },
};

const PriorityItem = struct {
    point: Point,
    priority: usize,
    fn compare(_: void, self: PriorityItem, other: PriorityItem) std.math.Order {
        return std.math.order(self.priority, other.priority);
    }
};

/// This is a "downhill" Dijkstra map because instead of starting from the "S"
/// tile, it starts from the "E" tile and walks downwards to find the shortest
/// path from the end to every other path in the map.
const DownhillDijkstraMap = struct {
    const Self = @This();

    heights: std.AutoHashMap(Point, u8),
    distances: std.AutoHashMap(Point, usize),
    start: Point,
    end: Point,

    pub fn init(input: []const u8, allocator: Allocator) !Self {
        var start: ?Point = null;
        var end: ?Point = null;

        var heights = std.AutoHashMap(Point, u8).init(allocator);
        var distances = std.AutoHashMap(Point, usize).init(allocator);

        var frontier = std.PriorityQueue(PriorityItem, void, PriorityItem.compare).init(allocator, {});
        defer frontier.deinit();

        const input_width = mem.indexOf(u8, input, "\n").? + 1;

        for (input) |char, index| {
            const point = Point{
                .x = @intCast(isize, @mod(index, input_width)),
                .y = @intCast(isize, index / input_width),
            };

            if (char == '\n') continue;
            if (char == 'E') end = point;
            if (char == 'S') start = point;

            try heights.put(point, switch (char) {
                'S' => 'a',
                'E' => 'z',
                else => char,
            });
        }

        if (end) |target_point| {
            try distances.put(target_point, 0);
            try frontier.add(.{ .point = target_point, .priority = 0 });
        }

        while (frontier.count() > 0) {
            const current_item = frontier.remove();
            const current = current_item.point;
            const current_height = heights.get(current).?;
            const current_cost = distances.get(current).?;

            for (von_neumann_neighbours) |n| {
                const next = Point{ .x = current.x + n.x, .y = current.y + n.y };
                const new_cost = current_cost + 1;
                const next_cost = distances.get(next);
                const next_height = heights.get(next);

                // Check whether this neighbour is even in the map
                if (next_height == null) continue;

                // Check if this step would be valid. Remember, we're starting
                // at the target, not the start, which means that we have to
                // reverse the original logic.
                if (current_height > next_height.? + 1) continue;

                // Check if this step would be an improvement
                if (next_cost == null or new_cost < next_cost.?) {
                    try distances.put(next, new_cost);
                    try frontier.add(.{ .point = next, .priority = new_cost });
                }
            }
        }

        return .{
            .heights = heights,
            .distances = distances,
            .start = start orelse @panic("Map did not contain a start position (S)"),
            .end = end orelse @panic("Map did not contain an end position (E)"),
        };
    }

    pub fn deinit(self: *Self) void {
        self.heights.deinit();
        self.distances.deinit();
    }

    pub fn shortestPathToPoint(self: *Self, target: Point) ?usize {
        return self.distances.get(target);
    }

    pub fn shortestPathToHeight(self: *Self, target: u8) ?usize {
        var heights_iter = self.heights.iterator();
        var min_dist: usize = std.math.maxInt(usize);

        while (heights_iter.next()) |entry| {
            const point = entry.key_ptr.*;
            const height = entry.value_ptr.*;
            const dist = self.distances.get(point) orelse min_dist;
            if (height == target and dist < min_dist) min_dist = dist;
        }

        return min_dist;
    }
};

fn part1(input: []const u8, allocator: Allocator) !usize {
    var map = try DownhillDijkstraMap.init(input, allocator);
    defer map.deinit();
    return map.shortestPathToPoint(map.start).?;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    var map = try DownhillDijkstraMap.init(input, allocator);
    defer map.deinit();
    return map.shortestPathToHeight('a').?;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, std.testing.allocator), 31);
    try testing.expectEqual(part2(example, std.testing.allocator), 29);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input, std.testing.allocator), 481);
    try testing.expectEqual(part2(input, std.testing.allocator), 480);
}
