const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const Point = @Vector(3, isize);

const von_neumann_neighbours = [_]Point{
    .{ -1, 0, 0 },
    .{ 1, 0, 0 },
    .{ 0, -1, 0 },
    .{ 0, 1, 0 },
    .{ 0, 0, -1 },
    .{ 0, 0, 1 },
};

fn part1(input: []const u8, allocator: Allocator) !usize {
    var points = std.AutoHashMap(Point, void).init(allocator);
    defer points.deinit();

    var coords = mem.tokenize(u8, input, "\n,");
    while (coords.peek()) |_| {
        var x = try std.fmt.parseInt(isize, coords.next().?, 10);
        var y = try std.fmt.parseInt(isize, coords.next().?, 10);
        var z = try std.fmt.parseInt(isize, coords.next().?, 10);
        try points.put(.{ x, y, z }, {});
    }

    var area: usize = 0;
    var cubes = points.keyIterator();
    while (cubes.next()) |pos| {
        for (von_neumann_neighbours) |step| {
            if (!points.contains(pos.* + step)) {
                area += 1;
            }
        }
    }
    return area;
}

const inf = std.math.maxInt(isize);

fn part2(input: []const u8, allocator: Allocator) !usize {
    var points = std.AutoHashMap(Point, void).init(allocator);
    defer points.deinit();
    var bounds_min = Point{ inf, inf, inf };
    var bounds_max = Point{ -inf, -inf, -inf };

    var coords = mem.tokenize(u8, input, "\n,");
    while (coords.peek()) |_| {
        var x = try std.fmt.parseInt(isize, coords.next().?, 10);
        var y = try std.fmt.parseInt(isize, coords.next().?, 10);
        var z = try std.fmt.parseInt(isize, coords.next().?, 10);
        var point = Point{ x, y, z };
        try points.put(point, {});
        bounds_min = @min(bounds_min, point);
        bounds_max = @max(bounds_max, point);
    }

    bounds_min -= Point{ 1, 1, 1 };
    bounds_max += Point{ 1, 1, 1 };

    // Floodfill the accessible space outside the sphere
    var external = std.AutoHashMap(Point, void).init(allocator);
    defer external.deinit();

    var stack = std.ArrayList(Point).init(allocator);
    defer stack.deinit();
    try stack.append(bounds_min);

    while (stack.items.len > 0) {
        var point = stack.pop();
        try external.put(point, {});
        for (von_neumann_neighbours) |step| {
            const neighbour = point + step;
            const gte_bounds_min = @reduce(.And, neighbour >= bounds_min);
            const lte_bounds_max = @reduce(.And, neighbour <= bounds_max);
            const visited = external.contains(neighbour);
            const blocked = points.contains(neighbour);
            if (gte_bounds_min and lte_bounds_max and !visited and !blocked) {
                try stack.append(neighbour);
            }
        }
    }

    var total_area: usize = 0;
    var cubes = points.keyIterator();
    while (cubes.next()) |pos| {
        for (von_neumann_neighbours) |step| {
            const neighbour = pos.* + step;
            const is_empty = !points.contains(neighbour);
            const is_external = external.contains(neighbour);
            if (is_empty and is_external) total_area += 1;
        }
    }

    return total_area;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1("1,1,1\n2,1,1", testing.allocator), 10);
    try testing.expectEqual(part1(example, testing.allocator), 64);
    try testing.expectEqual(part2(example, testing.allocator), 58);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input, testing.allocator), 4314);
    try testing.expectEqual(part2(input, testing.allocator), 2444);
}
