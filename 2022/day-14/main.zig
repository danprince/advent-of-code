const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const AutoHashMap = std.AutoHashMap;

const Tile = enum { air, rock, sand };
const Point = struct { x: isize, y: isize };
const TileMap = AutoHashMap(Point, Tile);

fn parsePoint(input: []const u8) !Point {
    var parts = mem.tokenize(u8, input, ",");
    var x = try std.fmt.parseInt(isize, parts.next().?, 10);
    var y = try std.fmt.parseInt(isize, parts.next().?, 10);
    return .{ .x = x, .y = y };
}

fn parseMap(input: []const u8, allocator: Allocator) !TileMap {
    var map = TileMap.init(allocator);
    var lines = mem.tokenize(u8, input, "\n");

    while (lines.next()) |line| {
        var parts = mem.tokenize(u8, line, " -> ");
        var cursor = try parsePoint(parts.next().?);

        while (parts.next()) |str| {
            var end = try parsePoint(str);

            while (true) {
                try map.put(cursor, .rock);
                if (cursor.x == end.x and cursor.y == end.y) break;
                cursor.x += std.math.sign(end.x - cursor.x);
                cursor.y += std.math.sign(end.y - cursor.y);
            }
        }
    }

    return map;
}

fn findLowestY(map: *TileMap) isize {
    var y: isize = 0;
    var iter = map.keyIterator();
    while (iter.next()) |p| {
        if (p.y > y) y = p.y;
    }
    return y;
}

fn simulateFallingSand(map: *TileMap, floor_level: isize, floor_tile: Tile) !bool {
    var sand = Point{ .x = 500, .y = 0 };
    while (true) {
        var down = Point{ .x = sand.x, .y = sand.y + 1 };
        var left = Point{ .x = sand.x - 1, .y = sand.y + 1 };
        var right = Point{ .x = sand.x + 1, .y = sand.y + 1 };
        var default = if (sand.y == floor_level - 1) floor_tile else .air;

        if (map.get(sand) orelse .air != .air) {
            return false; // The map is stable if the sand is blocked where it is.
        } else if (sand.y > floor_level) {
            return false; // The map is stable if sand falls below the floor level.
        } else if (map.get(down) orelse default == .air) {
            sand = down;
        } else if (map.get(left) orelse default == .air) {
            sand = left;
        } else if (map.get(right) orelse default == .air) {
            sand = right;
        } else {
            try map.put(sand, .sand);
            return true; // The sand stopped here, the map is not stable.
        }
    }
}

fn part1(input: []const u8, allocator: Allocator) !usize {
    var map = try parseMap(input, allocator);
    defer map.deinit();
    const floor_level = findLowestY(&map);
    var iterations: usize = 0;
    while (try simulateFallingSand(&map, floor_level, .air)) iterations += 1;
    return iterations;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    var map = try parseMap(input, allocator);
    defer map.deinit();
    try map.ensureTotalCapacity(0x10000);
    const floor_level = findLowestY(&map) + 2;
    var iterations: usize = 0;
    while (try simulateFallingSand(&map, floor_level, .rock)) iterations += 1;
    return iterations;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, testing.allocator), 24);
    try testing.expectEqual(part2(example, testing.allocator), 93);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input, testing.allocator), 793);
    try testing.expectEqual(part2(input, testing.allocator), 24166);
}
