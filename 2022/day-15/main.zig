const std = @import("std");
const debug = @import("debug.zig");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const AutoHashMap = std.AutoHashMap;

fn range(comptime n: usize) [n]void {
    return [_]void{{}} ** n;
}

const Point = struct { x: isize, y: isize };

const Sensor = struct {
    x: isize,
    y: isize,
    range: isize,
};

const SensorNetwork = struct {
    sensors: std.ArrayList(Sensor),
    beacons: std.AutoHashMap(Point, void),
    bounds: struct { x1: isize, y1: isize, x2: isize, y2: isize },

    pub fn init(input: []const u8, allocator: Allocator) !SensorNetwork {
        const inf = std.math.maxInt(isize);

        var network = SensorNetwork{
            .sensors = std.ArrayList(Sensor).init(allocator),
            .beacons = std.AutoHashMap(Point, void).init(allocator),
            .bounds = .{ .x1 = inf, .y1 = inf, .x2 = 0, .y2 = 0 },
        };

        var lines = mem.tokenize(u8, input, "\n");
        while (lines.next()) |line| {
            var parts = mem.tokenize(u8, line, " =,:");
            for (range(3)) |_| _ = parts.next();
            const sensor_x = try std.fmt.parseInt(isize, parts.next().?, 10);
            for (range(1)) |_| _ = parts.next();
            const sensor_y = try std.fmt.parseInt(isize, parts.next().?, 10);
            for (range(5)) |_| _ = parts.next();
            const beacon_x = try std.fmt.parseInt(isize, parts.next().?, 10);
            for (range(1)) |_| _ = parts.next();
            const beacon_y = try std.fmt.parseInt(isize, parts.next().?, 10);

            const sensor_range_x = try std.math.absInt(beacon_x - sensor_x);
            const sensor_range_y = try std.math.absInt(beacon_y - sensor_y);
            const sensor_range = sensor_range_x + sensor_range_y;
            try network.sensors.append(.{ .x = sensor_x, .y = sensor_y, .range = sensor_range });
            try network.beacons.put(.{ .x = beacon_x, .y = beacon_y }, {});
            network.expand(sensor_x - sensor_range, sensor_y - sensor_range);
            network.expand(sensor_x + sensor_range, sensor_y + sensor_range);
        }

        return network;
    }

    pub fn deinit(self: *SensorNetwork) void {
        self.sensors.deinit();
        self.beacons.deinit();
    }

    fn expand(self: *SensorNetwork, x: isize, y: isize) void {
        if (x < self.bounds.x1) self.bounds.x1 = x;
        if (y < self.bounds.y1) self.bounds.y1 = y;
        if (x > self.bounds.x2) self.bounds.x2 = x;
        if (y > self.bounds.y2) self.bounds.y2 = y;
    }

    pub fn canDetect(self: *SensorNetwork, x: isize, y: isize) bool {
        return for (self.sensors.items) |sensor| {
            const distance_x = std.math.absInt(sensor.x - x) catch unreachable;
            const distance_y = std.math.absInt(sensor.y - y) catch unreachable;
            const distance = distance_x + distance_y;
            if (distance <= sensor.range) break true;
        } else false;
    }

    pub fn hasBeacon(self: *SensorNetwork, x: isize, y: isize) bool {
        return self.beacons.get(.{ .x = x, .y = y }) != null;
    }
};

fn part1(input: []const u8, row: isize, allocator: Allocator) !usize {
    var network = try SensorNetwork.init(input, allocator);
    defer network.deinit();

    var count: usize = 0;
    var y = row;

    var x = network.bounds.x1;
    while (x <= network.bounds.x2) : (x += 1) {
        if (network.hasBeacon(x, y)) continue;
        if (!network.canDetect(x, y)) continue;
        count += 1;
    }

    return count;
}

fn part2(input: []const u8, max_coord: isize, allocator: Allocator) !isize {
    var network = try SensorNetwork.init(input, allocator);
    defer network.deinit();

    // Given that there is exactly one coordinate that we cannot detect, we
    // know it must be one tile outside the range of one of our sensors. So
    // instead of checking the entire coordinate range, we can check around
    // the edge of the sensor's range instead.
    for (network.sensors.items) |sensor| {
        var i: isize = 0;
        while (i <= sensor.range) : (i += 1) {
            const y1 = sensor.y - i;
            const y2 = sensor.y + i;
            const x1 = sensor.x - (sensor.range - i) - 1;
            const x2 = sensor.x + (sensor.range - i) + 1;
            if (x1 < 0 or y1 < 0) continue;
            if (x2 > max_coord or y2 > max_coord) continue;
            if (!network.canDetect(x1, y1)) return x1 * 4_000_000 + y1;
            if (!network.canDetect(x1, y2)) return x1 * 4_000_000 + y2;
            if (!network.canDetect(x2, y1)) return x2 * 4_000_000 + y1;
            if (!network.canDetect(x2, y2)) return x2 * 4_000_000 + y2;
        }
    }

    unreachable;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, 10, testing.allocator), 26);
    try testing.expectEqual(part2(example, 20, testing.allocator), 56_000_011);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input, 2_000_000, testing.allocator), 5_166_077);
    try testing.expectEqual(part2(input, 4_000_000, testing.allocator), 13_071_206_703_981);
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    print("Part 1: {}\n", .{try part1(input, 2_000_000, gpa.allocator())});
    print("Part 2: {}\n", .{try part2(input, 4_000_000, gpa.allocator())});
}
