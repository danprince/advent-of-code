const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

fn range(comptime n: usize) [n]void {
    return [_]void{{}} ** n;
}

const inf = std.math.maxInt(usize);

const Node = struct {
    name: []const u8,
    flow_rate: usize,
    tunnels: [][]const u8,
    allocator: Allocator,

    pub fn deinit(self: *Node) void {
        self.allocator.free(self.tunnels);
    }
};

const Edge = struct {
    from: []const u8,
    to: []const u8,
};

const Network = struct {};

fn parse(input: []const u8, allocator: Allocator) !Network {
    var flow_rates = std.ArrayList([]const u8).init(allocator);
    var node_index = std.StringHashMap(usize).init(allocator);
    var edge_list = std.ArrayList(Edge).init(allocator);

    var lines = mem.tokenize(u8, input, "\n");
    while (lines.next()) |line| {
        var parts = mem.tokenize(u8, line, " =;,");
        for (range(1)) |_| _ = parts.next();
        const from = parts.next().?;
        for (range(3)) |_| _ = parts.next();
        const flow_rate = try std.fmt.parseInt(usize, parts.next().?, 10);
        for (range(4)) |_| _ = parts.next();
        const index = flow_rates.items.len;
        try flow_rates.append(flow_rate);
        try node_index.put(from, index);
        while (parts.next()) |to| try edge_list.put(.{ .from = from, .to = to });
    }

    // Create the adjacency matrix
    const size = flow_rates.items.len;
    var dist = allocator.alloc([]usize, size);

    {
        var i: usize = 0;
        while (i < size) : (i += 1) {
            dist[i] = allocator.alloc(usize, size);
            var j: usize = 0;
            while (j < size) : (j += 1) {
                dist[i][j] = inf;
            }
        }
    }

    // Set the initial weights
    for (edge_list.items) |edge| {
        const i = node_index.get(edge.from).?;
        const j = node_index.get(edge.to).?;
        dist[i][j] = 1;
    }

    // Floyd-Warshall
    {
        var k: usize = 0;
        while (k < size) : (k += 1) {
            var i: usize = 0;
            while (i < size) : (i += 1) {
                var j: usize = 0;
                while (j < size) : (j += 1) {
                    if (dist[i][j] > dist[i][k] + dist[k][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }
    }

    {
        var k: usize = 0;
        while (k < size) : (k += 1) {
            var i: usize = 0;
            while (i < size) : (i += 1) {
                var j: usize = 0;
                var a = node_index.get()
                var
                _ = j;
            }
        }
    }

    return .{};
}

fn part1(input: []const u8, allocator: Allocator) !usize {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    _ = try parse(input, arena.allocator());

    return 0;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    _ = allocator;
    _ = input;
    return 0;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, testing.allocator), 0);
    try testing.expectEqual(part2(example, testing.allocator), 0);
}

test "inputs" {
    //const input = @embedFile("input.txt");
    //try testing.expectEqual(part1(input, testing.allocator), 0);
    //try testing.expectEqual(part2(input, testing.allocator), 0);
}
