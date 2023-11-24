const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;
const Coordinates = std.TailQueue(isize);

fn debug(coords: Coordinates) void {
    var curr = coords.first;
    while (curr) |node| {
        print("{d}", .{node.data});
        curr = node.next;
        if (curr != null) print(", ", .{});
    }
    print("\n", .{});
}

fn findNextN(coords: Coordinates, node: *Coordinates.Node, n: isize) *Coordinates.Node {
    var curr: *Coordinates.Node = node;
    var steps = n;
    while (steps != 0) {
        if (steps > 0) curr = curr.next orelse coords.first orelse unreachable;
        if (steps < 0) curr = curr.prev orelse coords.last orelse unreachable;
        steps -= std.math.sign(n);
    }
    return if (n < 0) curr.prev orelse coords.last orelse unreachable else curr;
}

fn part1(input: []const u8, child_allocator: Allocator) !isize {
    var arena = std.heap.ArenaAllocator.init(child_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();
    var coords = Coordinates{};
    var nodes = std.AutoArrayHashMap(usize, *Coordinates.Node).init(allocator);
    var index: usize = 0;

    var lines = mem.tokenize(u8, input, "\n");
    while (lines.next()) |line| : (index += 1) {
        var node = try allocator.create(Coordinates.Node);
        node.data = try std.fmt.parseInt(isize, line, 10);
        try nodes.put(index, node);
        coords.append(node);
    }

    print("nodes {}", .{nodes.count()});

    //print("initial arrangement\n", .{});
    //debug(coords);
    //print("\n", .{});

    for (nodes.values()) |node| {
        const sibling = findNextN(coords, node, node.data);
        if (sibling == node) continue;
        coords.remove(node);
        coords.insertAfter(sibling, node);
        //print("{} moves between x and y\n", .{node.data});
        //debug(coords);
        //print("\n", .{});
    }

    //debug(coords);

    var index_a = @mod(1000, nodes.count());
    var index_b = @mod(2000, nodes.count());
    var index_c = @mod(3000, nodes.count());

    var i: usize = 0;
    var sum: isize = 0;
    var curr = coords.first;
    while (i < nodes.count()) : (i += 1) {
        if (i == index_a or i == index_b or i == index_c) {
            sum += curr.?.data;
        }
    }

    return sum;
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    _ = allocator;
    _ = input;
    return 0;
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, testing.allocator), 3);
    try testing.expectEqual(part2(example, testing.allocator), 0);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input, testing.allocator), 0);
    //try testing.expectEqual(part2(input, testing.allocator), 0);
}
