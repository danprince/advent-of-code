const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const allocator: Allocator = std.testing.allocator;

const PacketType = enum { list, number };

const Packet = union(PacketType) {
    list: std.ArrayList(Packet),
    number: u4,

    /// Initialize an empty list packet. The caller owns the returned packet.
    pub fn initEmptyList() !Packet {
        return .{ .list = std.ArrayList(Packet).init(allocator) };
    }

    /// Initialize a number packet. The caller owns the returned packet.
    pub fn initNumber(number: u4) Packet {
        return .{ .number = number };
    }

    /// Initialise a list packet given a slice of packets. The caller owns the
    /// returned packet.
    pub fn initList(items: []const Packet) Packet {
        var list = initEmptyList() catch unreachable;
        for (items) |packet| list.append(packet) catch unreachable;
        return list;
    }

    /// Deallocate the packet recursively.
    pub fn deinit(self: Packet) void {
        switch (self) {
            .list => |list| {
                for (list.items) |packet| packet.deinit();
                list.deinit();
            },
            else => {},
        }
    }

    /// Append a packet to this packet. Panics if the receiving packet is not a
    /// list type packet.
    fn append(self: *Packet, packet: Packet) !void {
        switch (self.*) {
            .list => |*list| try list.append(packet),
            else => unreachable,
        }
    }

    /// Create the list representation of a packet. If the packet isn't already
    /// a list packet, return a single item list packet containing it.
    pub fn asList(self: Packet) Packet {
        return switch (self) {
            .list => self,
            .number => |number| initList(&[_]Packet{initNumber(number)}),
        };
    }

    /// Check whether `left` is less than `right` according to the puzzle rules.
    pub fn isLessThan(left: Packet, right: Packet) bool {
        return switch (left) {
            .number => |left_number| switch (right) {
                .number => |right_number| {
                    return left_number < right_number;
                },
                .list => {
                    const tmp = left.asList();
                    defer tmp.deinit();
                    return tmp.isLessThan(right);
                },
            },
            .list => |left_list| switch (right) {
                .number => {
                    const tmp = right.asList();
                    defer tmp.deinit();
                    return left.isLessThan(tmp);
                },
                .list => |right_list| {
                    for (left_list.items) |left_packet, index| {
                        const right_is_out = index >= right_list.items.len;
                        if (right_is_out) return false;
                        const right_packet = right_list.items[index];
                        if (left_packet.isLessThan(right_packet)) return true;
                        if (right_packet.isLessThan(left_packet)) return false;
                    }

                    const left_is_out = left_list.items.len < right_list.items.len;
                    return left_is_out;
                },
            },
        };
    }

    /// Check whether two packets are identical.
    pub fn eql(left: Packet, right: Packet) bool {
        return switch (left) {
            .number => |left_num| switch (right) {
                .number => |right_num| left_num == right_num,
                .list => false,
            },
            .list => |left_list| switch (right) {
                .number => false,
                .list => |right_list| {
                    if (left_list.items.len != right_list.items.len) return false;

                    for (left_list.items) |left_child, index| {
                        const right_child = right_list.items[index];
                        const child_eql = left_child.eql(right_child);
                        if (!child_eql) return false;
                    }

                    return true;
                },
            },
        };
    }

    /// Parse a packet from an input string.
    pub fn parse(input: []const u8) !Packet {
        var stack = std.ArrayList(Packet).init(allocator);
        defer stack.deinit();
        var pos: usize = 0;

        while (pos < input.len) : (pos += 1) {
            switch (input[pos]) {
                '[' => {
                    // We've started a new list, add it to the stack.
                    try stack.append(try initEmptyList());
                },
                ']' => {
                    // We've finished the current list, take it off the stack.
                    const packet = stack.pop();

                    // If this was the last packet on the stack, then we're done.
                    if (stack.items.len == 0) return packet;

                    // Otherwise, add this packet to the parent packet.
                    var parent = &stack.items[stack.items.len - 1];
                    try parent.append(packet);
                },
                '0'...'9' => {
                    // Scan forwards until we reach the end of the number
                    var end = pos + 1;
                    while (input[end] >= '0' and input[end] <= '9') : (end += 1) {}

                    // Then attempt to parse it as an integer.
                    const number = try std.fmt.parseInt(u4, input[pos..end], 10);

                    // And add it to the parent packet.
                    var parent = &stack.items[stack.items.len - 1];
                    try parent.append(initNumber(number));
                },
                else => {},
            }
        }

        // We only arrive here if the input brackets aren't balanced
        unreachable;
    }
};

fn part1(input: []const u8) !usize {
    var lines = mem.tokenize(u8, input, "\n\n");
    var index: usize = 1;
    var correct: usize = 0;

    while (lines.next()) |left_str| {
        const right_str = lines.next().?;

        const left = try Packet.parse(left_str);
        defer left.deinit();

        const right = try Packet.parse(right_str);
        defer right.deinit();

        if (left.isLessThan(right)) correct += index;
        index += 1;
    }

    return correct;
}

fn compareLessThan(_: void, lhs: Packet, rhs: Packet) bool {
    return lhs.isLessThan(rhs);
}

fn part2(input: []const u8) !usize {
    var packets = std.ArrayList(Packet).init(allocator);
    defer packets.deinit();
    defer for (packets.items) |packet| packet.deinit();

    var lines = mem.tokenize(u8, input, "\n\n");
    while (lines.next()) |str| try packets.append(try Packet.parse(str));

    var divider_packet_1 = Packet.initList(&[_]Packet{Packet.initList(&[_]Packet{Packet.initNumber(2)})});
    var divider_packet_2 = Packet.initList(&[_]Packet{Packet.initList(&[_]Packet{Packet.initNumber(6)})});
    try packets.append(divider_packet_1);
    try packets.append(divider_packet_2);

    std.sort.sort(Packet, packets.items, {}, compareLessThan);

    var index_1: usize = 0;
    var index_2: usize = 0;

    for (packets.items) |packet, index| {
        if (packet.eql(divider_packet_1)) index_1 = index + 1;
        if (packet.eql(divider_packet_2)) index_2 = index + 1;
    }

    return index_1 * index_2;
}

fn testLessThan(comptime left_str: []const u8, comptime right_str: []const u8) bool {
    const left = Packet.parse(left_str) catch unreachable;
    defer left.deinit();
    const right = Packet.parse(right_str) catch unreachable;
    defer right.deinit();
    return left.isLessThan(right);
}

test "Packet.isLessThan" {
    try testing.expect(testLessThan("[1,1,3,1,1]", "[1,1,5,1,1]"));
    try testing.expect(testLessThan("[[1],[2,3,4]]", "[[1],4]"));
    try testing.expect(!testLessThan("[9]", "[[8,7,6]]"));
    try testing.expect(testLessThan("[[4,4],4,4]", "[[4,4],4,4,4]"));
    try testing.expect(!testLessThan("[7,7,7,7]", "[7,7,7]"));
    try testing.expect(testLessThan("[]", "[3]"));
    try testing.expect(!testLessThan("[[[]]]", "[[]]"));
    try testing.expect(!testLessThan("[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]"));
}

fn testEql(comptime left_str: []const u8, comptime right_str: []const u8) bool {
    const left = Packet.parse(left_str) catch unreachable;
    defer left.deinit();
    const right = Packet.parse(right_str) catch unreachable;
    defer right.deinit();
    return left.eql(right);
}

test "Packet.eql" {
    try testing.expect(testEql("[1,1,3,1,1]", "[1,1,3,1,1]"));
    try testing.expect(testEql("[[1],[2,3,4]]", "[[1],[2,3,4]]"));
    try testing.expect(testEql("[[2]]", "[[2]]"));
    try testing.expect(!testEql("[[2]]", "[2]"));
    try testing.expect(!testEql("[[2]]", "[[1]]"));
    try testing.expect(!testEql("[[2]]", "[[[2]]]"));
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 13);
    try testing.expectEqual(part2(example), 140);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 6395);
    try testing.expectEqual(part2(input), 24921);
}
