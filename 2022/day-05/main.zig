const std = @import("std");
const testing = std.testing;
const print = std.debug.print;
const eql = std.mem.eql;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Command = struct {
    move: usize,
    from: usize,
    to: usize,

    fn parse(str: []const u8) !Command {
        var parts = std.mem.tokenize(u8, str, " ");
        _ = parts.next(); // Discard "move"
        const move = try std.fmt.parseInt(usize, parts.next().?, 10);
        _ = parts.next(); // Discard "from"
        const from = try std.fmt.parseInt(usize, parts.next().?, 10);
        _ = parts.next(); // Discard "to"
        const to = try std.fmt.parseInt(usize, parts.next().?, 10);
        return .{ .move = move, .from = from, .to = to };
    }
};

const Crane = struct {
    const OperatingMode = enum { single, multiple };

    mode: OperatingMode = .single,
    stacks: std.ArrayList(std.ArrayList(u8)),
    commands: std.ArrayList(Command),

    /// Creates a crane by parsing an initial configuration of stacks and
    /// commands matching the format in the input/example files.
    fn parse(input: []const u8) !Crane {
        var blocks = std.mem.split(u8, input, "\n\n");
        const stacks_input = blocks.next().?;
        const commands_input = blocks.next().?;

        var crane = Crane{
            .stacks = std.ArrayList(std.ArrayList(u8)).init(allocator),
            .commands = std.ArrayList(Command).init(allocator),
        };

        var commands_lines = std.mem.tokenize(u8, commands_input, "\n");

        while (commands_lines.next()) |line| {
            try crane.commands.append(try Command.parse(line));
        }

        // Getting a backwards iterator makes it easier to build the stacks in order
        var stack_lines = std.mem.splitBackwards(u8, stacks_input, "\n");

        {
            var index_string = stack_lines.next().?;
            var i: usize = 1;
            while (i <= index_string.len) : (i += 4) {
                try crane.stacks.append(std.ArrayList(u8).init(allocator));
            }
        }

        while (stack_lines.next()) |line| {
            var i: usize = 1;
            while (i < line.len) : (i += 4) {
                const ch = line[i];
                if (ch != ' ') {
                    var stack = &crane.stacks.items[i / 4];
                    try stack.append(ch);
                }
            }
        }

        return crane;
    }

    /// Execute all of this crane's rearranging commands.
    fn rearrange(self: *Crane) !void {
        for (self.commands.items) |command| {
            var from = &self.stacks.items[command.from - 1];
            var to = &self.stacks.items[command.to - 1];
            var index = from.items.len - command.move;
            if (index <= 0) index = 0;
            var items = from.items[index..];
            from.items.len -= items.len;
            if (self.mode == .single) std.mem.reverse(u8, items);
            try to.appendSlice(items);
        }
    }

    /// Read a string from the characters in the crates on top of each stack.
    fn readTopCrates(self: *const Crane) ![]u8 {
        var result = std.ArrayList(u8).init(allocator);
        // Not doing this creates a memory leak, right? Need to move `result.items`
        // from the heap to the stack before returning it.
        // defer result.deinit();
        for (self.stacks.items) |stack| {
            if (stack.items.len > 0) {
                const ch = stack.items[stack.items.len - 1];
                try result.append(ch);
            }
        }
        return result.items;
    }
};

test "Crane.parse" {
    const t = testing;
    const example = @embedFile("example.txt");
    const crane = try Crane.parse(example);
    const commands = crane.commands.items;
    const stacks = crane.stacks.items;

    try t.expectEqualSlices(Command, commands, &[_]Command{
        .{ .move = 1, .from = 2, .to = 1 },
        .{ .move = 3, .from = 1, .to = 3 },
        .{ .move = 2, .from = 2, .to = 1 },
        .{ .move = 1, .from = 1, .to = 2 },
    });

    try t.expectEqualStrings(stacks[0].items, "ZN");
    try t.expectEqualStrings(stacks[1].items, "MCD");
    try t.expectEqualStrings(stacks[2].items, "P");
}

fn part1(input: []const u8) ![]const u8 {
    var crane = try Crane.parse(input);
    try crane.rearrange();
    return try crane.readTopCrates();
}

fn part2(input: []const u8) ![]const u8 {
    var crane = try Crane.parse(input);
    crane.mode = .multiple;
    try crane.rearrange();
    return try crane.readTopCrates();
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {s}\n", .{try part1(input)});
    print("Part 2: {s}\n", .{try part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqualStrings(try part1(example), "CMZ");
    try testing.expectEqualStrings(try part2(example), "MCD");
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqualStrings(try part1(input), "ZWHVFWQWW");
    try testing.expectEqualStrings(try part2(input), "HZFZCCWWV");
}
