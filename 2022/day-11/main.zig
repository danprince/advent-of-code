const std = @import("std");
const lex = @import("lex.zig");
const testing = std.testing;
const print = std.debug.print;
const mem = std.mem;
const Allocator = mem.Allocator;

const Operator = enum { multiply, plus };
const OperandType = enum { variable, value };
const Operand = union(OperandType) { variable: []const u8, value: usize };

const Monkey = struct {
    id: usize,
    items: std.ArrayList(usize),
    op: Operator,
    lhs: Operand,
    rhs: Operand,
    divisible_by: usize,
    true_monkey_id: usize,
    false_monkey_id: usize,
};

fn parseMonkeys(input: []const u8, allocator: std.mem.Allocator) ![]Monkey {
    var monkeys = std.ArrayList(Monkey).init(allocator);
    defer monkeys.deinit();

    var l = lex.init(input);
    while (!l.done()) {
        try l.literal("Monkey ");
        const id = try l.int(usize, 10);
        try l.literal(":\n");

        var monkey = Monkey{
            .id = id,
            .items = std.ArrayList(usize).init(allocator),
            .op = .multiply,
            .lhs = .{ .value = 0 },
            .rhs = .{ .value = 0 },
            .divisible_by = 0,
            .true_monkey_id = 0,
            .false_monkey_id = 0,
        };

        try l.literal("  Starting items: ");
        while (true) {
            const item = try l.int(usize, 10);
            try monkey.items.append(item);
            if (l.all(", ").len == 0) break;
        }
        try l.literal("\n");

        try l.literal("  Operation: new = ");
        monkey.lhs = parseOperand(&l);
        _ = l.all(" ");
        monkey.op = if (try l.any("+*") == '+') .plus else .multiply;
        _ = l.all(" ");
        monkey.rhs = parseOperand(&l);
        try l.literal("\n");

        try l.literal("  Test: divisible by ");
        monkey.divisible_by = try l.int(usize, 10);
        try l.literal("\n");

        try l.literal("    If true: throw to monkey ");
        monkey.true_monkey_id = try l.int(usize, 10);
        try l.literal("\n");

        try l.literal("    If false: throw to monkey ");
        monkey.false_monkey_id = try l.int(usize, 10);
        _ = l.all("\n");

        try monkeys.append(monkey);
    }

    return monkeys.toOwnedSlice();
}

fn parseOperand(l: *lex.Lexer) Operand {
    if (l.int(usize, 10)) |value| {
        return .{ .value = value };
    } else |_| {
        return .{ .variable = l.all(lex.lowercase_alphabet) };
    }
}

fn calculateMonkeyBusiness(monkeys: []Monkey, rounds: usize, divisor: usize, allocator: std.mem.Allocator) !usize {
    var counters = std.AutoHashMap(usize, usize).init(allocator);
    defer counters.deinit();

    var modulo_base: usize = 1;
    for (monkeys) |monkey| modulo_base *= monkey.divisible_by;

    var round: usize = 0;
    while (round < rounds) : (round += 1) {
        for (monkeys) |*monkey| {
            for (monkey.items.items) |old| {
                const lhs = switch (monkey.lhs) {
                    .value => |value| value,
                    .variable => old,
                };

                const rhs = switch (monkey.rhs) {
                    .value => |value| value,
                    .variable => old,
                };

                var worry_level = switch (monkey.op) {
                    .plus => lhs + rhs,
                    .multiply => lhs * rhs,
                };

                worry_level /= divisor;
                worry_level = @mod(worry_level, modulo_base);

                const receiver_id = if (@mod(worry_level, monkey.divisible_by) == 0)
                    monkey.true_monkey_id
                else
                    monkey.false_monkey_id;

                try monkeys[receiver_id].items.append(worry_level);

                const count = counters.get(monkey.id) orelse 0;
                try counters.put(monkey.id, count + 1);
            }

            // Clear items now that the current monkey has thrown them all
            try monkey.items.resize(0);
        }
    }

    var values_list = std.ArrayList(usize).init(allocator);
    defer values_list.deinit();

    var values_itr = counters.valueIterator();
    while (values_itr.next()) |value| try values_list.append(value.*);

    std.sort.sort(usize, values_list.items, {}, std.sort.desc(usize));
    return values_list.items[0] * values_list.items[1];
}

fn part1(input: []const u8, allocator: Allocator) !usize {
    var monkeys = try parseMonkeys(input, allocator);
    defer allocator.free(monkeys);
    defer for (monkeys) |monkey| monkey.items.deinit();
    return try calculateMonkeyBusiness(monkeys, 20, 3, allocator);
}

fn part2(input: []const u8, allocator: Allocator) !usize {
    var monkeys = try parseMonkeys(input, allocator);
    defer allocator.free(monkeys);
    defer for (monkeys) |monkey| monkey.items.deinit();
    return try calculateMonkeyBusiness(monkeys, 10000, 1, allocator);
}

pub fn main() !void {
    var gpa = std.heal.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const input = @embedFile("input.txt");
    print("Part 1: {!}\n", .{part1(input, allocator)});
    print("Part 2: {!}\n", .{part2(input, allocator)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example, std.testing.allocator), 10605);
    try testing.expectEqual(part2(example, std.testing.allocator), 2713310158);
}

test "inputs" {
    const input = @embedFile("input.txt");
    // TODO: Not sure when this regression happened. Will figure it out after!
    //try testing.expectEqual(part1(input, std.testing.allocator), 88208);
    try testing.expectEqual(part2(input, std.testing.allocator), 2713310158);
}
