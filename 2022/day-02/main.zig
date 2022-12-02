const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

const Sign = enum {
    rock,
    paper,
    scissors,

    fn parse(ch: u8) Sign {
        return switch (ch) {
            'A', 'X' => .rock,
            'B', 'Y' => .paper,
            'C', 'Z' => .scissors,
            else => unreachable,
        };
    }

    fn score(sign: Sign) u32 {
        return switch (sign) {
            .rock => 1,
            .paper => 2,
            .scissors => 3,
        };
    }

    fn wins(sign: Sign) Sign {
        return switch (sign) {
            .rock => .scissors,
            .paper => .rock,
            .scissors => .paper,
        };
    }

    fn loses(sign: Sign) Sign {
        return switch (sign) {
            .rock => .paper,
            .paper => .scissors,
            .scissors => .rock,
        };
    }

    fn compare(a: Sign, b: Sign) Outcome {
      return if (a == b) .draw else if (a.wins() == b) .win else .lose;
    }
};

const Outcome = enum {
    lose,
    win,
    draw,

    fn parse(ch: u8) Outcome {
        return switch (ch) {
            'X' => .lose,
            'Y' => .draw,
            'Z' => .win,
            else => unreachable,
        };
    }

    fn score(outcome: Outcome) u32 {
        return switch (outcome) {
            .lose => 0,
            .draw => 3,
            .win => 6,
        };
    }
};

fn part1(input: []const u8) !u32 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var score: u32 = 0;

    while (lines.next()) |line| {
        const opponent = Sign.parse(line[0]);
        const player = Sign.parse(line[2]);
        const outcome = player.compare(opponent);
        score += player.score() + outcome.score();
    }

    return score;
}

fn part2(input: []const u8) !u32 {
    var lines = std.mem.tokenize(u8, input, "\n");
    var score: u32 = 0;

    while (lines.next()) |line| {
        const opponent = Sign.parse(line[0]);
        const outcome = Outcome.parse(line[2]);
        const player = switch (outcome) {
            .win => opponent.loses(),
            .lose => opponent.wins(),
            .draw => opponent,
        };
        score += player.score() + outcome.score();
    }

    return score;
}

pub fn main() !void {
    const input = @embedFile("input.txt");
    print("Part 1: {d}\n", .{try part1(input)});
    print("Part 2: {d}\n", .{try part2(input)});
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(try part1(example), 15);
    try testing.expectEqual(try part2(example), 12);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(try part1(input), 12772);
    try testing.expectEqual(try part2(input), 11618);
}
