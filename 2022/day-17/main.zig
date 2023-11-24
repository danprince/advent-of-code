const std = @import("std");
const testing = std.testing;
const print = std.debug.print;

const cols = 7;
const rows = 4000;
const Block = std.bit_set.IntegerBitSet(16);
const Grid = std.bit_set.ArrayBitSet(usize, rows * cols);
const Point = @Vector(2, isize);
const down = Point{ 0, -1 };
const left = Point{ -1, 0 };
const right = Point{ 1, 0 };

fn blockFromStr(str: []const u8) Block {
    var block = Block.initEmpty();
    var index: usize = 0;
    for (str) |c| {
        if (c == '\n') continue;
        if (c == '#') block.set(index);
        index += 1;
    }
    return block;
}

const block_1 = blockFromStr(
    \\####
    \\....
    \\....
    \\....
);

const block_2 = blockFromStr(
    \\.#..
    \\###.
    \\.#..
    \\....
);

const block_3 = blockFromStr(
    \\###.
    \\..#.
    \\..#.
    \\....
);

const block_4 = blockFromStr(
    \\#...
    \\#...
    \\#...
    \\#...
);

const block_5 = blockFromStr(
    \\##..
    \\##..
    \\....
    \\....
);

fn getInBlock(block: *const Block, x: u4, y: u4) bool {
    return block.isSet(x + y * 4);
}

fn getInGrid(grid: *Grid, x: isize, y: isize) bool {
    if (x < 0 or y < 0 or x >= cols or y >= rows) return true;
    return grid.isSet(@intCast(usize, x) + @intCast(usize, y) * cols);
}

fn setInGrid(grid: *Grid, x: isize, y: isize) void {
    grid.set(@intCast(usize, x) + @intCast(usize, y) * cols);
}

test "getInBlock" {
    try testing.expect(getInBlock(&block_4, 0, 0));
    try testing.expect(getInBlock(&block_4, 0, 1));
    try testing.expect(getInBlock(&block_4, 0, 2));
    try testing.expect(getInBlock(&block_4, 0, 3));
}

fn canPlaceInGrid(grid: *Grid, block: *Block, pos: Point) bool {
    var x: u4 = 0;
    while (x < 4) : (x += 1) {
        var y: u4 = 0;
        while (y < 4) : (y += 1) {
            if (getInBlock(block, x, y) and getInGrid(grid, pos[0] + x, pos[1] + y)) {
                return false;
            }
        }
    }
    return true;
}

fn placeInGrid(grid: *Grid, block: *Block, pos: Point) isize {
    var max_y: isize = 0;
    var x: u4 = 0;
    while (x < 4) : (x += 1) {
        var y: u4 = 0;
        while (y < 4) : (y += 1) {
            if (getInBlock(block, x, y)) {
                const tile_x = pos[0] + x;
                const tile_y = pos[1] + y;
                setInGrid(grid, tile_x, tile_y);
                if (tile_y > max_y) max_y = tile_y;
            }
        }
    }
    return max_y;
}

fn rowToInt(grid: *Grid, y: isize) usize {
    var result: usize = 0;
    var j: isize = 0;
    while (j < 3) : (j += 1) {
        var x: isize = 0;
        while (x < cols) : (x += 1) {
            if (getInGrid(grid, x, y - j)) result |= 1;
            result <<= 1;
        }
    }
    return result;
}

fn debug(grid: *Grid) void {
    const debug_height = 20;

    var y: isize = 0;
    while (y < debug_height) : (y += 1) {
        var x: isize = 0;
        print("|", .{});
        while (x < cols) : (x += 1) {
            const c: u8 = if (getInGrid(grid, x, debug_height - y - 1)) '#' else '.';
            print("{c}", .{c});
        }
        print("|\n", .{});
    }

    print("+-------+\n\n\n", .{});
}

fn part1(input: []const u8) !isize {
    const jets = std.mem.trim(u8, input, "\n");
    const blocks = [_]Block{ block_1, block_2, block_3, block_4, block_5 };

    var grid = Grid.initEmpty();
    var steps: usize = 0;
    var drops: usize = 0;
    var height: isize = 0;

    while (drops < 2022) : (drops += 1) {
        var block = blocks[@mod(drops, blocks.len)];
        var pos = Point{ 2, height + 3 };

        while (true) {
            const jet = jets[@mod(steps, jets.len)];
            const move = if (jet == '<') left else right;
            const old_pos = pos;
            steps += 1;

            if (canPlaceInGrid(&grid, &block, pos + move)) pos += move;
            if (canPlaceInGrid(&grid, &block, pos + down)) pos += down;
            if (pos[1] < old_pos[1]) continue;

            const top = placeInGrid(&grid, &block, pos) + 1;
            if (top > height) height = top;
            break;
        }
    }

    return height;
}

const CycleDetectionState = struct {
    top_row: usize,
    jet_index: usize,
    block_index: usize,
};

const CycleState = struct {
    dropped_blocks: usize,
    current_height: isize,
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

fn part2(input: []const u8) !isize {
    const allocator = gpa.allocator();
    const jets = std.mem.trim(u8, input, "\n");
    const blocks = [_]Block{ block_1, block_2, block_3, block_4, block_5 };

    var states = std.AutoHashMap(CycleDetectionState, CycleState).init(allocator);
    var grid = Grid.initEmpty();
    var steps: usize = 0;
    var drops: usize = 0;
    var height: isize = 0;
    var extra_height: isize = 0;

    while (drops < 1_000_000_000_000) : (drops += 1) {
        const block_index = @mod(drops, blocks.len);
        const jet_index = @mod(steps, jets.len);
        const top_row = rowToInt(&grid, height - 1);

        var block = blocks[block_index];
        var pos = Point{ 2, height + 3 };

        while (true) {
            const jet = jets[jet_index];
            const move = if (jet == '<') left else right;
            const old_pos = pos;

            const state = CycleDetectionState{
                .block_index = block_index,
                .jet_index = jet_index,
                .top_row = top_row,
            };
            if (states.get(state)) |prev_state| {
                const cycle_length = drops - prev_state.dropped_blocks;
                if (cycle_length > 0) {
                    print("found a cycle after {} drops\n", .{drops});
                    const height_change = height - prev_state.current_height;
                    const further_drops_required = 1_000_000_000_000 - drops;
                    const further_cycles_required = further_drops_required / cycle_length;
                    extra_height = @intCast(isize, further_cycles_required) * height_change;
                    drops += further_cycles_required * cycle_length;
                    print("state={} cycle length={} height change={}\n", .{ state, cycle_length, height_change });
                    print("simulate {} more cycles ({} more drops)\n", .{ further_cycles_required, further_drops_required });
                    print("now done {} drops\n", .{drops});
                    print("at {} height\n", .{height + extra_height});
                }
            } else {
                try states.put(state, .{
                    .dropped_blocks = drops,
                    .current_height = height,
                });
            }

            steps += 1;

            if (canPlaceInGrid(&grid, &block, pos + move)) pos += move;
            if (canPlaceInGrid(&grid, &block, pos + down)) pos += down;
            if (pos[1] < old_pos[1]) continue;

            const top = placeInGrid(&grid, &block, pos) + 1;
            if (top > height) height = top;
            break;
        }
    }

    return height + extra_height;
}

pub fn main() void {
    const example = @embedFile("example.txt");
    _ = try part2(example);
}

test "examples" {
    const example = @embedFile("example.txt");
    try testing.expectEqual(part1(example), 3068);
    try testing.expectEqual(part2(example), 1514285714288);
}

test "inputs" {
    const input = @embedFile("input.txt");
    try testing.expectEqual(part1(input), 3130);
    //try testing.expectEqual(part2(input), 0);
}
