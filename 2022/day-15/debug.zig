const std = @import("std");

pub const DebugGrid = struct {
    const Self = @This();
    const Point = struct { x: isize, y: isize };

    x1: isize = 0,
    y1: isize = 0,
    x2: isize = 0,
    y2: isize = 0,
    chars: std.AutoHashMap(Point, u8),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .chars = std.AutoHashMap(Point, u8).init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.chars.deinit();
    }

    pub fn put(self: *Self, x: isize, y: isize, ch: u8) void {
        if (x < self.x1) self.x1 = x;
        if (y < self.y1) self.y1 = y;
        if (x > self.x2) self.x2 = x;
        if (y > self.y2) self.y2 = y;
        self.chars.put(.{ .x = x, .y = y }, ch) catch unreachable;
    }

    pub fn print(self: *Self) void {
        var y = self.y1;
        while (y <= self.y2) : (y += 1) {
            var x = self.x1;
            while (x <= self.x2) : (x += 1) {
                std.debug.print("{c}", .{self.chars.get(.{ .x = x, .y = y }) orelse ' '});
            }
            std.debug.print("\n", .{});
        }
    }
};
