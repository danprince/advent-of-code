const std = @import("std");
const testing = std.testing;
const mem = std.mem;

pub const digits = "0123456789";
pub const lowercase_alphabet = "abcdefghijklmnopqrstuvwxyz";
pub const uppercase_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

const LexError = error{CouldNotParse};

pub const Lexer = struct {
    input: []const u8,
    start_pos: usize = 0,
    end_pos: usize = 0,

    fn reset(self: *Lexer) void {
        self.end_pos = self.start_pos;
    }

    fn scan(self: *Lexer, set: []const u8) []const u8 {
        while (!self.done() and containsByte(set, self.input[self.end_pos])) : (self.end_pos += 1) {}
        return self.input[self.start_pos..self.end_pos];
    }

    fn containsByte(str: []const u8, byte: u8) bool {
        for (str) |ch| if (ch == byte) return true;
        return false;
    }

    fn take(self: *Lexer) []const u8 {
        const str = self.input[self.start_pos..self.end_pos];
        self.start_pos = self.end_pos;
        return str;
    }

    pub fn literal(self: *Lexer, str: []const u8) !void {
        if (!mem.eql(u8, str, self.input[self.start_pos .. self.start_pos + str.len])) {
            return LexError.CouldNotParse;
        } else {
            self.start_pos += str.len;
            self.end_pos = self.start_pos;
        }
    }

    pub fn done(self: *Lexer) bool {
        return self.end_pos >= self.input.len;
    }

    pub fn all(self: *Lexer, set: []const u8) []const u8 {
        _ = self.scan(set);
        return self.take();
    }

    pub fn any(self: *Lexer, set: []const u8) !u8 {
        if (containsByte(set, self.input[self.end_pos])) {
            const ch = self.input[self.end_pos];
            self.end_pos += 1;
            self.start_pos = self.end_pos;
            return ch;
        } else {
            return LexError.CouldNotParse;
        }
    }

    pub fn int(self: *Lexer, comptime T: type, radix: u8) !T {
        const str = self.scan("-0123456789ABCDEF");
        if (std.fmt.parseInt(T, str, radix)) |value| {
            _ = self.take();
            return value;
        } else |_| {
            self.reset();
            return LexError.CouldNotParse;
        }
    }
};

pub fn init(input: []const u8) Lexer {
    return .{ .input = input };
}

test "lex literal" {
    var l = init("hello world");
    try l.literal("hello");
    try l.literal(" ");
    try l.literal("world");
}

test "lex literal mismatch" {
    var l = init("hello world");
    try testing.expectEqual(l.literal("goodbye"), LexError.CouldNotParse);
}

test "lex all" {
    var l = init("1234 end");
    try testing.expectEqualStrings(l.all("0123456"), "1234");
    try testing.expectEqualStrings(l.all(&std.ascii.whitespace), " ");
    try l.literal("end");
}

test "lex all empty" {
    var l = init("end");
    try testing.expectEqualStrings(l.all("0123456"), "");
    try l.literal("end");
}

test "lex all to end of input" {
    var l = init("555");
    try testing.expectEqualStrings(l.all("5"), "555");
}

test "lex any" {
    var l = init("1 end");
    try testing.expectEqual(l.any("1234"), '1');
    try l.literal(" end");
}

test "lex any" {
    var l = init("5 end");
    try testing.expectEqual(l.any("1234"), LexError.CouldNotParse);
}

test "lex int decimal" {
    var l = init("1234 end");
    try testing.expectEqual(l.int(usize, 10), 1234);
    try l.literal(" end");
}

test "lex signed int" {
    var l = init("-1234 end");
    try testing.expectEqual(l.int(isize, 10), -1234);
    try l.literal(" end");
}

test "lex hex int" {
    var l = init("1234 end");
    try testing.expectEqual(l.int(usize, 16), 0x1234);
    try l.literal(" end");
}

test "lex invalid int" {
    var l = init("start end");
    try testing.expectEqual(l.int(usize, 16), LexError.CouldNotParse);
    try l.literal("start end");
}
