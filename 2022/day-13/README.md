# Day 13

Yikes! That was a rough day for Zig. I felt like I ended up fighting the language on lots of levels. The problem was straightforward enough, but it took a lot of coding to get to an answer.

It's possible to safely model the recursive nature of the packet structures using tagged unions in Zig.

```zig
const PacketType = enum { list, number };

const Packet = union(PacketType) {
  list: std.ArrayList(Packet),
  number: u4,
};
```

To clarify, a packet is either a `u4` or a `std.ArrayList(Packet)` and the `PacketType` enum will help us differentiate between them.

So, how do we go from `"[1,1,3,1,1]"` to `Packet`? I went for a stack based parser, because it seemed simpler to implement than a recursive strategy. It scans forwards one character at a time.

- If the current character is `[`
  - Put a new empty list packet onto the stack
- If the current character is `]`
  - Pop the top packet from the stack
  - If the stack is empty
    - Return the popped packet
    - Otherwise, add the popped packet to the list packet at the top of the stack
- If the current character is `0...9`
  - Scan whilst input is numerical
  - Parse the number
  - Add the number to the list packet at the top of the stack

Here's how that looks in Zig.

```zig
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
```

Definitely more complex than all the lucky folks who just parsed their input with `eval` today, but still fairly readable code.

I would have liked to write some tests for `Packet.parse` but `std.testing.expectEqual` didn't want to compare `Packet` instances, so I wrote a debug printer and checked them against the example formats. In another programming language, this might have been have been bypassed by writing a `toString` method then comparing strings instead, but when you're managing memory manually, alloc'ing and realloc'ing space on the heap to encode a recursive data structure, then making it the caller's responsibility to free is just painful.

Speaking of `alloc`, I hit a new problem today. My `Packet.list` type uses a `std.ArrayList` to store nested packets, and `std.ArrayList` stores its data on the heap, meaning it needs an allocator. This in turn means that `Packet` needs an allocator to initialise lists, and it also needs to `deinit` them when the packet itself is deinitialised.

Normally the pattern for this in Zig is to pass an allocator to an `init` method, then the struct holds onto, so you can call `deinit` without having to ensure that you're passing the same allocator to both calls. However, because `Packet` is a union, not a struct it has nowhere to store an allocator. Unions are an "unmanaged" data structure!

Instead of having the caller constantly passing allocators back and forth (this starts getting really messy later) I decided to hoist a single allocator up to the global scope and be done with it.

```zig
const std = @import("std");
const allocator: std.mem.Allocator = std.testing.allocator;
```

I'm using the allocator from the testing module because I've stopped using a `main` function in favour of explicit tests on the puzzle input. However, you could use the builtin module to use a different allocator outside of tests.

```zig
const std = @import("std");
const builtin = @import("builtin");
const allocator: std.mem.Allocator = if (builtin.is_test) std.testing.allocator else std.heap.page_allocator;
```

The last allocation concern here is ensuring that our deallocation is recursive. It's not enough to just call `ArrayList.deinit`, because the allocated memory can contain pointers to other `ArrayList` instances on the heap.

```zig
const Packet = union(PacketType) {
    list: std.ArrayList(Packet),
    number: u4,

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
}
```

Now that we're done parsing and allocating (and deallocating) we can actually start solving the puzzle. I won't bog this down detailing the implementation too much, but this part of the puzzle really made me miss pattern matching. As far as I can tell, `switch` is the only way to differentiate between tagged unions, and even with Zig's switch being an expression, it's still a bit of a mess.

```zig
pub fn isLessThan(left: Packet, right: Packet) bool {
    return switch (left) {
        .number => |left_number| switch (right) {
            .number => |right_number| {
            .list => |right_list| {},
        },
        .list => |left_list| switch (right) {
            .number => |right_number| {}
            .list => |right_list| {},
        },
    };
}
```

This time I was able to write some tests. I also ended up writing a test helper function for the first time to keep the allocation/deallocation pattern sane.

```zig
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
```

Part 2 wasn't a huge additional stretch. We'd already done the tricky work of implementing a less than comparator and most languages with generic data structures (this includes Zig) will allow you to plug in a custom comparator to sorting functions.

Zig doesn't have lambdas though, which means the comparator needs to be extracted out into a separate function.

```zig
fn compareLessThan(_: void, lhs: Packet, rhs: Packet) bool {
    return lhs.isLessThan(rhs);
}

fn part2(input: []const u8) !usize {
  var packets = std.ArrayList(Packet).init(allocator);
  // ...
  std.sort.sort(Packet, packets.items, {}, compareLessThan);
}
```

So close to an answer, and yet my naive attempt to use `std.mem.indexOfScalar` to find the indices of both divider packets was thwarted by the fact that tagged unions can't be compared with `==` (maybe they can if both of their values can?).

Back to coding to implement `Packet.eql` then, another recursive function, which helped me get the correct answer.

<details>
  <summary>
  After finishing today in Zig, I quickly re-solved the problem in JavaScript as a comparison for how much simpler the solution could be in a more appropriate language.
  </summary>

```js
import fs from "fs";
import assert from "assert";

function parse(input) {
  return input.split("\n").filter(line => line).map(eval);
}

function isLessThan(left, right) {
  if (typeof left === "number" && typeof right === "number") {
    return left < right;
  } else if (typeof left === "number") {
    return isLessThan([left], right);
  } else if (typeof right === "number") {
    return isLessThan(left, [right]);
  } else {
    for (let i = 0; i < left.length; i++) {
      if (i >= right.length) return false;
      if (isLessThan(left[i], right[i])) return true;
      if (isLessThan(right[i], left[i])) return false;
    }
    return left.length < right.length;
  }
}

function part1(input) {
  let packets = parse(input);
  let correct = 0;
  for (let index = 1; packets.length; index += 1) {
    if (isLessThan(packets.shift(), packets.shift())) {
      correct += index;
    }
  }
  return correct;
}

function part2(input) {
  let packets = parse(input);
  let div1 = [[2]];
  let div2 = [[6]];
  packets.push(div1, div2);
  packets.sort((left, right) => isLessThan(left, right) ? -1 : 1);
  return (packets.indexOf(div1) + 1) * (packets.indexOf(div2) + 1);
}

assert(isLessThan([1,1,3,1,1], [1,1,5,1,1]));
assert(isLessThan([[1],[2,3,4]], [[1],4]));
assert(!isLessThan([9], [[8,7,6]]));
assert(isLessThan([[4,4],4,4], [[4,4],4,4,4]));
assert(!isLessThan([7,7,7,7], [7,7,7]));
assert(isLessThan([], [3]));
assert(!isLessThan([[[]]], [[]]));
assert(!isLessThan([1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]));

let example = fs.readFileSync("./day-13/example.txt", "utf8").trim();
let input = fs.readFileSync("./day-13/input.txt", "utf8").trim();
assert.equal(part1(example), 13);
assert.equal(part2(example), 140);
assert.equal(part1(input), 6395);
assert.equal(part2(input), 24921);
```

</details>

Definitely an enjoyable day, but hoping the rest of the puzzles go with the grain a little bit more!
