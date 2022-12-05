# Day 5
The actual puzzle here was incredibly straightforward, but there was a lot of setup around the edges that meant it took a while to get a solution.

The input parsing was more complex than previous days, but still not too tricky with Zig's `std.mem` functions. When you're dealing with quantities that are not known at compile time, it can be verbose to set this stuff up.

If I had more time today, I'd be interested to play with an allocator free version that takes advantage of the fact that the crane input is a comptime known value to initialize a crane with a fixed number of stacks, of fixed sizes, based on the inputs. I think you'd end up having to roll your own stack implementation though.

This is the first day that I ended up feeling like it was appropriate to reach for the heap, with all the `ArrayList` instances that I used to solve the problem. I found myself wondering again about error handling. Some of the functions I wrote (such as `Crane.readTopCrates`) need to allocate memory behind the scenes, even though it's not obvious why. It's not always clear _why_ a given function is returning an error union. Today I decided to just propagate those errors all the way up to the top, but I suspect in future, I might more inclined to just `catch unreachable` at the callsites that first create potential error values instead.

There's also an interesting question about where to put your allocators. Today I opted for a global allocator, but also means that I can't make use of `std.testing.allocator` in tests. The alternative would involve constructing every `Crane` instance with an allocator then passing allocators around in `main` and each of the tests.

The final place I ended up confused was in `readTopCrates`. This function creates a string from the characters on top of each stack of crates, which involves creating a string at runtime.

```zig
/// Read a string from the characters in the crates on top of each stack.
fn readTopCrates(self: *const Crane) ![]u8 {
    var result = std.ArrayList(u8).init(allocator);
    defer result.deinit();
    for (self.stacks.items) |stack| {
        if (stack.items.len > 0) {
            const ch = stack.items[stack.items.len - 1];
            try result.append(ch);
        }
    }
    return result.items;
}
```

The `defer result.deinit()` call creates a bug here, which results in corrupted memory in the slice that is returned. I understand why this happens (`result` is declared on the heap and `result.items` is a slice, which is backed by a pointer into the heap allocated memory), however, I'm not quite sure how to fix it. I want to copy the underlying memory onto the stack then free the allocated space on the heap. Should try to find some other functions that build strings dynamically and see what they do.
