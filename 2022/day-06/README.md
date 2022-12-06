# Day 6
Not a huge amount to say about Zig today. I'm not the biggest fan of programming with labels, but in languages without functional iteration methods, it often feels cleaner to use one, than to introduce book-keeping variables.

Today's solution felt surprisingly close to how you might go about solving this problem in C. I was tempted to reach for `std.bit_sets` again, but ended up deciding to use arrays instead.

I would prefer it if you could initialize variables inside Zig's while loop, as the pattern of having those live in the enclosing scope feels a bit wonky to me, and gives programmers the opportunity to introduce intermediate variables between the initializer and the loop body, or even to move the loop body around without also moving the initializer. Basically, I would prefer it if Zig had traditional for loops. I prefer Go's version of this, where `while` doesn't exist, but `for` can take a condition (e.g. `for true {`).

Something I'm noticing about writing Zig is that there are certain pathways that confuse the language server. For example, if I use `std.StaticBitSet` then I never get completions or errors for variables of that type in my editor. Here's the source from the standard library:

```zig
pub fn StaticBitSet(comptime size: usize) type {
    if (size <= @bitSizeOf(usize)) {
        return IntegerBitSet(size);
    } else {
        return ArrayBitSet(usize, size);
    }
}
```

My guess is that there's a problem with inferring a union like `IntegerBitSet | ArrayBitSet` across branches like that (even if the condition is comptime known). Even if `StaticBitSet` returned a [union type](https://ziglang.org/documentation/master/#Tagged-union) I think you might constantly have to `switch` before usage. In other languages without union types, the same problem might be solved with a `BitSet` interface that both structs implement, but Zig's version of interfaces seems to be "make sure they match". Hopefully that's a direction Zig will go in future, because it puts me off using certain parts of the standard library at the moment.
