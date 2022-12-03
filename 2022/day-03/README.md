# Day 3

Enjoying Zig! Managed another day without reaching for the allocator. Initially I solved part 1 with a brute force loop, then revisited the approach during part 2 and swapped the implementation for bitsets.

Today was the first day I managed to cause a runtime crash in Zig, and first that happened by using `std.StaticBitSet('z')` and trying to store a `'z'` (should have been `'z' + 1`). And the second came when using the `.?` operator to unwrap `null` values from groups of rucksacks in part 2, without using any kind of check to see whether the iterator was done. Thankfully, both times, the crash messages were very clear and it didn't take long to fix the problems.

Things haven't been quite as good in the editor today. I have been using the `HEAD` version of Zig and the `zls-vscode` crashed so many times today that it reached the _not even going to try and restart_ state twice. It seems to try to download the latest version of ZLS every time it starts, which usually ends up resulting in an error about a version mismatch with my Zig version. The advice in [this article](https://zig.news/jarredsumner/setting-up-visual-studio-code-for-writing-zig-kcj) suggests building ZLS yourself, which seems sane, except when I try to build it, I get hit with errors. More version mismatches?

```zig
/opt/homebrew/Cellar/zig/HEAD-71038c4/lib/zig/std/build.zig:540:29: error: expected type '?[]const []const u8', found 'error{OutOfMemory}![][]const u8'
            .enum_options = enum_options,
                            ^~~~~~~~~~~~
referenced by:
    build: /Users/dan/conf/zls/build.zig:27:10
    runBuild: /opt/homebrew/Cellar/zig/HEAD-71038c4/lib/zig/build_runner.zig:231:32
    remaining reference traces hidden; use '-freference-trace' to see all reference traces

/opt/homebrew/Cellar/zig/HEAD-71038c4/lib/zig/std/build.zig:540:29: error: expected type '?[]const []const u8', found 'error{OutOfMemory}![][]const u8'
            .enum_options = enum_options,
                            ^~~~~~~~~~~~
```

Definitely need to sort that out, because the Zig docs aren't particularly great, and reading the source of the standard library is often the best way to get solid answers. I rely pretty heavily on being able to jump-to-definition to figure out how to use features like `StaticBitSet`, where the [docs](https://ziglang.org/documentation/master/std/#root;StaticBitSet) read:

```
Parameters
size: usize,
```

That's it. Zig still seems young enough that Googling often doesn't turn up any useful results (Stack Overflow threads, blog posts about features, Reddit discussions). However, the standard library is very readable, and that's plenty for now.

Still super happy with the inline tests (and also amazingly useful as examples when reading the standard library). I wish more programming languages would do this!
