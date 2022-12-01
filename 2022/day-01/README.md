# Day 1
Bit of a slow start for a day one puzzle as I got comfortable with Zig's toolchain and idioms.

I love that more languages are adopting patterns for inline testing, which is a huge improvement over separating unit tests out into separate files, especially for challenges like Advent of Code where you're always working from a reference input.

Compile time programming is always fun, and Zig's `comptime` semantics seem more interesting than most languages. Nice to be able to pass types around as first class citizens at compile time, and cool to build generics that way.

Also really like the errors-as-values model, with `try`, `catch`, and `if` having error handling behaviours built in. Can imagine it's easy to just get into the habit of propagating all errors upwards by using `try`.

The editor support isn't quite what I had hoped, seems like the compiler still knows quite a lot more than the language server exposes. Settings `zig.buildOnSave` helps there though.

The puzzle was simple enough, I suspect to use `std.mem.split` a lot, but I'm already slightly nervous about doing an entire month of Advent of Code without Regex.

For part 1, I just used the iterator you get back from `std.mem.split`. I found it interesting that iterable doesn't seem to be a trait/interface/protocol, just a struct with a `next` method. Curious to see whether it's even possible to write a function that takes an arbitrary iterator. I _love_ languages like Zig that allow me to jump to definitions in the standard library to read the implementation.

Initially, I approached part 2 with an `ArrayList`, where I stored all of the summed values, then converted it to a slice, passed the slice to `std.sort.sort`, and summed the first 3. Creating an `ArrayList` requires choosing an allocator and I felt pretty confident that this task could be done without touching the heap. My final solution ended up just using an array (fixed size) to store the top 3 values instead. That ends up needing an ad-hoc implementation of an insertion sort, which feels a bit messy. I briefly experimented with using a 4-element array and `std.sort.insertionSort` which works fine as an alternative.
