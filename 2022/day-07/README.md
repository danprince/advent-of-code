# Day 7
That was fun! Felt like a definite difficulty speedbump compared to yesterday's puzzle though.

This is a puzzle where I saw two obvious pathways for approach. The first would be to parse the file system into a formal tree, then use recursion to calculate the size of each dir. The second was to attempt to calculate all of the sizes during the parsing traversal, which is what I ended up going for. I had a hunch that it might bite me in part 2 (which it did).

For the first part of the puzzle, I kept a running total of the sizes of all directories that I had seen that were within the allowed size, but the second part of the puzzle needed the root dir size, which meant having to add a second traversal, once the root dir size was known.

If I'm being honest, writing the parsing code in Zig is always a bit of a pain. In other languages a puzzle like this would be parsed trivially with 2 or 3 capture group regexes, but every morning I sit down with `std.mem` and feel a little bit sad. I originally parsed today's input with two levels of `std.mem.tokenize` (one for lines, one for spaces) but it created so much code that I ended up rewriting it in a flatter style, once I realised I didn't actually need to parse the names of the files/dirs.

It's still a little unclear to me as to how to structure deallocation code sometimes. Let's look at a simplified version of `parse` as an example.

```zig
fn parse(allocator: std.mem.Allocator) ![]usize {
  var sizes = std.ArrayList(usize).init(allocator);
  defer sizes.deinit();

  try sizes.append(10);
  try sizes.append(20);
  try sizes.append(30);

  return sizes.items;
}
```

This function returns a slice of the values, which creates a segmentation fault because that memory is freed as part when the `defer sizes.deinit()` statement runs. This makes sense, because the returned slice needs to live beyond the end of the execution of this function.

That would mean we want the caller to be responsible for freeing this memory instead, but in this case the caller only has access to the `items` slice. They can't free any additional memory that the internal data structure might have allocated.

One way to approach this would be to return the array list, instead of the slice, but leaking implementation details like this gets really messy, really fast.

I'm not sure whether this is idiomatic Zig, but I ended up allocating additional memory for the contents of the array list, copying it over, then safely deallocating the array list.

```zig
fn parse(allocator: std.mem.Allocator) ![]usize {
  var sizes = std.ArrayList(usize).init(allocator);
  defer sizes.deinit();

  try sizes.append(10);
  try sizes.append(20);
  try sizes.append(30);

  var size_items = allocator.allocate(usize, sizes.items.len);
  std.mem.copy(usize, size_items, sizes.items);
  return size_items;
}
```

Responsibility for freeing `size_items` transfers to the caller (answering a question I had on day 5) and this pattern becomes easier to follow when you're passing allocators explicitly.
