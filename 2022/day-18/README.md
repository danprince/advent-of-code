# Day 18

Finally back to a day which didn't make my head hurt. The first part was deceptively straightforward and resulted in what I thought was some pretty elegant Zig code.

```zig
const Point = @Vector(3, isize);

const von_neumann_neighbours = [_]Point{
    .{ -1, 0, 0 },
    .{ 1, 0, 0 },
    .{ 0, -1, 0 },
    .{ 0, 1, 0 },
    .{ 0, 0, -1 },
    .{ 0, 0, 1 },
};

fn part1(input: []const u8, allocator: Allocator) !usize {
    var points = std.AutoHashMap(Point, void).init(allocator);
    defer points.deinit();

    var coords = mem.tokenize(u8, input, "\n,");
    while (coords.peek()) |_| {
        var x = try std.fmt.parseInt(isize, coords.next().?, 10);
        var y = try std.fmt.parseInt(isize, coords.next().?, 10);
        var z = try std.fmt.parseInt(isize, coords.next().?, 10);
        try points.put(.{ x, y, z }, {});
    }

    var area: usize = 0;
    var cubes = points.keyIterator();
    while (cubes.next()) |pos| {
        for (von_neumann_neighbours) |step| {
            if (!points.contains(pos.* + step)) {
                area += 1;
            }
        }
    }
    return area;
}
```

Part two wasn't much more complicated, but I ended up spending quite a while working through very literal edge cases. I used a floodfill algorithm from 0,0,0 (in relative coordinate space) to identify the external cubes, then used that same set to discard surface area measurements for internal spaces.

However, I neglected to check whether I was considering "out of bounds" spaces when calculating the area, which meant I ended up counting a bunch of spaces outside the cubic bounds of the sphere as "internal" because they didn't appear in my external points set.

This has been a year of off-by-one errors for me. Here's the fix:

```zig
bounds_min -= Point{ 1, 1, 1 };
bounds_max += Point{ 1, 1, 1 };
```

I actually found that the significant challenge today was the 3D nature of the problem. It's harder to think about higher dimensions, they have more edge cases, and most of all, it's much harder to debug in a terminal to check why something is wrong. I ended up having to print out the contents of my sphere in slices to realise what was going wrong at the boundaries.
