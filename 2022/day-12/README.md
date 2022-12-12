# Day 12

Everyone's Advent of Code bingo card should include at least one pathfinding day, so this puzzle was no real surprise. The ["Introduction to A*" article](https://www.redblobgames.com/pathfinding/a-star/introduction.html) on [redblobgames.com](https://www.redblobgames.com/) is a fantastic resource for pathfinding (amongst many other algorithms) and is still my goto resource, despite having implemented multiple versions of BFS, Dijkstra's Algorithm, and A* plenty of times before.

A* would have been the fastest algorithm for part one, but I implemented Dijkstra's algorithm, mostly because I know it better and it usually involves less code. My initial solution just a `std.ArrayList` as a queue, but it turned out to be very easy to swap this for a `std.PriorityQueue` once I had the correct answer. Way faster!

For these kinds of grid based problems, I tend to find that I'm leaning towards using `isize` as much as possible, to avoid having to constantly guard against numbers underflowing (for when checking neighbours at the boundaries) or using the saturating arithmetic operators (and having to deal with edge cases when distinct calcuations produce identical values).

[Dijkstra maps](http://www.roguebasin.com/index.php/The_Incredible_Power_of_Dijkstra_Maps) are commonplace in traditional roguelike development, and the second part of this puzzle maps almost directly to a classic pathfinding optimisation scenario.

Instead of pathfinding from each monster to the player with a heuristic based algorithm, pathfind from the player, but use Dijkstra's algorithm (which finds the shortest path from the start to _every_ accessible location) then have the monsters walk "downhill" on that map.

If you squint right, `a` tiles are monsters, and the `E` tile is the player.

I've been writing a lot of Zig outside of Advent of Code over the last week and I'm definitely starting to feel productive. Patterns are gradually starting to click and I'm getting a better handle on patterns for error handling and allocations. The standard library is an excellent codebase, and has become my primary reference, instead of the docs, both for patterns and for information. I can definitely see myself continuing to use the language beyond Advent of Code.
