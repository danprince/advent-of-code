using Test

function part1(input)
    grid = split(input, "\n")
    count_trees(grid, [3, 1])
end

function part2(input)
    grid = split(input, "\n")
    moves = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
    prod([count_trees(grid, move) for move in moves])
end

function count_trees(grid, move)
    x = 1
    dx, dy = move
    rows = length(grid)
    cols = length(grid[1])

    count(1+dy:dy:rows) do y
        x = (x + dx - 1) % cols + 1
        grid[y][x] == '#'
    end
end

example = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"

@test part1(example) == 7
@test part2(example) == 336

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
