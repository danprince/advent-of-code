mutable struct Tile
    id::Int
    image::BitArray{2}
end

const Point = Tuple{Int, Int}
const PartialSolution = Dict{Point, Tile}
const Solution = Array{Tile, 2}

const UP = (+0, -1)
const DOWN = (+0, +1)
const LEFT  = (-1, +0)
const RIGHT = (+1, +0)

const monster = BitArray([
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 1 1;
    0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 0
])

"""
Get the 4 adjacent puzzle positions for a given point.
"""
neighbours(p::Point) = [p .+ UP, p .+ DOWN, p .+ LEFT, p .+ RIGHT]

"""
Flip a 2D array vertically
"""
flipv(a) = reverse(a, dims = 1)

"""
Flip a 2D array horizontally
"""
fliph(a) = reverse(a, dims = 2)

"""
Return all possible rotations for a given array
"""
rotations(a) = [a, rotr90(a), rot180(a), rotl90(a)]

"""
Return all possible orientations (rotations/flips) for a given array
"""
orientations(a) = [rotations(a); rotations(a |> flipv)]

top_side(tile::Tile) = @view tile.image[1, :]
bottom_side(tile::Tile) = @view tile.image[end, :]
left_side(tile::Tile) = @view tile.image[:, 1]
right_side(tile::Tile) = @view tile.image[:, end]

"""
Print a compact view of a 2D BitArray
"""
function debug(array::BitArray{2})
    rows, cols = size(array)

    for row in 1:rows
        for col in 1:cols
            bit = array[row, col]
            print(bit ? '#' : '.')
        end
        print('\n')
    end
end

"""
Check whether a tile can be inserted into a partial solution at
a given position.
"""
function can_insert(puzzle::PartialSolution, tile::Tile, pos::Point)
    above = get(puzzle, pos .+ UP, nothing)
    below = get(puzzle, pos .+ DOWN, nothing)
    left = get(puzzle, pos .+ LEFT, nothing)
    right = get(puzzle, pos .+ RIGHT, nothing)

    (above === nothing || top_side(tile) == bottom_side(above)) &&
        (below === nothing || bottom_side(tile) == top_side(below)) &&
        (left === nothing || left_side(tile) == right_side(left)) &&
        (right === nothing || right_side(tile) == left_side(right))
end

const transforms = [flipv, fliph, rotr90]

"""
Find a solution from a list of tiles.
"""
function solve(tiles::Vector{Tile})::Solution
    queue = copy(tiles)
    puzzle = Dict{Point, Tile}()
    slots = Set{Point}([(0, 0)])

    while !isempty(queue)
        tile = popfirst!(queue)
        solved = false

        for position in slots
            if can_insert(puzzle, tile, position)
                # add this tile to the solution
                puzzle[position] = tile

                # remove this position from the available slots
                delete!(slots, position)

                # add unsolved neighbours to the available slots
                for adjacent_position in neighbours(position)
                    unsolved = !haskey(puzzle, adjacent_position)

                    if unsolved
                        push!(slots, adjacent_position)
                    end
                end

                solved = true
                break
            end
        end

        if !solved
            transform = rand(transforms)
            tile.image = transform(tile.image)
            push!(queue, tile)
        end
    end

    positions = keys(puzzle)
    min_x, min_y = min.(positions...)
    max_x, max_y = max.(positions...)
    rows = max_y - min_y + 1
    cols = max_x - min_x + 1
    array = Array{Any, 2}(nothing, rows, cols)

    for ((x, y), tile) in puzzle
        rx = x - min_x + 1
        ry = y - min_y + 1
        array[rx, ry] = tile
    end

    array
end

"""
Trim the borders and stitch the tiles of a solution together into
a single continuous array.
"""
function stitch(solution::Solution)::BitArray{2}
    grid_size = size(solution)
    tile_size = size(solution[1].image) .- 2
    image_size = grid_size .* tile_size
    grid_height, grid_width = grid_size
    tile_height, tile_width = tile_size

    image = falses(image_size)

    for y in 1:grid_height, x in 1:grid_width
        tile = solution[y, x]

        # convert grid coords into image coords
        y0 = (y - 1) * tile_height + 1
        y1 = y0 + tile_height - 1
        x0 = (x - 1) * tile_width + 1
        x1 = x0 + tile_width - 1

        # TODO: Had to swap coordinates here and no idea why
        # Are the images somehow rotated?
        image[x0:x1, y0:y1] = tile.image[2:end-1, 2:end-1]
    end

    image
end

function parse_input(input::AbstractString)::Vector{Tile}
    rx = r"Tile (\d+):\n([\.#\n]+)\n"m
    matches = eachmatch(rx, input)

    return map(matches) do m
        id = parse(Int, m[1])
        lines = split(m[2])
        rows = length(lines)
        cols = length(lines[1])
        image = falses(rows, cols)

        for row in 1:rows, col in 1:cols
            image[row, col] = lines[row][col] == '#'
        end

        Tile(id, image)
    end
end

function part1(solution::Solution)
    corners = [solution[1, 1], solution[1, end], solution[end, 1], solution[end, end]]
    prod(tile -> tile.id, corners)
end

function part2(solution::Solution)
    image = stitch(solution)
    monster_height, monster_width = size(monster)

    for image in orientations(image)
        image_height, image_width = size(image)

        x0, y0 = 1, 1
        x1 = image_width - monster_width
        y1 = image_height - monster_height
        monsters = 0

        for y in y0:y1, x in x0:x1
            sample = @view image[y:y+monster_height-1, x:x+monster_width-1]

            if monster == sample .& monster
                sample .⊻= monster
                monsters += 1
            end
        end

        if monsters > 0
            roughness = sum(image)
            return roughness
        end
    end
end

open("input.txt") do file
    tiles = read(file, String) |> parse_input
    @time solution = solve(tiles)
    println("Part 1: $(part1(solution))")
    println("Part 2: $(part2(solution))")
end
