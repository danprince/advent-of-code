# Using cube coordinates to model hexagonal grid.
# See: https://www.redblobgames.com/grids/hexagons/#coordinates-cube

const Coord = Tuple{Int, Int, Int}
const BLACK = true
const WHITE = false

neighbours((x, y, z)::Coord) = (
    (x + 1, y - 1, z + 0),
    (x + 0, y - 1, z + 1),
    (x + 1, y + 0, z - 1),
    (x - 1, y + 1, z + 0),
    (x - 1, y + 0, z + 1),
    (x + 0, y + 1, z - 1),
)

function part1(input)
    lines = split(input)
    tiles = Dict{Coord, Bool}()

    for line in lines
        x, y, z = 0, 0, 0

        for m in eachmatch(r"(e|se|sw|w|nw|ne)", line)
            step = m[1]

            if step == "e"
                x += 1; y -= 1
            elseif step == "se"
                y -= 1; z += 1
            elseif step == "ne"
                x += 1; z -= 1
            elseif step == "w"
                x -= 1; y += 1
            elseif step == "sw"
                x -= 1; z += 1
            elseif step == "nw"
                y += 1; z -= 1
            else
                throw("invalid step $step")
            end
        end

        tiles[x, y, z] = !get(tiles, (x, y, z), false)
    end

    tiles |> values |> count, tiles
end

function part2(state)
    next_state = Dict{Coord, Bool}()

    for day in 1:100
        # Make sure neighbours of any flipped tiles are considered as
        # part of this state.
        for (tile, color) in state
            if color == BLACK
                for neighbour in neighbours(tile)
                    get!(state, neighbour, false)
                end
            end
        end

        for (tile, color) in state
            score = 0

            for neighbour in neighbours(tile)
                if get(state, neighbour, false)
                    score += 1
                end
            end

            next_state[tile] = begin
                if color == BLACK && score == 0 || score > 2
                    WHITE
                elseif color == WHITE && score == 2
                    BLACK
                else
                    get(state, tile, false)
                end
            end
        end

        state, next_state = next_state, state
    end

    state |> values |> sum
end

open("input.txt") do file
    input = read(file, String) |> strip
    p1, state = part1(input)
    p2 = part2(state)
    println("Part 1: $(p1)")
    println("Part 2: $(p2)")
end
