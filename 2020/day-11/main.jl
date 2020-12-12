using Test

const adjacent_steps = [
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1)
]

function parse_state(str)
    lines = split(str)
    cols = length(lines[1])
    rows = length(lines)
    state = fill('.', rows, cols)

    for row in 1:rows, col in 1:cols
        state[row, col] = lines[row][col]
    end

    state
end

function simulate(update, state)
    state = copy(state)
    new_state = copy(state)
    rows, cols = size(state)

    while true
        for row in 1:rows, col in 1:cols
            seat = state[row, col]
            new_state[row, col] = update(state, seat, row, col)
        end

        if state == new_state
            return count(seat -> seat == '#', new_state)
        end

        state, new_state = new_state, state
    end
end

function part1(state)
    simulate(state) do state, seat, row, col
        neighbours = count(adjacent_steps) do step
            pos = (row, col) .+ step
            get(state, pos, 'L') == '#'
        end

        if seat == '#' && neighbours >= 4
            return 'L'
        elseif seat == 'L' && neighbours == 0
            return '#'
        else
            return seat
        end
    end
end

function part2(state)
    simulate(state) do state, seat, row, col
        neighbours = count_visible_neighbours(state, row, col)

        if seat == '#' && neighbours >= 5
            return 'L'
        elseif seat == 'L' && neighbours == 0
            return '#'
        else
            return seat
        end
    end
end

function count_visible_neighbours(state, row, col)
    count(adjacent_steps) do step
        pos = (row, col)

        while true
            pos = pos .+ step
            neighbour = get(state, pos, 'L')

            if neighbour != '.'
                return neighbour == '#'
            end
        end
    end
end

example = parse_state("L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

@test part1(example) == 37
@test part2(example) == 26

open("input.txt") do file
    state = read(file, String) |> parse_state
    println("Part 1: $(part1(state))")
    println("Part 2: $(part2(state))")
end
