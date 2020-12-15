const adjacent = [(i, j) for i in -1:1, j in -1:1 if !(i == j == 0)]

function parse_state(input)
    lines = split(input)
    rows = length(lines)
    cols = length(lines[1])
    state = zeros(Bool, rows, cols)

    for row in 1:rows
        for col in 1:cols
            char = lines[row][col]
            state[row, col] = char == '#' ? true : false
        end
    end

    state
end

function simulate(state)
    rows, cols = size(state)
    next_state = copy(state)

    for row in 1:rows, col in 1:cols
        light = state[row, col]

        neighbours = count(adjacent) do (i, j)
            get(state, (row + i, col + j), false)
        end

        next_state[row, col] = begin
            if light
                2 <= neighbours <= 3
            else
                neighbours == 3
            end
        end
    end

    next_state
end

function part1(state)
    state = copy(state)

    for i in 1:100
        state = simulate(state)
    end

    sum(state)
end

function part2(state)
    rows, cols = size(state)

    state = copy(state)
    state[1, 1] = state[rows, 1] = state[1, cols] = state[rows, cols] = 1

    for i in 1:100
        state = simulate(state)
        state[1, 1] = state[rows, 1] = state[1, cols] = state[rows, cols] = 1
    end

    sum(state)
end

open("input.txt") do file
    input = read(file, String)
    state = parse_state(input)
    println("Part 1: $(part1(state))")
    println("Part 2: $(part2(state))")
end
