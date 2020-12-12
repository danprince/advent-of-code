using Test

const directions = ('N', 'E', 'S', 'W')

function rotate(direction::Char, degrees::Int)
    steps = degrees / 90
    index = findfirst(d -> d == direction, directions)
    index = mod1(index + steps, 4)
    directions[index]
end

function rotate(point::Tuple{Int, Int}, degrees::Int)
    x, y = point
    s = sind(degrees)
    c = cosd(degrees)
    rx = Int(x * c - y * s)
    ry = Int(x * s + y * c)
    (rx, ry)
end

function part1(input)
    x, y = 0, 0
    heading = 'E'

    for line in split(input)
        action = line[1]
        steps = parse(Int, line[2:end])

        if action == 'F'
            action = heading
        end

        if action == 'N' y -= steps
        elseif action == 'S' y += steps
        elseif action == 'E' x += steps
        elseif action == 'W' x -= steps
        elseif action == 'L' heading = rotate(heading, -steps)
        elseif action == 'R' heading = rotate(heading, steps)
        else error("Invalid instruction", line) end
    end

    abs(x) + abs(y)
end

function part2(input)
    ferry = (0, 0)
    waypoint = (10, -1)

    for line in split(input)
        action = line[1]
        steps = parse(Int, line[2:end])

        if action == 'F'
            ferry = ferry .+ waypoint .* steps
            continue
        end

        waypoint = begin
            if action == 'N' waypoint .- (0, steps)
            elseif action == 'S' waypoint .+ (0, steps)
            elseif action == 'E' waypoint .+ (steps, 0)
            elseif action == 'W' waypoint .- (steps, 0)
            elseif action == 'L' rotate(waypoint, -steps)
            elseif action == 'R' rotate(waypoint, steps)
            else error("Invalid instruction", line) end
        end
    end

    x, y = ferry
    abs(x) + abs(y)
end

example = "F10
N3
F7
R90
F11"

@test part1(example) == 25
@test part2(example) == 286

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
