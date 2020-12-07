using Test

function part1(instructions)
    lights = zeros(Bool, 1000, 1000)

    for (cmd, xs, ys) in instructions
        if cmd == "turn on"
            lights[xs, ys] .= true
        elseif cmd == "turn off"
            lights[xs, ys] .= false
        elseif cmd == "toggle"
            lights[xs, ys] = .!lights[xs, ys]
        end
    end

    sum(lights)
end

function part2(instructions)
    lights = zeros(UInt8, 1000, 1000)

    for (cmd, xs, ys) in instructions
        if cmd == "turn on"
            lights[xs, ys] .+= 1
        elseif cmd == "toggle"
            lights[xs, ys] .+= 2
        elseif cmd == "turn off"
            lights[xs, ys] = map(lights[xs, ys]) do brightness
                brightness > 0 ? brightness - 1 : 0
            end
        end
    end

    sum(lights)
end

function parse_input(input)
    matches = eachmatch(r"(.*) (\d+),(\d+) through (\d+),(\d+)", input)

    map(matches) do m
        cmd, coords = m.captures[1], m.captures[2:end]
        x0, y0, x1, y1 = parse.(Int, coords) .+ 1
        (cmd, x0:x1, y0:y1)
    end
end

open("input.txt") do file
    instructions = read(file, String) |> parse_input
    println("Part 1: $(part1(instructions))")
    println("Part 2: $(part2(instructions))")
end
