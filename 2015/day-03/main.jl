using Test

const directions = Dict(
    '<'=>(-1, 0),
    '>'=>(1, 0),
    '^'=>(0, -1),
    'v'=>(0, 1),
)

function part1(input)
    santa = (0, 0)
    houses = Set([santa])

    for c in input
        santa = santa .+ directions[c]
        push!(houses, santa)
    end

    length(houses)
end

function part2(input)
    santa = (0, 0)
    robosanta = (0, 0)
    houses = Set([santa, robosanta])

    for (i, c) in enumerate(input)
        if isodd(i)
            santa = santa .+ directions[c]
            push!(houses, santa)
        else
            robosanta = robosanta .+ directions[c]
            push!(houses, robosanta)
        end
    end

    length(houses)
end

@test part1(">") == 2
@test part1("^>v<") == 4
@test part1("^v^v^v^v^v") == 2

@test part2("^v") == 3
@test part2("^>v<") == 3
@test part2("^v^v^v^v^v") == 11

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
