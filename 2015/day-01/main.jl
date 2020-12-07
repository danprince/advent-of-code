using Test

const dir = Dict('('=>+1, ')'=>-1)

function part1(input)
    sum(input) do c
        dir[c]
    end
end

function part2(input)
    floor = 0

    findfirst(input) do c
        floor += dir[c]
        floor == -1
    end
end

@test part1("(())") == 0
@test part1("(((") == 3
@test part1("())") == -1

@test part2(")") == 1
@test part2("()())") == 5

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
