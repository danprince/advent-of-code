using Test

function part1(input)
    lines = split(input)

    sum(lines) do line
        l, w, h = parse.(Int, split(line, "x"))
        sides = (l * w, w * h, h * l)
        sum(2 .* sides) + minimum(sides)
    end
end

function part2(input)
    lines = split(input)

    sum(lines) do line
        l, w, h = parse.(Int, split(line, "x"))
        a, b = sort([l, w, h])
        2a + 2b + l * w * h
    end
end

@test part1("2x3x4") == 58
@test part1("1x1x10") == 43
@test part2("2x3x4") == 34
@test part2("1x1x10") == 14

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
