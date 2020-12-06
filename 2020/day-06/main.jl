using Test

function part1(input)
    groups = split(input, "\n\n")

    sum(groups) do group
        lines = split(group, "\n")
        reduce(union, lines) |> length
    end
end

function part2(input)
    groups = split(input, "\n\n")

    sum(groups) do group
        lines = split(group, "\n")
        reduce(intersect, lines) |> length
    end
end

example = "abc

a
b
c

ab
ac

a
a
a
a

b"

@test part1(example) == 11
@test part2(example) == 6

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
