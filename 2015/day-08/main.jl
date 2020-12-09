using Test

function part1(input)
    strings = split(input)

    sum(strings) do str
        actual = Meta.parse(str) |> eval
        length(str) - length(actual)
    end
end

function part2(input)
    strings = split(input)

    sum(strings) do str
        escaped = escape_string(str)
        length(escaped) + 2 - length(str)
    end
end

open("example.txt") do file
    example = read(file, String) |> strip
    @test part1(example) == 12
    @test part2(example) == 19
end

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
