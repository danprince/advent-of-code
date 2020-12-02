using Test

function part1(input)
    nums = parse_input(input)

    for (x, y) in Iterators.product(nums, nums)
        if x + y == 2020
            return x * y
        end
    end
end

function part2(input)
    nums = parse_input(input)

    for (x, y, z) in Iterators.product(nums, nums, nums)
        if x + y + z == 2020
            return x * y * z
        end
    end
end

function parse_input(input)
    lines = split(input, "\n")
    map(line -> parse(Int64, line), lines)
end

example = "1721
979
366
299
675
1456"

@test part1(example) == 514579
@test part2(example) == 241861950

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
