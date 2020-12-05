using Test

part1 = maximum

function part2(ids)
    set = Set(ids)

    for i in 0:length(ids)
        if !(i in set) && (i + 1 in set) && (i - 1 in set)
            return i
        end
    end
end

function bsp(str, one)
    reduce(str, init=0) do x, c
        x << 1 | (c == one)
    end
end

function parse_seat(str)
    row = bsp(str[1:7], 'B')
    col = bsp(str[8:10], 'R')
    row * 8 + col
end

@test parse_seat("FBFBBFFRLR") == 357
@test parse_seat("BFFFBBFRRR") == 567
@test parse_seat("FFFBBBFRRR") == 119
@test parse_seat("BBFFBBFRLL") == 820

open("input.txt") do file
    input = read(file, String)
    ids = parse_seat.(split(input))
    println("Part 1: $(part1(ids))")
    println("Part 2: $(part2(ids))")
end
