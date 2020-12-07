using Test

function part1(input)
    strings = split(input)

    count(strings) do str
        all([
            match(r"[aeiou].*[aeiou].*[aeiou]", str) != nothing,
            match(r"(.)\1", str) != nothing,
            match(r"(ab|cd|pq|xy)", str) == nothing,
        ])
    end
end

function part2(input)
    strings = split(input)

    count(strings) do str
        all([
            match(r"(..).*\1", str) != nothing,
            match(r"(.).\1", str) != nothing,
        ])
    end
end

@test part1("ugknbfddgicrmopn") == 1
@test part1("aaa") == 1
@test part1("jchzalrnumimnmhp") == 0
@test part1("haegwjzuvuyypxyu") == 0
@test part1("dvszwmarrgswjxmb") == 0

@test part2("aaa") == 0
@test part2("qjhvhtzxzqqjkmpb") == 1
@test part2("xxyxx") == 1
@test part2("uurcxstgmygtbstg") == 0
@test part2("ieodomkazucvgmuy") == 0
@test part2("zczc") == 1

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
