using Test
using JSON

function part1(input)
    matches = eachmatch(r"-?\d+", input)

    if isempty(matches)
        0
    else
        sum(matches) do m
            parse(Int, m.match)
        end
    end
end

function sum_in_json(v)
    if isa(v, Dict)
        vals = values(v)
        "red" in vals ? 0 : sum(sum_in_json, v)
    elseif isa(v, Vector)
        sum(sum_in_json, v)
    elseif isa(v, Number)
        v
    else
        0
    end
end

function part2(input)
    json = JSON.parse(input)
    sum_in_json(json)
end

@test part1("""[1,2,3]""") == 6
@test part1("""{"a":2,"b":4}""") == 6
@test part1("""[[[3]]]""") == 3
@test part1("""{"a":{"b":4},"c":-1}""") == 3
@test part1("""{"a":[-1,1]}""") == 0
@test part1("""[-1,{"a":1}]""") == 0
@test part1("""[]""") == 0
@test part1("""{}""") == 0

@test part2("""[1,2,3]""") == 6
@test part2("""[1,{"c":"red","b":2},3]""") == 4
@test part2("""{"d":"red","e":[1,2,3,4],"f":5}""") == 0
@test part2("""[1,"red",5]""") == 6

open("input.json") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
