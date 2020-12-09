using Test
using Combinatorics

mutable struct Family
    people::Vector
    relationships::Dict
end

function Family(input::AbstractString)
    rx = r"(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+)."
    people = Set()
    relationships = Dict()

    for m in eachmatch(rx, input)
        from, type, units, to = m.captures
        happiness = parse(Int, units)
        happiness = type == "gain" ? happiness : -happiness
        relationships[from => to] = happiness
        push!(people, from, to)
    end

    Family(people |> collect, relationships)
end

function part1(family)
    n = length(family.people)

    maximum(family.people |> permutations) do seats
        sum(1:n) do i
            person = seats[i]
            left = i > 1 ? seats[i - 1] : seats[n]
            right = i < n ? seats[i + 1] : seats[1]
            family.relationships[person => left] +
                family.relationships[person => right]
        end
    end
end

# TODO: The smarter solution here would be to find the unhappiest pair
# from part1 and seat yourself between them.
function part2(family)
    you = "You"

    for person in family.people
        family.relationships[you => person] = 0
        family.relationships[person => you] = 0
    end

    push!(family.people, you)
    part1(family)
end

example = Family("Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.")

@test part1(example) == 330

open("input.txt") do file
    family = read(file, String) |> Family
    println("Part 1: $(part1(family))")
    println("Part 2: $(part2(family))")
end
