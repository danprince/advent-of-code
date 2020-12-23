include("CircularLinkedLists.jl")

using .CircularLinkedLists

function play_crab_cups!(numbers; moves)
    high = maximum(numbers)
    curr = CircularLinkedList{Int}(numbers)
    cups_by_value = Dict(cup.value => cup for cup in curr)

    for move = 1:moves
        a = delete!(curr.next)
        b = delete!(curr.next)
        c = delete!(curr.next)

        dest = curr.value - 1

        while true
            if dest == a.value || dest == b.value || dest == c.value
                dest -= 1
            elseif dest <= 0
                dest = high
            else
                break
            end
        end

        dest_cup = cups_by_value[dest]
        insert!(dest_cup, a)
        insert!(a, b)
        insert!(b, c)

        curr = curr.next
    end

    cups_by_value[1] |> collect
end

function part1(input)
    cups = parse.(Int, input |> collect)
    cups = play_crab_cups!(cups, moves = 10)
    join(cups[2:end])
end

function part2(input)
    cups = parse.(Int, input |> collect)
    high = maximum(cups) + one(Int)
    cups = vcat(cups, high:Int(1_000_000))
    cups = play_crab_cups!(cups, moves = 10_000_000)
    cups[2] * cups[3]
end

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
