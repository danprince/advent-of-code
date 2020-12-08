using Test

look_and_say(str::AbstractString) = collect(str) |> look_and_say |> join

function look_and_say(chars::Vector{Char})
    result::Vector{Char} = []
    run = 0
    prev = nothing

    for i = 1:length(chars)
        curr = chars[i]

        if prev != curr && run > 0
            push!(result, Char(run + 48), prev)
            run = 0
        end

        prev = curr
        run += 1
    end

    if run > 0
        push!(result, Char(run + 48), prev)
    end

    result
end

function part1(input)
    chars = collect(input)

    for i = 1:40
       chars = look_and_say(chars)
    end

    join(chars) |> length
end

function part2(input)
    chars = collect(input)

    for i = 1:50
       chars = look_and_say(chars)
    end

    join(chars) |> length
end

@test look_and_say("1") == "11"
@test look_and_say("11") == "21"
@test look_and_say("21") == "1211"
@test look_and_say("1211") == "111221"
@test look_and_say("111221") == "312211"

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
