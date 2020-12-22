const Deck = Vector{Int}

function combat(p1::Deck, p2::Deck)
    while !isempty(p1) && !isempty(p2)
        c1 = popfirst!(p1)
        c2 = popfirst!(p2)

        if c1 > c2
            push!(p1, c1, c2)
        else
            push!(p2, c2, c1)
        end
    end

    isempty(p1) ? p2 : p1
end

function recursive_combat(p1::Deck, p2::Deck)
    seen = Set{Tuple{Deck, Deck}}()

    while !isempty(p1) && !isempty(p2)
        key = copy.((p1, p2))

        if key in seen
            return p1
        else
            push!(seen, key)
        end

        c1 = popfirst!(p1)
        c2 = popfirst!(p2)

        winner = if length(p1) >= c1 && length(p2) >= c2
            r1 = p1[1:c1]
            r2 = p2[1:c2]
            recursive_combat(r1, r2) === r1 ? p1 : p2
        else
            c1 > c2 ? p1 : p2
        end

        if winner === p1
            push!(p1, c1, c2)
        else
            push!(p2, c2, c1)
        end
    end

    return isempty(p1) ? p2 : p1
end

function parse_input(input)
    parts = split(input |> strip, "\n\n")

    p1 = begin
        lines = split(parts[1], '\n')
        parse.(Int, lines[2:end])
    end

    p2 = begin
        lines = split(parts[2], '\n')
        parse.(Int, lines[2:end])
    end

    p1, p2
end

function part1(input)
    p1, p2 = parse_input(input)
    winner = combat(p1, p2)
    sum(prod, winner |> reverse |> enumerate)
end

function part2(input)
    p1, p2 = parse_input(input)
    winner = recursive_combat(p1, p2)
    sum(prod, winner |> reverse |> enumerate)
end

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
