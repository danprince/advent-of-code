using Test

function is_readable(pass::Vector{Char})
    for c in pass
        if c == 'i' || c == 'o' || c == 'l'
            return false
        end
    end

    return true
end

function has_pairs(pass::Vector{Char})
    prev_pair = nothing

    for i in 2:length(pass)
        a, b = pass[i], pass[i-1]

        if a == b
            if prev_pair != nothing && a != prev_pair
                return true
            else
                prev_pair = a
            end
        end
    end

    return false
end

function has_straight(pass::Vector{Char})
    for i in 3:length(pass)
        if pass[i-2] + 1 == pass[i-1] == pass[i] - 1
            return true
        end
    end

    return false
end

function is_valid(s)
    is_readable(s) && has_pairs(s) && has_straight(s)
end

function next_pass!(pass::Vector{Char})
    for i in length(pass):-1:1
        if pass[i] < 'z'
            pass[i] += 1
            break
        else
            pass[i] = 'a'
        end
    end

    pass
end

function find_next_valid(input)
    pass = collect(input)

    while true
        next_pass!(pass)

        if is_valid(pass)
            return join(pass)
        end
    end
end

@test has_straight("hijklmmn" |> collect) == true
@test has_straight("abbceffg" |> collect) == false

@test is_readable("abbceffg" |> collect) == true
@test is_readable("hijklmmn" |> collect) == false

@test has_pairs("abbceffg" |> collect) == true
@test has_pairs("abbcefgh" |> collect) == false

@test next_pass!("abc" |> collect) == collect("abd")
@test next_pass!("abz" |> collect) == collect("aca")

open("input.txt") do file
    input = read(file, String) |> strip
    p1 = find_next_valid(input)
    p2 = find_next_valid(p1)
    println("Part 1: $(p1)")
    println("Part 2: $(p2)")
end
