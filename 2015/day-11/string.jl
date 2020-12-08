using Test

# Solving with strings and regex ended up being pretty slow and
# memory heavy because of the per-iteration string allocations.
#
# See main.jl for a faster version that uses a mutable
# Vector{Char} instead.

has_straight(str) = any(3:length(str)) do i
    str[i - 2] + 1 == str[i - 1] && str[i - 1] + 1 == str[i]
end

is_readable(str) = match(r"[iol]", str) == nothing

has_pairs(str) = begin
    pairs = map(m -> m[1], eachmatch(r"(.)\1", str))
    unique(pairs) |> length >= 2
end

is_valid(str) = is_readable(str) && has_pairs(str) && has_straight(str)

function next_password(str)
    chars = collect(str)

    for i in length(chars):-1:1
        if chars[i] < 'z'
            chars[i] += 1
            break
        else
            chars[i] = 'a'
        end
    end

    join(chars)
end

function find_next_valid(password)
    while true
        password = next_password(password)

        if is_valid(password)
            return password
        end
    end
end

@test has_straight("hijklmmn") == true
@test has_straight("abbceffg") == false

@test is_readable("abbceffg") == true
@test is_readable("hijklmmn") == false

@test has_pairs("abbceffg") == true

@test next_password("abc") == "abd"
@test next_password("abz") == "aca"

open("input.txt") do file
    input = read(file, String) |> strip
    @time p1 = find_next_valid(input)
    @time p2 = find_next_valid(p1)
    println("Part 1: $(p1)")
    println("Part 2: $(p2)")
end
