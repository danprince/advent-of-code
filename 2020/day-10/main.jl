using Test

can_connect(a, b) = b >= a - 3

function part1(adapters)
    ones = 0
    threes = 1

    sort!(adapters)

    for i in 1:length(adapters)
        volts = adapters[i] - get(adapters, i-1, 0)

        if volts == 1
            ones += 1
        elseif volts == 3
            threes += 1
        end
    end

    ones * threes
end

function part2(adapters)
    sort!(adapters)

    charger_outlet = 0
    device_adapter = adapters[end] + 3
    push!(adapters, device_adapter)
    pushfirst!(adapters, charger_outlet)

    n = length(adapters)
    arrangements = zeros(Int, n)
    arrangements[1] = 1

    for i in 1:n
        for j = i-1:-1:1
            if can_connect(adapters[i], adapters[j])
                arrangements[i] += arrangements[j]
            else
                break
            end
        end
    end

    arrangements[n]
end

example = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
example2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

@test part1(example) == 35
@test part1(example2) == 220
@test part2(example) == 8
@test part2(example2) == 19208

open("input.txt") do file
    input = read(file, String) |> strip
    lines = split(input)
    adapters = parse.(Int, lines)
    println("Part 1: $(part1(adapters))")
    println("Part 2: $(part2(adapters))")
end
