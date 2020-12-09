using Test

function part1(data; preamble=25)
    for i in preamble+1:length(data)
        x = data[i]
        found = false

        for a in i-preamble:i, b in i-preamble:i
            if a == b
                continue
            elseif data[i] == data[a] + data[b]
                found = true
                break
            end
        end

        if found == false
            return data[i]
        end
    end
end

function part2(data; target)
    left, right = 1, 1
    total = data[1]

    while left <= length(data)
        if total < target
            right += 1
            total += data[right]

        elseif total > target
            total -= data[left]
            left += 1

        elseif left != right
            window = @view data[left:right]
            low, high = extrema(window)
            return low + high
        end
    end
end

example = [35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576]

@test part1(example, preamble=5) == 127
@test part2(example, target=127) == 62

open("input.txt") do file
    input = read(file, String)
    data = parse.(Int, split(input))
    p1 = part1(data)
    p2 = part2(data; target=p1)
    println("Part 1: $p1")
    println("Part 2: $p2")
end
