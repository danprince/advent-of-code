using MD5

function part1(input)
    find_hash(input, "00000")
end

function part2(input)
    find_hash(input, "000000")
end

function find_hash(input, prefix)
    i = 0

    while true
        str = string(input, i)
        hash = md5(str) |> bytes2hex

        if startswith(hash, prefix)
            return i
        else
            i += 1
        end
    end
end

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
