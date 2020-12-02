using Test

function part1(input)
    passwords = parse_input(input)

    count(passwords) do password
        instances = count(char -> char == password.char, password.value)
        instances >= password.min && instances <= password.max
    end
end

function part2(input)
    passwords = parse_input(input)

    count(passwords) do password
        min = password.value[password.min] == password.char
        max = password.value[password.max] == password.char
        min != max
    end
end

function parse_input(input)
    lines = split(input, "\n")

    map(lines) do line
        m = match(r"(\d+)-(\d+) ([a-z]): (.*)", line)
        minstr, maxstr, char, value = m.captures
        min = parse(Int64, minstr)
        max = parse(Int64, maxstr)
        (min = min, max = max, char = char[1], value = value)
    end
end

example = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

@test part1(example) == 2
@test part2(example) == 1

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
