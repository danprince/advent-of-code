 const aunt_sue = Dict(
    "children" => 3,
    "cats" => 7,
    "samoyeds" => 2,
    "pomeranians" => 3,
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => 5,
    "trees" => 3,
    "cars" => 2,
    "perfumes" => 1,
)

function parse_input(input)
    aunts = Dict()

    for m in eachmatch(r"Sue (\d+): (.*)", input)
        id = m[1]
        aunts[id] = Dict()

        for m in eachmatch(r"([a-z]+): (\d+)", m[2])
            key = m[1]
            val = parse(Int, m[2])
            aunts[id][key] = val
        end
    end

    aunts
end

function part1(aunts)
    for (id, aunt) in aunts
        if all(aunt) do (key, val)
            aunt[key] == aunt_sue[key]
        end
            return id
        end
    end
end

function part2(aunts)
    for (id, aunt) in aunts
        if all(aunt) do (key, val)
            if key == "cats" || key == "trees"
                aunt[key] > aunt_sue[key]
            elseif key == "pomerians" || key == "goldfish"
                aunt[key] < aunt_sue[key]
            else
                aunt[key] == aunt_sue[key]
            end
        end
            return id
        end
    end
end

open("input.txt") do file
    input = read(file, String)
    aunts = parse_input(input)
    @time println("Part 1: $(part1(aunts))")
    @time println("Part 2: $(part2(aunts))")
end
