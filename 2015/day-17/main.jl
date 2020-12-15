using Combinatorics

function parse_configs(input; eggnog = 150)
    lines = split(input)
    nums = parse.(Int, lines)
    configs = []

    for i in 1:length(nums)
        for config in combinations(nums, i)
            if sum(config) == eggnog
                push!(configs, config)
            end
        end
    end

    configs
end

function part1(configs)
    length(configs)
end

function part2(configs)
    smallest = minimum(length, configs)

    count(configs) do config
        length(config) == smallest
    end
end

open("input.txt") do file
    input = read(file, String) |> strip
    configs = parse_configs(input)
    println("Part 1: $(part1(configs))")
    println("Part 2: $(part2(configs))")
end
