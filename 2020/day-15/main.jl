function play(input; nth = 2020)
    numbers = parse.(Int, split(input, ","))
    mem1 = Dict()
    mem2 = Dict()
    prev = nothing

    for turn in 1:nth
        spoken = nothing

        if turn <= length(numbers)
            spoken = numbers[turn]
        else
            # last time spoken
            s1 = get(mem1, prev, nothing)

            # penultimate time spoken
            s2 = get(mem2, prev, nothing)

            if s2 == nothing
                # has only been spoken once before
                spoken = 0
            else
                spoken = s1 - s2
            end
        end

        mem2[spoken] = get(mem1, spoken, nothing)
        mem1[spoken] = turn

        print("$spoken ")

        prev = spoken
    end

    println()
    prev
end

part1(input) = play(input, nth = 100)
part2(input) = play(input, nth = 30000000)

open("example.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    #println("Part 2: $(part2(input))")
end
