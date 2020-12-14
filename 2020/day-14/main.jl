# set the bits of value from the mask string, leaving X bits unchanged
function xmask(value, mask)
    for i in 1:36
        bit = 36 - i

        if mask[i] == '1'
            value |= (1 << bit)
        elseif mask[i] == '0'
            value &= ~(1 << bit)
        end
    end

    value
end

# return all possible masked values, treating X bits as "floating"
# superpositions.
function floatmask(value, mask)
    values = Vector{UInt64}()
    stack = [(value, 1)]

    while !isempty(stack)
        value, start = pop!(stack)

        for i in start:36
            n = 36 - i

            if mask[i] == '1'
                # 1 bits always become 1
                value |= (1 << n)

            elseif mask[i] == 'X'
                # this bit is floating (it is both 0 and 1)
                zero = value & ~(1 << n)
                one = value | (1 << n)

                # handle the zero branch in this loop
                value = zero

                # push the one branch onto the stack to handle later
                push!(stack, (one, i + 1))
            end
        end

        push!(values, value)
    end

    values
end

function part1(input)
    mem = Dict{UInt64, UInt64}()
    mask = ""

    for m in eachmatch(r"(.+) = (.+)", input)
        name, value = m.captures

        if name == "mask"
            mask = value
        else
            m = match(r"(\d+)", name)
            addr = parse(UInt64, m[1])
            num = parse(UInt64, value)
            num = xmask(num, mask)
            mem[addr] = num
        end
    end

    values(mem) |> sum
end

function part2(input)
    mem = Dict{UInt64, UInt64}()
    mask = ""

    for m in eachmatch(r"(.+) = (.+)", input)
        name, value = m.captures

        if name == "mask"
            mask = value
        else
            m = match(r"(\d+)", name)
            addr = parse(UInt64, m[1])
            num = parse(UInt64, value)

            for addr in floatmask(addr, mask)
                mem[addr] = num
            end
        end
    end

    values(mem) |> sum
end

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
