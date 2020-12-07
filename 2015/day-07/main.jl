using Test

"""
Parse expr as a number or take a value from the wire with that id.
"""
function readwire!(wires, expr)
    val = tryparse(Int, expr)

    if val != nothing
        return val
    else
        return take!(wires[expr])
    end
end

function evaluate!(wires, expr)
    m = match(r"NOT (\w+)", expr)

    if m != nothing
        a = readwire!(wires, m[1])
        return ~a
    end

    m = match(r"(\w+) AND (\w+)", expr)

    if m != nothing
        a = readwire!(wires, m[1])
        b = readwire!(wires, m[2])
        return a & b
    end

    m = match(r"(\w+) OR (\w+)", expr)

    if m != nothing
        a = readwire!(wires, m[1])
        b = readwire!(wires, m[2])
        return a | b
    end

    m = match(r"(\w+) LSHIFT (\d+)", expr)

    if m != nothing
        a = readwire!(wires, m[1])
        b = readwire!(wires, m[2])
        return a << b
    end

    m = match(r"(\w+) RSHIFT (\d+)", expr)

    if m != nothing
        a = readwire!(wires, m[1])
        b = readwire!(wires, m[2])
        return a >> b
    end

    return readwire!(wires, expr)
end

function emulate(input; overrides=Dict())
    lines = split(input, "\n")
    wires = Dict()

    for m in eachmatch(r"(.*) -> (\w+)", input)
        expr, id = m.captures
        wire = wires[id] = Channel{UInt16}()

        @async begin
            value = if haskey(overrides, id)
                overrides[id]
            else
                evaluate!(wires, expr)
            end

            # multiple wires can read from this channel, so make sure
            # it always has the value
            while true
                put!(wire, value)
            end
        end
    end

    wires
end

function part1(input)
    wires = emulate(input)
    take!(wires["a"])
end

function part2(input)
    wires = emulate(input)
    new_b = take!(wires["a"])

    wires = emulate(input, overrides=Dict("b"=>new_b))
    take!(wires["a"])
end

open("input.txt") do file
    input = read(file, String) |> strip
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
