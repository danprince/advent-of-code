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
    parts = split(expr)

    if length(parts) == 1
        readwire!(wires, parts[1])

    elseif length(parts) == 2
        # NOT is the only unary operator
        ~readwire!(wires, parts[2])

    elseif length(parts) == 3
        lhs, op, rhs = parts
        a = readwire!(wires, lhs)
        b = readwire!(wires, rhs)

        if op == "AND"
            a & b
        elseif op == "OR"
            a | b
        elseif op == "LSHIFT"
            a << b
        elseif op == "RSHIFT"
            a >> b
        end
    end
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
