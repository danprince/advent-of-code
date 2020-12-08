using Test

mutable struct Program
    acc::Int
    ip::Int
    code::Vector
end

function Program(src::AbstractString)
    lines = split(src, "\n")

    code = map(lines) do line
        parts = split(line)
        op = Symbol(parts[1])
        val = parse(Int, parts[2])
        (op, val)
    end

    Program(0, 1, code)
end

function reset!(vm::Program)
    vm.ip = 1
    vm.acc = 0
end

function exec!(vm::Program)
    (op, val) = vm.code[vm.ip]

    if op == :jmp
        vm.ip += val
    elseif op == :acc
        vm.acc += val
        vm.ip += 1
    else
        vm.ip += 1
    end
end

function part1(vm::Program)
    seen = Set{Int}()

    while !(vm.ip in seen)
        push!(seen, vm.ip)
        exec!(vm)
    end

    vm.acc
end

function part2(vm::Program)
    for i in 1:length(vm.code)
        (op, val) = vm.code[i]

        if op == :acc
            continue
        end

        # attempt to fix instruction
        vm.code[i] = if op == :jmp
            (:nop, val)
        else
            (:jmp, val)
        end

        seen = Set{Int}()

        while !(vm.ip in seen)
            if vm.ip > length(vm.code)
                return vm.acc
            else
                push!(seen, vm.ip)
                exec!(vm)
            end
        end

        # restore original instruction
        vm.code[i] = (op, val)
        reset!(vm)
    end
end

example = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

@test part1(example |> Program) == 5
@test part2(example |> Program) == 8

open("input.txt") do file
    input = read(file, String) |> strip
    @time println("Part 1: $(part1(input |> Program))")
    @time println("Part 2: $(part2(input |> Program))")
end
