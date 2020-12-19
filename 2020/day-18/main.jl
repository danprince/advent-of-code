function evaluate(expr, operators)
    stack = []
    postfix = []

    for symbol in expr
        if '0' <= symbol <= '9'
            push!(postfix, symbol)

        elseif symbol == '('
            push!(stack, symbol)

        # pop operators to the stack until we find the opening brace
        elseif symbol == ')'
            while stack[end] != '('
                push!(postfix, pop!(stack))
            end

            pop!(stack)

        # pop higher or equal precendence operators
        elseif haskey(operators, symbol)
            operator = operators[symbol]

            while !isempty(stack) && haskey(operators, stack[end])
                prev_operator = operators[stack[end]]

                if prev_operator.precedence < operator.precedence
                    break
                end

                push!(postfix, pop!(stack))
            end

            push!(stack, symbol)
        end
    end

    while !isempty(stack)
        push!(postfix, pop!(stack))
    end

    result = []

    while !isempty(postfix)
        symbol = popfirst!(postfix)

        if '0' <= symbol <= '9'
            val = parse(Int, symbol)
            push!(result, val)
        elseif haskey(operators, symbol)
            lhs = pop!(result)
            rhs = pop!(result)
            op = operators[symbol]
            val = op.func(lhs, rhs)
            push!(result, val)
        end
    end

    first(result)
end

function part1(lines)
    operators = Dict(
        '+' => (func=+, precedence=0),
        '*' => (func=*, precedence=0),
    )

    sum(lines) do line
        evaluate(line, operators)
    end
end

function part2(lines)
    operators = Dict(
        '+' => (func=+, precedence=1),
        '*' => (func=*, precedence=0),
    )

    sum(lines) do line
        evaluate(line, operators)
    end
end

@test part1(["1 + 2 * 3 + 4 * 5 + 6"]) == 71
@test part1(["1 + (2 * 3) + (4 * (5 + 6))"]) == 51
@test part1(["2 * 3 + (4 * 5)"]) == 26
@test part1(["5 + (8 * 3 + 9 + 3 * 4 * 3)"]) == 437
@test part1(["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"]) == 12240
@test part1(["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"]) == 13632

@test part2(["1 + (2 * 3) + (4 * (5 + 6))"]) == 51
@test part2(["2 * 3 + (4 * 5)"]) == 46
@test part2(["5 + (8 * 3 + 9 + 3 * 4 * 3)"]) ==  1445
@test part2(["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"]) == 669060
@test part2(["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"]) == 23340

open("input.txt") do file
    input = readlines(file)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
