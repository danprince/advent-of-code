using Test
using Combinatorics

function parse_routes(input)
    routes = Dict()
    weights = Dict()
    nodes = Set()

    for m in eachmatch(r"(\w+) to (\w+) = (\d+)", input)
        from, to = m[1], m[2]
        weight = parse(Int, m[3])
        weights[from=>to] = weights[to=>from] = weight
        push!(nodes, from, to)
    end

    for route in collect(nodes) |> permutations
        routes[route] = sum(2:length(route)) do i
            from = route[i-1]
            to = route[i]
            weights[from=>to]
        end
    end

    routes
end

part1(routes) = routes |> values |> minimum
part2(routes) = routes |> values |> maximum

example = "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141" |> parse_routes

@test part1(example) == 605
@test part2(example) == 982

open("input.txt") do file
    routes = read(file, String) |> parse_routes
    println("Part 1: $(part1(routes))")
    println("Part 2: $(part2(routes))")
end

