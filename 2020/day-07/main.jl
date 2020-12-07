using Test

struct Graph
    parents::Dict{AbstractString, Dict{AbstractString, Int}}
    children::Dict{AbstractString, Dict{AbstractString, Int}}
end

function Graph(input::AbstractString)
    graph = Graph(Dict(), Dict())

    for m in eachmatch(r"(.+) bags contain (.+)\.", input)
        parent, contains = m.captures
        children = get!(Dict, graph.children, parent)

        for m in eachmatch(r"(\d+) ([a-z\s]+) bags?", contains)
            count, child = m.captures
            count = parse(Int, count)
            parents = get!(Dict, graph.parents, child)
            parents[parent] = count
            children[child] = count
        end
    end

    graph
end

function part1(graph::Graph; bag="shiny gold")
    stack = [bag]
    ancestors = []

    while !isempty(stack)
        bag = pop!(stack)

        if haskey(graph.parents, bag)
            parents = keys(graph.parents[bag])
            append!(stack, parents)
            append!(ancestors, parents)
        end
    end

    unique(ancestors) |> length
end

function part2(graph::Graph; bag="shiny gold")
    children = get(graph.children, bag, [])

    if isempty(children)
        return 0
    end

    sum(children) do (child, count)
        count + count * part2(graph, bag=child)
    end
end

example = Graph("light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

@test part1(example) == 4
@test part2(example) == 32

open("input.txt") do file
    graph = read(file, String) |> Graph
    println("Part 1: $(part1(graph))")
    println("Part 2: $(part2(graph))")
end
