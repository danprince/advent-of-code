# Not at all happy with this solution. The nested "for" iterator solution
# is hardcoded to generate the numbers for 4 ingredients.
#
# Pretty sure there's a recursive solution for generating those numbers,
# or a better Julia programmer would be able to write a macro that nests
# N loops based number of ingredients.
#
# Initially solved P1 with random hill climbing, but couldn't get a P2
# solution, so switched to the brute force option.
#
# Could probably be solved elegantly with an optimizer/constraint solver
# like https://github.com/jump-dev/JuMP.jl

struct Ingredient
    name::String
    capacity::Int
    durability::Int
    flavor::Int
    texture::Int
    calories::Int
end

function Ingredient(str)
    rx = r"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)"
    m = match(rx, str)
    name = String(m[1])
    attrs = m.captures[2:end]
    capacity, durability, flavor, texture, calories = parse.(Int, attrs)
    Ingredient(name, capacity, durability, flavor, texture, calories)
end

function calculate_score(cookie, ingredients; desired_calories = nothing)
    capacity = 0
    durability = 0
    flavor = 0
    texture = 0
    calories = 0

    for i in 1:length(cookie)
        ingredient = ingredients[i]
        spoons = cookie[i]
        capacity += ingredient.capacity * spoons
        durability += ingredient.durability * spoons
        flavor += ingredient.flavor * spoons
        texture += ingredient.texture * spoons
        calories += ingredient.calories * spoons
    end

    if desired_calories !== nothing && desired_calories != calories
        return 0
    end

    max(0, capacity) * max(0, durability) * max(0, flavor) * max(0, texture)
end

function part1(ingredients)
    best_score = 0

    for a in 0:100, b in 0:100-a, c in 0:100-a-b
        d = 100 - a - b - c
        cookie = [a, b, c, d]
        score = calculate_score(cookie, ingredients)

        if score >= best_score
            best_score = score
        end
    end

    best_score
end

function part2(ingredients)
    best_score = 0

    for a in 0:100, b in 0:100-a, c in 0:100-a-b
        d = 100 - a - b - c
        cookie = [a, b, c, d]
        score = calculate_score(cookie, ingredients, desired_calories=500)

        if score >= best_score
            best_score = score
        end
    end

    best_score
end

open("input.txt") do file
    input = read(file, String) |> strip
    lines = split(input, "\n")
    ingredients = Ingredient.(lines)
    println("Part 1: $(part1(ingredients))")
    println("Part 2: $(part2(ingredients))")
end
