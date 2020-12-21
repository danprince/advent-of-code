using Test

function parse_ingredients_list(input)
    matches = eachmatch(r"(.+) \(contains (.+)\)", input)

    map(matches) do m
        ingredients = split(m[1])
        allergens = split(m[2], ", ")
        ingredients => allergens
    end
end

function identify_allergens(ingredients_list)
    # mapping of allergen to ingredients that they may occur in
    candidates = Dict{AbstractString, Set{AbstractString}}()

    for (ingredients, allergens) in ingredients_list
        for allergen in allergens
            b = Set(ingredients)
            a = get!(candidates, allergen, b)
            intersect!(a, b)
        end
    end

    # mapping of ingredient to corresponding allergen
    contains = Dict()

    while !isempty(candidates)
        for (allergen, ingredients) in candidates
            if length(ingredients) == 1
                ingredient = ingredients |> first
                delete!(candidates, allergen)
                contains[ingredient] = allergen

                for (allergen, ingredients) in candidates
                    delete!(ingredients, ingredient)
                end
            end
        end
    end

    contains
end

function part1(input)
    ingredients_list = parse_ingredients_list(input)
    contains = identify_allergens(ingredients_list)
    allergenic_ingredients = keys(contains)

    sum(ingredients_list) do (ingredients, allergens)
        setdiff(ingredients, allergenic_ingredients) |> length
    end
end

function part2(input)
    ingredients_list = parse_ingredients_list(input)
    contains = identify_allergens(ingredients_list)
    present = Dict(i => a for (a, i) in contains)

    allergens = present |> keys |> collect |> sort
    ingredients = map(a -> present[a], allergens)
    join(ingredients, ',')
end

example = """
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
"""

@test part1(example) == 5
@test part2(example) == "mxmxvkd,sqjhc,fvjkl"

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end
