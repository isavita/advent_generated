function solve()
    allergen_map = Dict{String,Set{String}}()
    ingredient_count = Dict{String,Int}()
    safe_ingredients = Set{String}()

    for line in eachline("input.txt")
        parts = split(line, " (contains ")
        ingredients = split(parts[1])
        allergens = length(parts) > 1 ? split(parts[2][1:end-1], ", ") : String[]

        for ingredient in ingredients
            ingredient_count[ingredient] = get(ingredient_count, ingredient, 0) + 1
            push!(safe_ingredients, ingredient)
        end

        for allergen in allergens
            if !haskey(allergen_map, allergen)
                allergen_map[allergen] = Set(ingredients)
            else
                allergen_map[allergen] = intersect(allergen_map[allergen], Set(ingredients))
            end
        end
    end

    for ingredients in values(allergen_map)
        setdiff!(safe_ingredients, ingredients)
    end

    count = sum(ingredient_count[ingredient] for ingredient in safe_ingredients)
    println(count)
end

solve()