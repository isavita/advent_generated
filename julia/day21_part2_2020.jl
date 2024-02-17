
allergenMap = Dict{String, Dict{String, Bool}}()
ingredientAllergen = Dict{String, String}()

open("input.txt") do file
    for line in eachline(file)
        parts = split(line, " (contains ")
        ingredients = split(parts[1])
        allergens = length(parts) > 1 ? split(parts[2][1:end-1], ", ") : []

        for allergen in allergens
            if !(allergen in keys(allergenMap))
                allergenMap[allergen] = Dict{String, Bool}()
                for ingredient in ingredients
                    allergenMap[allergen][ingredient] = true
                end
            else
                for (ingredient, _) in allergenMap[allergen]
                    if !(ingredient in ingredients)
                        delete!(allergenMap[allergen], ingredient)
                    end
                end
            end
        end
    end
end

while length(allergenMap) > 0
    for (allergen, ingredients) in allergenMap
        if length(ingredients) == 1
            ingredient = first(keys(ingredients))
            ingredientAllergen[allergen] = ingredient
            for (_, ingredients) in allergenMap
                delete!(ingredients, ingredient)
            end
            delete!(allergenMap, allergen)
        end
    end
end

allergens = sort(collect(keys(ingredientAllergen)))
result = [ingredientAllergen[allergen] for allergen in allergens]

println(join(result, ","))
