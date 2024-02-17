
using DelimitedFiles

struct Ingredient
    name::String
    capacity::Int
    durability::Int
    flavor::Int
    texture::Int
end

function readIngredients(filename)
    ingredients = []
    for line in eachline(filename)
        parts = split(line)
        if length(parts) < 11
            continue
        end
        capacity = parse(Int, parts[3][1:end-1])
        durability = parse(Int, parts[5][1:end-1])
        flavor = parse(Int, parts[7][1:end-1])
        texture = parse(Int, parts[9][1:end-1])
        push!(ingredients, Ingredient(parts[1], capacity, durability, flavor, texture))
    end
    return ingredients
end

function findMaxScore(ingredients, totalTeaspoons)
    calculateMaxScore(ingredients, 1, totalTeaspoons, [])
end

function calculateMaxScore(ingredients, index, remaining, teaspoons)
    if index == length(ingredients)
        push!(teaspoons, remaining)
        return score(ingredients, teaspoons)
    end
    maxScore = 0
    for i in 0:remaining
        score = calculateMaxScore(ingredients, index+1, remaining-i, vcat(teaspoons, i))
        maxScore = max(maxScore, score)
    end
    return maxScore
end

function score(ingredients, teaspoons)
    capacity = 0
    durability = 0
    flavor = 0
    texture = 0
    for (i, ingredient) in enumerate(ingredients)
        capacity += ingredient.capacity * teaspoons[i]
        durability += ingredient.durability * teaspoons[i]
        flavor += ingredient.flavor * teaspoons[i]
        texture += ingredient.texture * teaspoons[i]
    end
    capacity = max(0, capacity)
    durability = max(0, durability)
    flavor = max(0, flavor)
    texture = max(0, texture)
    return capacity * durability * flavor * texture
end

ingredients = readIngredients("input.txt")
maxScore = findMaxScore(ingredients, 100)
println(maxScore)
