
struct Ingredient
    name::String
    capacity::Int
    durability::Int
    flavor::Int
    texture::Int
    calories::Int
end

function read_ingredients(filename::String)
    ingredients = Ingredient[]
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line)
            if length(parts) < 11
                continue
            end
            capacity = parse(Int, parts[3][1:end-1])
            durability = parse(Int, parts[5][1:end-1])
            flavor = parse(Int, parts[7][1:end-1])
            texture = parse(Int, parts[9][1:end-1])
            calories = parse(Int, parts[11])
            push!(ingredients, Ingredient(parts[1], capacity, durability, flavor, texture, calories))
        end
    end
    ingredients
end

function score(ingredients::Vector{Ingredient}, teaspoons::Vector{Int})
    capacity = max(sum(ingredient.capacity * teaspoons[i] for (i, ingredient) in enumerate(ingredients)), 0)
    durability = max(sum(ingredient.durability * teaspoons[i] for (i, ingredient) in enumerate(ingredients)), 0)
    flavor = max(sum(ingredient.flavor * teaspoons[i] for (i, ingredient) in enumerate(ingredients)), 0)
    texture = max(sum(ingredient.texture * teaspoons[i] for (i, ingredient) in enumerate(ingredients)), 0)
    capacity * durability * flavor * texture
end

function calculate_calories(ingredients::Vector{Ingredient}, teaspoons::Vector{Int})
    sum(ingredient.calories * teaspoons[i] for (i, ingredient) in enumerate(ingredients))
end

function find_max_score(ingredients::Vector{Ingredient}, total_teaspoons::Int, target_calories::Int)
    max_score = 0
    function backtrack(index::Int, remaining::Int, teaspoons::Vector{Int})
        if index == length(ingredients) - 1
            push!(teaspoons, remaining)
            if calculate_calories(ingredients, teaspoons) == target_calories
                max_score = max(max_score, score(ingredients, teaspoons))
            end
            pop!(teaspoons)
            return
        end
        for i in 0:remaining
            push!(teaspoons, i)
            backtrack(index + 1, remaining - i, teaspoons)
            pop!(teaspoons)
        end
    end
    backtrack(0, total_teaspoons, Int[])
    max_score
end

ingredients = read_ingredients("input.txt")
println(find_max_score(ingredients, 100, 500))
