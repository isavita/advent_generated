function readIngredients(filename)
    local ingredients = {}
    for line in io.lines(filename) do
        local parts = {}
        for word in line:gmatch("%S+") do
            table.insert(parts, word)
        end
        local capacity = tonumber(parts[3]:sub(1, #parts[3]-1))
        local durability = tonumber(parts[5]:sub(1, #parts[5]-1))
        local flavor = tonumber(parts[7]:sub(1, #parts[7]-1))
        local texture = tonumber(parts[9]:sub(1, #parts[9]-1))
        table.insert(ingredients, {
            name = parts[1],
            capacity = capacity,
            durability = durability,
            flavor = flavor,
            texture = texture
        })
    end
    return ingredients
end

function calculateMaxScore(ingredients, index, remaining, teaspoons)
    if index == #ingredients then
        local copy = {table.unpack(teaspoons)}
        table.insert(copy, remaining)
        return score(ingredients, copy)
    end

    local maxScore = 0
    for i = 0, remaining do
        local copy = {table.unpack(teaspoons)}
        table.insert(copy, i)
        local s = calculateMaxScore(ingredients, index + 1, remaining - i, copy)
        if s > maxScore then
            maxScore = s
        end
    end
    return maxScore
end

function score(ingredients, teaspoons)
    local capacity, durability, flavor, texture = 0, 0, 0, 0
    for i, ingredient in ipairs(ingredients) do
        capacity = capacity + ingredient.capacity * teaspoons[i]
        durability = durability + ingredient.durability * teaspoons[i]
        flavor = flavor + ingredient.flavor * teaspoons[i]
        texture = texture + ingredient.texture * teaspoons[i]
    end

    if capacity < 0 then capacity = 0 end
    if durability < 0 then durability = 0 end
    if flavor < 0 then flavor = 0 end
    if texture < 0 then texture = 0 end

    return capacity * durability * flavor * texture
end

function findMaxScore(ingredients, totalTeaspoons)
    return calculateMaxScore(ingredients, 1, totalTeaspoons, {})
end

local ingredients = readIngredients("input.txt")
local maxScore = findMaxScore(ingredients, 100)
print(maxScore)