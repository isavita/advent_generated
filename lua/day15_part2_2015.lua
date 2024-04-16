function readIngredients(filename)
    local file = io.open(filename, "r")
    local ingredients = {}
    for line in file:lines() do
        local parts = {}
        for part in line:gmatch("%S+") do
            table.insert(parts, part)
        end

        local capacity = tonumber(parts[3]:sub(1, -2))
        local durability = tonumber(parts[5]:sub(1, -2))
        local flavor = tonumber(parts[7]:sub(1, -2))
        local texture = tonumber(parts[9]:sub(1, -2))
        local calories = tonumber(parts[11])

        table.insert(ingredients, {
            name = parts[1],
            capacity = capacity,
            durability = durability,
            flavor = flavor,
            texture = texture,
            calories = calories
        })
    end
    file:close()
    return ingredients
end

function calculateMaxScore(ingredients, index, remaining, teaspoons, targetCalories)
    if index == #ingredients then
        local newTeaspoons = {table.unpack(teaspoons)}
        table.insert(newTeaspoons, remaining)
        if calculateCalories(ingredients, newTeaspoons) == targetCalories then
            return score(ingredients, newTeaspoons)
        end
        return 0
    end

    local maxScore = 0
    for i = 0, remaining do
        local newTeaspoons = {table.unpack(teaspoons)}
        table.insert(newTeaspoons, i)
        local currentScore = calculateMaxScore(ingredients, index + 1, remaining - i, newTeaspoons, targetCalories)
        if currentScore > maxScore then
            maxScore = currentScore
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

    capacity = math.max(0, capacity)
    durability = math.max(0, durability)
    flavor = math.max(0, flavor)
    texture = math.max(0, texture)

    return capacity * durability * flavor * texture
end

function calculateCalories(ingredients, teaspoons)
    local calories = 0
    for i, ingredient in ipairs(ingredients) do
        calories = calories + ingredient.calories * teaspoons[i]
    end
    return calories
end

local ingredients = readIngredients("input.txt")
local maxScore = calculateMaxScore(ingredients, 1, 100, {}, 500)
print(maxScore)