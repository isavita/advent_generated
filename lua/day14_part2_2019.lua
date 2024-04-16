function parseChemical(s)
    local amount, name = s:match("(%d+) (%w+)")
    return {name = name, amount = tonumber(amount)}
end

function calculateOre(chem, amount, reactions, ingredients, surplus)
    surplus[chem] = surplus[chem] or 0  -- Initialize surplus for the chemical if not already initialized

    if chem == "ORE" then
        return amount
    end

    if surplus[chem] >= amount then
        surplus[chem] = surplus[chem] - amount
        return 0
    end

    amount = amount - surplus[chem]
    surplus[chem] = 0
    local reaction = reactions[chem]
    local times = math.ceil(amount / reaction.amount)
    local ore = 0

    for _, ingredient in ipairs(ingredients[chem]) do
        ore = ore + calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    end

    surplus[chem] = surplus[chem] + times * reaction.amount - amount
    return ore
end

function maxFuel(reactions, ingredients, oreAvailable)
    local low, high = 0, oreAvailable
    while low < high do
        local mid = math.floor((low + high + 1) / 2)
        if calculateOre("FUEL", mid, reactions, ingredients, {}) > oreAvailable then
            high = mid - 1
        else
            low = mid
        end
    end
    return low
end

local reactions = {}
local ingredients = {}

-- Assuming the file exists and is formatted correctly
local file = io.open("input.txt", "r")
if file then
    for line in file:lines() do
        local parts = {}
        for part in line:gmatch("[^=>]+") do
            table.insert(parts, part:match("^%s*(.-)%s*$")) -- Trim spaces
        end
        local output = parseChemical(parts[2])
        local inputs = {}
        for inPart in parts[1]:gmatch("[^,]+") do
            table.insert(inputs, parseChemical(inPart))
        end
        reactions[output.name] = output
        ingredients[output.name] = inputs
    end
    file:close()
else
    print("Error: Unable to open file.")
    return
end

local oreAvailable = 1000000000000
print(maxFuel(reactions, ingredients, oreAvailable))