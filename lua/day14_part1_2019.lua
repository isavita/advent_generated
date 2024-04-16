function parseChemical(s)
    local parts = {}
    for part in s:gmatch("%S+") do
        table.insert(parts, part)
    end
    local amount = tonumber(parts[1])
    local name = parts[2]
    return {name = name, amount = amount}
end

function calculateOre(chem, amount, reactions, ingredients, surplus)
    if chem == "ORE" then
        return amount
    end

    if not surplus[chem] then
        surplus[chem] = 0
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

local reactions = {}
local ingredients = {}
local surplus = {}

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

for line in file:lines() do
    local parts = {}
    for part in line:gmatch("[^=>]+") do
        table.insert(parts, part:match("^%s*(.-)%s*$"))
    end
    local output = parseChemical(parts[2])
    local inputs = {}
    for inChem in parts[1]:gmatch("[^,]+") do
        table.insert(inputs, parseChemical(inChem))
    end
    reactions[output.name] = output
    ingredients[output.name] = inputs
end

file:close()

-- Initialize surplus for each chemical
for chem in pairs(reactions) do
    surplus[chem] = 0
end

print(calculateOre("FUEL", 1, reactions, ingredients, surplus))