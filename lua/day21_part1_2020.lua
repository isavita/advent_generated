local function contains(table, element)
    for _, value in pairs(table) do
        if value == element then
            return true
        end
    end
    return false
end

local allergenMap = {}
local ingredientCount = {}
local safeIngredients = {}

local file = io.open("input.txt", "r")
for line in file:lines() do
    local parts = {}
    for part in string.gmatch(line, "[^%(]+") do
        table.insert(parts, part)
    end
    local ingredients = {}
    for ingredient in string.gmatch(parts[1], "%S+") do
        table.insert(ingredients, ingredient)
        ingredientCount[ingredient] = (ingredientCount[ingredient] or 0) + 1
        safeIngredients[ingredient] = true
    end

    local allergens = {}
    if #parts > 1 then
        local allergenPart = string.sub(parts[2], 10, -2)
        for allergen in string.gmatch(allergenPart, "[^, ]+") do
            table.insert(allergens, allergen)
        end
    end

    for _, allergen in ipairs(allergens) do
        if not allergenMap[allergen] then
            allergenMap[allergen] = {}
            for _, ingredient in ipairs(ingredients) do
                allergenMap[allergen][ingredient] = true
            end
        else
            for ingredient in pairs(allergenMap[allergen]) do
                if not contains(ingredients, ingredient) then
                    allergenMap[allergen][ingredient] = nil
                end
            end
        end
    end
end
file:close()

for _, ingredients in pairs(allergenMap) do
    for ingredient in pairs(ingredients) do
        safeIngredients[ingredient] = nil
    end
end

local count = 0
for ingredient in pairs(safeIngredients) do
    count = count + (ingredientCount[ingredient] or 0)
end

print(count)