local function read_file(file_path)
    local file = io.open(file_path, "r")
    local data = {}
    if file then
        for line in file:lines() do
            table.insert(data, line)
        end
        file:close()
    end
    return data
end

local function parse_line(line)
    local ingredients_part, allergens_part = line:match("^(.*) %(contains (.*)%)$")
    local ingredients = {}
    for ingredient in ingredients_part:gmatch("%S+") do
        table.insert(ingredients, ingredient)
    end
    local allergens = {}
    if allergens_part then
        for allergen in allergens_part:gmatch("[^, ]+") do
            table.insert(allergens, allergen)
        end
    end
    return ingredients, allergens
end

local function contains(tbl, item)
    for _, v in ipairs(tbl) do
        if v == item then
            return true
        end
    end
    return false
end

local function remove_ingredient_from_all(allergen_map, ingredient)
    for _, ingredients in pairs(allergen_map) do
        ingredients[ingredient] = nil
    end
end

local function solve(data)
    local allergen_map = {}
    local ingredient_allergen = {}

    for _, line in ipairs(data) do
        local ingredients, allergens = parse_line(line)
        for _, allergen in ipairs(allergens) do
            if not allergen_map[allergen] then
                allergen_map[allergen] = {}
                for _, ingredient in ipairs(ingredients) do
                    allergen_map[allergen][ingredient] = true
                end
            else
                local new_map = {}
                for ingredient in pairs(allergen_map[allergen]) do
                    if contains(ingredients, ingredient) then
                        new_map[ingredient] = true
                    end
                end
                allergen_map[allergen] = new_map
            end
        end
    end

    local progress = true
    while progress do
        progress = false
        for allergen, ingredients in pairs(allergen_map) do
            if next(ingredients) ~= nil and next(ingredients, next(ingredients)) == nil then
                local ingredient = next(ingredients)
                ingredient_allergen[allergen] = ingredient
                remove_ingredient_from_all(allergen_map, ingredient)
                allergen_map[allergen] = nil
                progress = true
            end
        end
    end

    local sorted_allergens = {}
    for allergen in pairs(ingredient_allergen) do
        table.insert(sorted_allergens, allergen)
    end
    table.sort(sorted_allergens)

    local result = {}
    for _, allergen in ipairs(sorted_allergens) do
        table.insert(result, ingredient_allergen[allergen])
    end

    return table.concat(result, ",")
end

local data = read_file("input.txt")
print(solve(data))