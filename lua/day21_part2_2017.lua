function split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

function reverse(s)
    return s:reverse()
end

function rotate(input)
    local parts = split(input, "/")
    local size = #parts
    local newParts = {}
    for x = 1, size do
        local newRow = ""
        for y = size, 1, -1 do
            newRow = newRow .. parts[y]:sub(x, x)
        end
        table.insert(newParts, newRow)
    end
    return table.concat(newParts, "/")
end

function flip(input)
    local parts = split(input, "/")
    for i, part in ipairs(parts) do
        parts[i] = reverse(part)
    end
    return table.concat(parts, "/")
end

function enhance(input, rules, memo)
    if memo[input] then
        return memo[input]
    end

    local original = input
    for i = 1, 4 do
        if rules[input] then
            memo[original] = rules[input]
            return rules[input]
        end
        input = rotate(input)
    end
    input = flip(input)
    for i = 1, 4 do
        if rules[input] then
            memo[original] = rules[input]
            return rules[input]
        end
        input = rotate(input)
    end
    return ""
end

local rules = {}
local memo = {}
local file = io.open("input.txt", "r")
for line in file:lines() do
    local parts = split(line, " => ")
    rules[parts[1]] = parts[2]
end
file:close()

local grid = { ".#.", "..#", "###" }

for i = 1, 18 do
    local newSize
    local subSize
    if #grid % 2 == 0 then
        subSize = 2
        newSize = #grid / 2 * 3
    else
        subSize = 3
        newSize = #grid / 3 * 4
    end

    local newGrid = {}
    for x = 1, newSize do
        newGrid[x] = ""
    end

    for y = 1, #grid, subSize do
        for x = 1, #grid, subSize do
            local square = {}
            for dy = 0, subSize - 1 do
                table.insert(square, grid[y + dy]:sub(x, x + subSize - 1))
            end
            local newSquare = enhance(table.concat(square, "/"), rules, memo)
            local newSquareParts = split(newSquare, "/")
            for dy, row in ipairs(newSquareParts) do
                newGrid[(y - 1) / subSize * (subSize + 1) + dy] = newGrid[(y - 1) / subSize * (subSize + 1) + dy] .. row
            end
        end
    end
    grid = newGrid
end

local count = 0
for _, row in ipairs(grid) do
    for pixel in row:gmatch(".") do
        if pixel == '#' then
            count = count + 1
        end
    end
end

print(count)