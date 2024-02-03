
-- Read input from file
local file = io.open("input.txt", "r")
local line = file:read()
file:close()

local numbers = {}
for num in line:gmatch("%d+") do
    table.insert(numbers, tonumber(num))
end

-- Parse the tree and calculate the value of the root node
local index = 1
local function parseTree(data)
    local childCount, metaCount = data[index], data[index + 1]
    index = index + 2

    local childValues = {}
    for i = 1, childCount do
        local childValue
        childValue = parseTree(data)
        table.insert(childValues, childValue)
    end

    local value = 0
    if childCount == 0 then
        for i = 1, metaCount do
            value = value + data[index + i - 1]
        end
    else
        for i = 1, metaCount do
            local metadata = data[index + i - 1]
            if metadata <= childCount and metadata > 0 then
                value = value + childValues[metadata]
            end
        end
    end
    index = index + metaCount

    return value
end

local value = parseTree(numbers)
print(value)
