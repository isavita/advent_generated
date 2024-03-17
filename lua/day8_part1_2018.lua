-- Read input from file
function readInput(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Failed to open file: " .. filename)
    end
    local line = file:read("*l")
    file:close()

    local numbers = {}
    for part in string.gmatch(line, "%d+") do
        table.insert(numbers, tonumber(part))
    end
    return numbers
end

-- Parse the tree and calculate the metadata sum
function parseTree(data, index)
    local childCount, metaCount = data[index], data[index + 1]
    index = index + 2

    local sum = 0
    for i = 1, childCount do
        local childSum, newIndex = parseTree(data, index)
        sum = sum + childSum
        index = newIndex
    end

    for i = 1, metaCount do
        sum = sum + data[index + i - 1]
    end
    index = index + metaCount

    return sum, index
end

-- Main function
local numbers = readInput("input.txt")
local sum, _ = parseTree(numbers, 1)
print(sum)