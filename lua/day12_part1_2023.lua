
function parseStringToInts(numbersLine)
    local numbers = {}
    local numbersParts = {}
    for numberStr in string.gmatch(numbersLine, "[^,]+") do
        table.insert(numbersParts, numberStr)
    end

    for _, numberStr in ipairs(numbersParts) do
        local number = tonumber(numberStr)
        table.insert(numbers, number)
    end

    return numbers
end

function countArrangementsRecursive(row, iSprings, iGroup, iContiguousDamaged, cache)
    if iSprings == #row.Springs + 1 then
        if iGroup == #row.Group + 1 and iContiguousDamaged == 0 then
            return 1
        elseif iGroup == #row.Group and iContiguousDamaged == row.Group[iGroup] then
            return 1
        end
        return 0
    end

    local cacheKey = {iSprings, iGroup, iContiguousDamaged}
    if cache[cacheKey] then
        return cache[cacheKey]
    end

    local res = 0
    local char = row.Springs:sub(iSprings, iSprings)
    if char == '.' or char == '?' then
        if iContiguousDamaged == 0 then
            res = res + countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache)
        elseif iContiguousDamaged == row.Group[iGroup] then
            res = res + countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache)
        end
    end
    if char == '#' or char == '?' then
        if iGroup < #row.Group + 1 and iContiguousDamaged < row.Group[iGroup] then
            res = res + countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache)
        end
    end

    cache[cacheKey] = res
    return res
end

function countArrangements(row)
    return countArrangementsRecursive(row, 1, 1, 0, {})
end

function solve(input)
    local res = 0
    for _, line in ipairs(input) do
        local parts = {}
        for part in string.gmatch(line, "%S+") do
            table.insert(parts, part)
        end
        local springs = parts[1]
        local group = parseStringToInts(parts[2])

        local row = {Springs = springs, Group = group}
        res = res + countArrangements(row)
    end

    return res
end

function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*all")
    file:close()

    local lines = {}
    for line in string.gmatch(content, "[^\n]+") do
        table.insert(lines, line)
    end

    return lines
end

local input = readFile("input.txt")
print(solve(input))
