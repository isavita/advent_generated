
local file = io.open("input.txt", "r")
local template = file:read("*line")
local rules = {}

for line in file:lines() do
    if line ~= "" then
        local parts = {}
        for part in string.gmatch(line, "%S+") do
            table.insert(parts, part)
        end
        rules[parts[1]] = parts[3]
    end
end

local pairCounts = {}
for i = 1, #template - 1 do
    pairCounts[template:sub(i, i + 1)] = (pairCounts[template:sub(i, i + 1)] or 0) + 1
end

for step = 1, 40 do
    local newPairCounts = {}
    for pair, count in pairs(pairCounts) do
        if rules[pair] then
            local insert = rules[pair]
            newPairCounts[pair:sub(1, 1) .. insert] = (newPairCounts[pair:sub(1, 1) .. insert] or 0) + count
            newPairCounts[insert .. pair:sub(2, 2)] = (newPairCounts[insert .. pair:sub(2, 2)] or 0) + count
        else
            newPairCounts[pair] = (newPairCounts[pair] or 0) + count
        end
    end
    pairCounts = newPairCounts
end

local elementCounts = {}
for pair, count in pairs(pairCounts) do
    elementCounts[pair:sub(1, 1)] = (elementCounts[pair:sub(1, 1)] or 0) + count
end
elementCounts[template:sub(#template, #template)] = (elementCounts[template:sub(#template, #template)] or 0) + 1

local maxCount, minCount = 0, 2^63 - 1
for _, count in pairs(elementCounts) do
    maxCount = math.max(maxCount, count)
    minCount = math.min(minCount, count)
end

print(maxCount - minCount)
