-- Read input from file
local file = io.open("input.txt", "r")
local containers = {}
for line in file:lines() do
    table.insert(containers, tonumber(line))
end
file:close()

local function findCombinations(containers, target, index, count, minCount, ways)
    if target == 0 then
        if minCount[1] == 0 or count < minCount[1] then
            minCount[1] = count
            ways[1] = 1
        elseif count == minCount[1] then
            ways[1] = ways[1] + 1
        end
        return
    end
    if target < 0 or index >= #containers then
        return
    end
    -- Include current container
    findCombinations(containers, target - containers[index + 1], index + 1, count + 1, minCount, ways)
    -- Exclude current container
    findCombinations(containers, target, index + 1, count, minCount, ways)
end

local minCount = {0}
local ways = {0}
findCombinations(containers, 150, 0, 0, minCount, ways)
print(ways[1])