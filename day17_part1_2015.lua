
local containers = {}
local file = io.open("input.txt", "r")
if file then
    for line in file:lines() do
        table.insert(containers, tonumber(line))
    end
    file:close()
end

local function countCombinations(containers, target, index)
    if target == 0 then
        return 1
    end
    if target < 0 or index > #containers then
        return 0
    end
    return countCombinations(containers, target - containers[index], index + 1) +
           countCombinations(containers, target, index + 1)
end

print(countCombinations(containers, 150, 1))
