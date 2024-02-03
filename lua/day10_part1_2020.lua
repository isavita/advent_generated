
local file = io.open("input.txt", "r")
local adapters = {}

for line in file:lines() do
    table.insert(adapters, tonumber(line))
end

table.sort(adapters)

local joltDifferences = { [3] = 1 }
local previousJoltage = 0

for i, adapter in ipairs(adapters) do
    local diff = adapter - previousJoltage
    joltDifferences[diff] = (joltDifferences[diff] or 0) + 1
    previousJoltage = adapter
end

local product = joltDifferences[1] * joltDifferences[3]
print(product)
