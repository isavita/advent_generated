
local file = io.open("input.txt", "r")
local adapters = {0}

for line in file:lines() do
    table.insert(adapters, tonumber(line))
end

table.sort(adapters)
table.insert(adapters, adapters[#adapters] + 3)

function countArrangements(adapters)
    local ways = {[0] = 1}

    for i = 2, #adapters do
        local currentJoltage = adapters[i]
        ways[currentJoltage] = (ways[currentJoltage - 1] or 0) + (ways[currentJoltage - 2] or 0) + (ways[currentJoltage - 3] or 0)
    end

    return ways[adapters[#adapters]]
end

print(countArrangements(adapters))
