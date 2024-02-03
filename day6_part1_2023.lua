
function calculateWaysToWin(time, record)
    local waysToWin = 0
    for holdTime = 1, time - 1 do
        local travelTime = time - holdTime
        local distance = holdTime * travelTime
        if distance > record then
            waysToWin = waysToWin + 1
        end
    end
    return waysToWin
end

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local times = {}
local distances = {}

for line in file:lines() do
    local values = {}
    for value in line:gmatch("%S+") do
        table.insert(values, tonumber(value))
    end
    if #times == 0 then
        times = values
    else
        distances = values
    end
end

file:close()

local totalWays = 1
for i = 1, #times do
    local ways = calculateWaysToWin(times[i], distances[i])
    totalWays = totalWays * ways
end

print(totalWays)
