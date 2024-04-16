local function calculateWaysToWinLongRace(time, record)
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

local time, distance
local count = 0

for line in file:lines() do
    if line ~= "" then
        local parts = {}
        for part in string.gmatch(line, "[^:]+") do
            table.insert(parts, part)
        end
        local value = string.gsub(parts[2], " ", "")
        if count == 0 then
            time = tonumber(value)
            count = count + 1
        else
            distance = tonumber(value)
        end
    end
end

file:close()

local waysToWin = calculateWaysToWinLongRace(time, distance)

print(waysToWin)