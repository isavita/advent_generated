local file = io.open("input.txt", "r")
local discs = {}

for line in file:lines() do
    local _, _, discNum, totalPositions, startPosition = string.find(line, "Disc #(%d+) has (%d+) positions; at time=0, it is at position (%d+).")
    discs[tonumber(discNum)] = { totalPositions = tonumber(totalPositions), startPosition = tonumber(startPosition) }
end
file:close()

local time = 0
while true do
    local allPositionsZero = true
    for i, disc in pairs(discs) do
        local position = (disc.startPosition + time + i) % disc.totalPositions
        if position ~= 0 then
            allPositionsZero = false
            break
        end
    end
    if allPositionsZero then
        print(time)
        break
    end
    time = time + 1
end