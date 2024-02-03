
local discs = {}
local file = io.open("input.txt", "r")
for line in file:lines() do
    local _, _, totalPositions, startPosition = string.find(line, "Disc #%d+ has (%d+) positions; at time=0, it is at position (%d+).")
    table.insert(discs, {totalPositions = tonumber(totalPositions), startPosition = tonumber(startPosition)})
end
file:close()

table.insert(discs, {totalPositions = 11, startPosition = 0})

local time = 0
while true do
    local success = true
    for i, disc in ipairs(discs) do
        local position = (disc.startPosition + time + i) % disc.totalPositions
        if position ~= 0 then
            success = false
            break
        end
    end
    if success then
        print(time)
        break
    end
    time = time + 1
end
