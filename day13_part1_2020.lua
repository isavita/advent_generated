
local file = io.open("input.txt", "r")
local earliestDeparture = tonumber(file:read())
local busIDs = {}
for id in file:read():gmatch("%w+") do
    table.insert(busIDs, id)
end
file:close()

local earliestBusID = 0
local minWaitTime = earliestDeparture

for _, id in ipairs(busIDs) do
    if id == "x" then
        goto continue
    end
    local busID = tonumber(id)
    local waitTime = busID - (earliestDeparture % busID)
    if waitTime < minWaitTime then
        minWaitTime = waitTime
        earliestBusID = busID
    end
    ::continue::
end

print(earliestBusID * minWaitTime)
