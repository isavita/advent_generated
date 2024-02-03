
local file = io.open("input.txt", "r")
local positions = {}
for line in file:lines() do
    for num_str in line:gmatch("%d+") do
        table.insert(positions, tonumber(num_str))
    end
end
file:close()

table.sort(positions)

local function calculateNewFuel(currentPosition, newPosition)
    local diff = math.abs(currentPosition - newPosition)
    return (diff * (diff + 1)) / 2
end

local min_fuel = math.huge
for i = positions[1], positions[#positions] do
    local fuel = 0
    for _, pos in ipairs(positions) do
        fuel = fuel + calculateNewFuel(pos, i)
    end
    if fuel < min_fuel then
        min_fuel = fuel
    end
end

print(min_fuel)
