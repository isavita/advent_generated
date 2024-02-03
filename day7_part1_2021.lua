
local file = io.open("input.txt", "r")
local positions = {}

for line in file:lines() do
    for num_str in line:gmatch("%d+") do
        table.insert(positions, tonumber(num_str))
    end
end

table.sort(positions)

local function calculateFuel(currentPosition, newPosition)
    return math.abs(currentPosition - newPosition)
end

local min_fuel = math.huge
for i = positions[1], positions[#positions] do
    local fuel = 0
    for _, pos in ipairs(positions) do
        fuel = fuel + calculateFuel(pos, i)
    end
    if fuel < min_fuel then
        min_fuel = fuel
    end
end

print(min_fuel)
