
local file = io.open("input.txt", "r")
local horizontalPosition = 0
local depth = 0
local aim = 0

for line in file:lines() do
    local command = {}
    for word in line:gmatch("%S+") do
        table.insert(command, word)
    end

    local direction = command[1]
    local units = tonumber(command[2])

    if direction == "forward" then
        horizontalPosition = horizontalPosition + units
        depth = depth + aim * units
    elseif direction == "down" then
        aim = aim + units
    elseif direction == "up" then
        aim = aim - units
    end
end

local product = horizontalPosition * depth
print(product)
file:close()
