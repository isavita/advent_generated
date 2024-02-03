
function abs(x)
    if x < 0 then
        return -x
    end
    return x
end

function max(a, b)
    if a > b then
        return a
    end
    return b
end

function distance(x, y, z)
    return (abs(x) + abs(y) + abs(z)) / 2
end

local file = io.open("input.txt", "r")
if not file then
    print("File reading error")
    return
end

local input = file:read("*line")
file:close()

local directions = {}
for dir in input:gmatch("[^,]+") do
    table.insert(directions, dir)
end

local x, y, z = 0, 0, 0
local maxDistance = 0

for _, dir in ipairs(directions) do
    if dir == "n" then
        y = y + 1
        z = z - 1
    elseif dir == "ne" then
        x = x + 1
        z = z - 1
    elseif dir == "se" then
        x = x + 1
        y = y - 1
    elseif dir == "s" then
        y = y - 1
        z = z + 1
    elseif dir == "sw" then
        x = x - 1
        z = z + 1
    elseif dir == "nw" then
        x = x - 1
        y = y + 1
    end

    local curDistance = distance(x, y, z)
    maxDistance = max(maxDistance, curDistance)
end

print(maxDistance)
