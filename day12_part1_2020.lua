
local file = io.open("input.txt", "r")
local ship = {x = 0, y = 0, facing = 0}

for line in file:lines() do
    local action = line:sub(1, 1)
    local value = tonumber(line:sub(2))

    if action == 'N' then
        ship.y = ship.y + value
    elseif action == 'S' then
        ship.y = ship.y - value
    elseif action == 'E' then
        ship.x = ship.x + value
    elseif action == 'W' then
        ship.x = ship.x - value
    elseif action == 'L' then
        ship.facing = (ship.facing - value + 360) % 360
    elseif action == 'R' then
        ship.facing = (ship.facing + value) % 360
    elseif action == 'F' then
        if ship.facing == 0 then
            ship.x = ship.x + value
        elseif ship.facing == 90 then
            ship.y = ship.y - value
        elseif ship.facing == 180 then
            ship.x = ship.x - value
        elseif ship.facing == 270 then
            ship.y = ship.y + value
        end
    end
end

local manhattanDistance = math.abs(ship.x) + math.abs(ship.y)
print(manhattanDistance)
