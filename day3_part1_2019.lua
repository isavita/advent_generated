
function getPoints(path)
    local points = {}
    local current = {X = 0, Y = 0}
    for move in string.gmatch(path, "[^,]+") do
        local dir = string.sub(move, 1, 1)
        local steps = tonumber(string.sub(move, 2))
        for i = 1, steps do
            if dir == "U" then
                current.Y = current.Y + 1
            elseif dir == "D" then
                current.Y = current.Y - 1
            elseif dir == "L" then
                current.X = current.X - 1
            elseif dir == "R" then
                current.X = current.X + 1
            end
            points[current.X .. "," .. current.Y] = true
        end
    end
    return points
end

local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local wire1 = getPoints(lines[1])
local wire2 = getPoints(lines[2])

local intersections = {}
for point1 in pairs(wire1) do
    if wire2[point1] then
        intersections[point1] = true
    end
end

local minDistance = math.huge
for point in pairs(intersections) do
    local x, y = string.match(point, "(%-?%d+),(%-?%d+)")
    local distance = math.abs(tonumber(x)) + math.abs(tonumber(y))
    if distance < minDistance then
        minDistance = distance
    end
end

print(minDistance)
