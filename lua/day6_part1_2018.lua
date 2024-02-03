
local points = {}
local maxX, maxY = 0, 0

for line in io.lines("input.txt") do
    local coords = {}
    for coord in line:gmatch("%d+") do
        coords[#coords+1] = tonumber(coord)
    end
    local x, y = coords[1], coords[2]
    if x > maxX then
        maxX = x
    end
    if y > maxY then
        maxY = y
    end
    points[#points+1] = {x = x, y = y}
end

local grid = {}
for i = 1, maxX+2 do
    grid[i] = {}
    for j = 1, maxY+2 do
        grid[i][j] = 0
    end
end

local areas = {}
local infinite = {}
for i = 1, #points do
    areas[i] = 0
    infinite[i] = false
end

for i = 1, maxX+2 do
    for j = 1, maxY+2 do
        local minDist = maxX + maxY
        for k, point in ipairs(points) do
            local dist = math.abs(point.x - i) + math.abs(point.y - j)
            if dist < minDist then
                minDist = dist
                grid[i][j] = k
            elseif dist == minDist then
                grid[i][j] = -1
            end
        end
        if grid[i][j] ~= -1 then
            if i == 1 or j == 1 or i == maxX+2 or j == maxY+2 then
                infinite[grid[i][j]] = true
            end
            areas[grid[i][j]] = areas[grid[i][j]] + 1
        end
    end
end

local maxArea = 0
for i, area in ipairs(areas) do
    if not infinite[i] and area > maxArea then
        maxArea = area
    end
end

print(maxArea)
