
function parseCoordinates(input)
    local coordinates = {}
    for line in input:gmatch("[^\n]+") do
        local x, y = line:match("(%d+), (%d+)")
        table.insert(coordinates, {x=tonumber(x), y=tonumber(y)})
    end
    return coordinates
end

function findRegionSize(coordinates, maxDistance)
    local minX, minY, maxX, maxY = findBoundingBox(coordinates)
    local regionSize = 0

    for x = minX, maxX do
        for y = minY, maxY do
            local totalDistance = 0

            for _, c in ipairs(coordinates) do
                totalDistance = totalDistance + manhattanDistance(x, y, c.x, c.y)
            end

            if totalDistance < maxDistance then
                regionSize = regionSize + 1
            end
        end
    end

    return regionSize
end

function findBoundingBox(coordinates)
    local minX, minY = math.huge, math.huge
    local maxX, maxY = -math.huge, -math.huge

    for _, c in ipairs(coordinates) do
        if c.x < minX then minX = c.x end
        if c.y < minY then minY = c.y end
        if c.x > maxX then maxX = c.x end
        if c.y > maxY then maxY = c.y end
    end

    return minX, minY, maxX, maxY
end

function manhattanDistance(x1, y1, x2, y2)
    return math.abs(x1 - x2) + math.abs(y1 - y2)
end

-- Read input from file
local file = io.open("input.txt", "r")
local content = file:read("*all")
file:close()

-- Parse coordinates
local coordinates = parseCoordinates(content)

-- Find the region size
local regionSize = findRegionSize(coordinates, 10000)

-- Print the result
print(regionSize)
