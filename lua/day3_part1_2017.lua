
-- Step 1: Read input
local file = io.open("input.txt", "r")
local target = tonumber(file:read("*line"))
file:close()

-- Step 2: Find the square
local sideLength = math.ceil(math.sqrt(target))
if sideLength % 2 == 0 then
    sideLength = sideLength + 1
end

-- Step 3: Find distance to the nearest middle point
local maxValue = sideLength * sideLength
local stepsFromEdge = (sideLength - 1) / 2
local distanceToMiddle = 0

for i = 0, 3 do
    local middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
    local distance = math.abs(target - middlePoint)
    if distance < distanceToMiddle or i == 0 then
        distanceToMiddle = distance
    end
end

-- Step 4: Calculate Manhattan Distance
local manhattanDistance = stepsFromEdge + distanceToMiddle

print(manhattanDistance)
