-- Read input from file
local file = io.open("input.txt", "r")
local line = file:read("*line")
file:close()

-- Extract ranges from the input
local xRangeString, yRangeString = line:match("target area: x=(.*), y=(.*)")
local xMin, xMax = xRangeString:match("(%-?%d+)..(%-?%d+)")
local yMin, yMax = yRangeString:match("(%-?%d+)..(%-?%d+)")
xMin, xMax, yMin, yMax = tonumber(xMin), tonumber(xMax), tonumber(yMin), tonumber(yMax)

-- Function to check if the projectile is moving away from the target area
local function isMovingAway(xPos, yPos, xVel, yVel)
    if (xPos < xMin and xVel < 0) or (xPos > xMax and xVel > 0) or (yPos < yMin and yVel < 0) then
        return true
    end
    return false
end

-- Main simulation loop
local maxY = -math.huge
for xVel = -1000, 1000 do
    for yVel = -1000, 1000 do
        local xPos, yPos = 0, 0
        local curXVel, curYVel = xVel, yVel
        local highestY = yPos
        while true do
            xPos = xPos + curXVel
            yPos = yPos + curYVel

            if xPos >= xMin and xPos <= xMax and yPos >= yMin and yPos <= yMax then
                if highestY > maxY then
                    maxY = highestY
                end
                break
            end

            if isMovingAway(xPos, yPos, curXVel, curYVel) then
                break
            end

            if curXVel > 0 then
                curXVel = curXVel - 1
            elseif curXVel < 0 then
                curXVel = curXVel + 1
            end

            curYVel = curYVel - 1
            if yPos > highestY then
                highestY = yPos
            end
        end
    end
end

-- Print the result
print(maxY)