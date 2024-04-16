function isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax)
    if xPos < xMin and xVel < 0 then return true end
    if xPos > xMax and xVel > 0 then return true end
    if yPos < yMin and yVel < 0 then return true end
    if yPos > yMax and yVel > 0 then return false end
    return false
end

local file = io.open("input.txt", "r")
local line = file:read("*line")
file:close()

local parts = {}
for part in string.gmatch(line, "[^,]+") do
    table.insert(parts, part)
end

local xRange = string.sub(parts[1], 16)
local yRange = string.sub(parts[2], 4)

local xMin, xMax = xRange:match("(%-?%d+)..(%-?%d+)")
local yMin, yMax = yRange:match("(%-?%d+)..(%-?%d+)")

xMin = tonumber(xMin)
xMax = tonumber(xMax)
yMin = tonumber(yMin)
yMax = tonumber(yMax)

local velocities = {}
for xVel = -1000, 1000 do
    for yVel = -1000, 1000 do
        local xPos, yPos = 0, 0
        local curXVel, curYVel = xVel, yVel
        local inTargetArea = false

        while true do
            xPos = xPos + curXVel
            yPos = yPos + curYVel
            
            if xPos >= xMin and xPos <= xMax and yPos >= yMin and yPos <= yMax then
                inTargetArea = true
                break
            end
            
            if isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax) then
                break
            end

            if curXVel > 0 then
                curXVel = curXVel - 1
            elseif curXVel < 0 then
                curXVel = curXVel + 1
            end

            curYVel = curYVel - 1
        end

        if inTargetArea then
            local velocityKey = tostring(xVel) .. "," .. tostring(yVel)
            velocities[velocityKey] = true
        end
    end
end

function countTable(t)
    local count = 0
    for _ in pairs(t) do count = count + 1 end
    return count
end

print(countTable(velocities))