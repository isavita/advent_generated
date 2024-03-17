-- Read input from file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Helper function to split a string
local function split(str, sep)
    local result = {}
    local pattern = string.format("([^%s]+)", sep)
    for part in str:gmatch(pattern) do
        table.insert(result, part)
    end
    return result
end

-- Helper function to get points with steps
local function getPointsWithSteps(path)
    local points = {}
    local current = {x = 0, y = 0}
    local steps = 0
    for _, move in ipairs(split(path, ",")) do
        local dir = move:sub(1, 1)
        local dist = tonumber(move:sub(2))
        for i = 1, dist do
            steps = steps + 1
            if dir == "U" then
                current.y = current.y + 1
            elseif dir == "D" then
                current.y = current.y - 1
            elseif dir == "L" then
                current.x = current.x - 1
            elseif dir == "R" then
                current.x = current.x + 1
            end
            local key = current.x .. "," .. current.y
            if not points[key] then
                points[key] = steps
            end
        end
    end
    return points
end

-- Solve the task
local wire1 = getPointsWithSteps(lines[1])
local wire2 = getPointsWithSteps(lines[2])

local minSteps = math.huge
for point, steps1 in pairs(wire1) do
    if wire2[point] then
        local totalSteps = steps1 + wire2[point]
        if totalSteps < minSteps then
            minSteps = totalSteps
        end
    end
end

print(minSteps)