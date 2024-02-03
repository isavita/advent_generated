
function readReindeerDetails(filename)
    local file = io.open(filename, "r")
    local reindeers = {}
    
    for line in file:lines() do
        local parts = {}
        for part in line:gmatch("%S+") do
            table.insert(parts, part)
        end
        local speed = tonumber(parts[4])
        local flyTime = tonumber(parts[7])
        local restTime = tonumber(parts[14])
        
        table.insert(reindeers, {speed = speed, flyTime = flyTime, restTime = restTime, flying = true, distance = 0, points = 0, timeInMode = 0})
    end
    
    file:close()
    return reindeers
end

function simulateRaceWithPoints(reindeers, totalSeconds)
    for i = 1, totalSeconds do
        local maxDistance = 0
        for j, reindeer in ipairs(reindeers) do
            if reindeer.flying then
                reindeer.distance = reindeer.distance + reindeer.speed
            end
            reindeer.timeInMode = reindeer.timeInMode + 1
            if (reindeer.flying and reindeer.timeInMode == reindeer.flyTime) or (not reindeer.flying and reindeer.timeInMode == reindeer.restTime) then
                reindeer.flying = not reindeer.flying
                reindeer.timeInMode = 0
            end
            if reindeer.distance > maxDistance then
                maxDistance = reindeer.distance
            end
        end
        for j, reindeer in ipairs(reindeers) do
            if reindeer.distance == maxDistance then
                reindeer.points = reindeer.points + 1
            end
        end
    end
end

function findMaxPoints(reindeers)
    local maxPoints = 0
    for _, reindeer in ipairs(reindeers) do
        if reindeer.points > maxPoints then
            maxPoints = reindeer.points
        end
    end
    return maxPoints
end

local reindeers = readReindeerDetails("input.txt")
simulateRaceWithPoints(reindeers, 2503)
local maxPoints = findMaxPoints(reindeers)
print(maxPoints)
