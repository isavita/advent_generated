
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
        
        table.insert(reindeers, {speed = speed, flyTime = flyTime, restTime = restTime, flying = true, distance = 0, timeInMode = 0})
    end
    
    file:close()
    return reindeers
end

function simulateRace(reindeers, totalSeconds)
    for i = 1, totalSeconds do
        for j = 1, #reindeers do
            local reindeer = reindeers[j]
            if reindeer.flying then
                reindeer.distance = reindeer.distance + reindeer.speed
                reindeer.timeInMode = reindeer.timeInMode + 1
                if reindeer.timeInMode == reindeer.flyTime then
                    reindeer.flying = false
                    reindeer.timeInMode = 0
                end
            else
                reindeer.timeInMode = reindeer.timeInMode + 1
                if reindeer.timeInMode == reindeer.restTime then
                    reindeer.flying = true
                    reindeer.timeInMode = 0
                end
            end
        end
    end
end

function findMaxDistance(reindeers)
    local maxDistance = 0
    for _, reindeer in ipairs(reindeers) do
        if reindeer.distance > maxDistance then
            maxDistance = reindeer.distance
        end
    end
    return maxDistance
end

local reindeers = readReindeerDetails("input.txt")
simulateRace(reindeers, 2503)
local maxDistance = findMaxDistance(reindeers)
print(maxDistance)
