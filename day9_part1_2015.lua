
local distances = {}
local file = io.open("input.txt", "r")
for line in file:lines() do
    local parts = {}
    for part in line:gmatch("%S+") do
        table.insert(parts, part)
    end
    local from, to, dist = parts[1], parts[3], tonumber(parts[5])
    
    if not distances[from] then
        distances[from] = {}
    end
    distances[from][to] = dist
    
    if not distances[to] then
        distances[to] = {}
    end
    distances[to][from] = dist
end
file:close()

local function getUniqueLocations(distances)
    local locationSet = {}
    for from, _ in pairs(distances) do
        locationSet[from] = true
        for to, _ in pairs(distances[from]) do
            locationSet[to] = true
        end
    end
    
    local locations = {}
    for location, _ in pairs(locationSet) do
        table.insert(locations, location)
    end
    
    return locations
end

local function calculateRouteDistance(route, distances)
    local sum = 0
    for i = 1, #route-1 do
        sum = sum + distances[route[i]][route[i+1]]
    end
    return sum
end

local function permute(arr, i, minDistance, distances)
    if i > #arr then
        return
    end
    if i == #arr then
        local dist = calculateRouteDistance(arr, distances)
        if minDistance[1] == -1 or dist < minDistance[1] then
            minDistance[1] = dist
        end
        return
    end
    for j = i, #arr do
        arr[i], arr[j] = arr[j], arr[i]
        permute(arr, i+1, minDistance, distances)
        arr[i], arr[j] = arr[j], arr[i]
    end
end

local locations = getUniqueLocations(distances)
local minDistance = {-1}
permute(locations, 1, minDistance, distances)
print(minDistance[1])
