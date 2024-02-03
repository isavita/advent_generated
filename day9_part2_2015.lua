
local distances = {}
for line in io.lines("input.txt") do
    local parts = {}
    for part in string.gmatch(line, "%S+") do
        table.insert(parts, part)
    end
    if #parts ~= 5 then
        goto continue
    end

    local from, to, dist = parts[1], parts[3], tonumber(parts[5])
    distances[from] = distances[from] or {}
    distances[from][to] = dist

    distances[to] = distances[to] or {}
    distances[to][from] = dist

    ::continue::
end

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
    for i = 1, #route - 1 do
        sum = sum + distances[route[i]][route[i + 1]]
    end
    return sum
end

local function permute(arr, i, bestDistance, distances, findShortest)
    if i > #arr then
        return
    end
    if i == #arr then
        local dist = calculateRouteDistance(arr, distances)
        if findShortest then
            if bestDistance[1] == 0 or dist < bestDistance[1] then
                bestDistance[1] = dist
            end
        else
            if dist > bestDistance[1] then
                bestDistance[1] = dist
            end
        end
        return
    end
    for j = i, #arr do
        arr[i], arr[j] = arr[j], arr[i]
        permute(arr, i + 1, bestDistance, distances, findShortest)
        arr[i], arr[j] = arr[j], arr[i]
    end
end

local locations = getUniqueLocations(distances)
local maxDistance = {0}
permute(locations, 1, maxDistance, distances, false)
print(maxDistance[1])
