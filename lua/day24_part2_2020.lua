local directions = {
    e = {1, 0},
    se = {0, 1},
    sw = {-1, 1},
    w = {-1, 0},
    nw = {0, -1},
    ne = {1, -1}
}

local function getNeighbors(tile)
    local neighbors = {}
    for _, dir in pairs(directions) do
        table.insert(neighbors, {tile[1] + dir[1], tile[2] + dir[2]})
    end
    return neighbors
end

local function coordinateKey(q, r)
    return q .. "," .. r
end

local blackTiles = {}

local file = io.open("input.txt", "r")
for line in file:lines() do
    local i = 1
    local coord = {0, 0}
    while i <= #line do
        local dir
        local c = line:sub(i, i)
        if c == 'e' or c == 'w' then
            dir = c
            i = i + 1
        else
            dir = line:sub(i, i+1)
            i = i + 2
        end
        local move = directions[dir]
        coord[1] = coord[1] + move[1]
        coord[2] = coord[2] + move[2]
    end
    local key = coordinateKey(coord[1], coord[2])
    blackTiles[key] = not blackTiles[key]
end
file:close()

for day = 1, 100 do
    local tilesToCheck = {}
    for key, isBlack in pairs(blackTiles) do
        if isBlack then
            local q, r = key:match("([^,]+),([^,]+)")
            q, r = tonumber(q), tonumber(r)
            tilesToCheck[key] = true
            for _, neighbor in ipairs(getNeighbors({q, r})) do
                local neighborKey = coordinateKey(neighbor[1], neighbor[2])
                tilesToCheck[neighborKey] = true
            end
        end
    end

    local newBlackTiles = {}
    for key, _ in pairs(tilesToCheck) do
        local q, r = key:match("([^,]+),([^,]+)")
        q, r = tonumber(q), tonumber(r)
        local blackNeighborCount = 0
        for _, neighbor in ipairs(getNeighbors({q, r})) do
            local neighborKey = coordinateKey(neighbor[1], neighbor[2])
            if blackTiles[neighborKey] then
                blackNeighborCount = blackNeighborCount + 1
            end
        end
        if (blackTiles[key] and (blackNeighborCount == 1 or blackNeighborCount == 2)) or
           (not blackTiles[key] and blackNeighborCount == 2) then
            newBlackTiles[key] = true
        end
    end

    blackTiles = newBlackTiles
end

local count = 0
for _, isBlack in pairs(blackTiles) do
    if isBlack then count = count + 1 end
end

print(count)