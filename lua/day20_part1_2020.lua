-- Helper function to read file content
local function readFile(filename)
    local tiles = {}
    local currentTileId = nil
    local currentTileData = {}

    for line in io.lines(filename) do
        if line:match("^Tile (%d+):$") then
            if currentTileId then
                tiles[currentTileId] = currentTileData
            end
            currentTileId = tonumber(line:match("^Tile (%d+):$"))
            currentTileData = {}
        elseif line ~= "" then
            table.insert(currentTileData, line)
        end
    end
    if currentTileId then
        tiles[currentTileId] = currentTileData
    end
    return tiles
end

-- Extract edges from a tile
local function getEdges(tile)
    local top = tile[1]
    local bottom = tile[#tile]
    local left, right = "", ""
    for i = 1, #tile do
        left = left .. tile[i]:sub(1, 1)
        right = right .. tile[i]:sub(-1, -1)
    end
    return {top, right, bottom, left}
end

-- Find matching tiles based on edges
local function findMatches(tiles)
    local edgeMap = {}
    for id, tile in pairs(tiles) do
        local edges = getEdges(tile)
        for _, edge in ipairs(edges) do
            edgeMap[edge] = edgeMap[edge] or {}
            table.insert(edgeMap[edge], id)
            local reversed = edge:reverse()
            edgeMap[reversed] = edgeMap[reversed] or {}
            table.insert(edgeMap[reversed], id)
        end
    end
    return edgeMap
end

-- Determine corner tiles
local function findCorners(tiles, edgeMap)
    local cornerCandidates = {}
    for id, tile in pairs(tiles) do
        local edges = getEdges(tile)
        local uniqueCount = 0
        for _, edge in ipairs(edges) do
            if #edgeMap[edge] == 1 or #edgeMap[edge:reverse()] == 1 then
                uniqueCount = uniqueCount + 1
            end
        end
        if uniqueCount == 2 then
            table.insert(cornerCandidates, id)
        end
    end
    return cornerCandidates
end

-- Main execution function
local function main()
    local tiles = readFile("input.txt")
    local edgeMap = findMatches(tiles)
    local corners = findCorners(tiles, edgeMap)
    local product = 1
    for _, corner in ipairs(corners) do
        product = product * corner
    end
    print("Product of corner tile IDs:", product)
end

main()