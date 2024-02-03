
local file = io.open("input.txt", "r")
local heightmap = {}
for line in file:lines() do
    local row = {}
    for i = 1, #line do
        local height = tonumber(string.sub(line, i, i))
        table.insert(row, height)
    end
    table.insert(heightmap, row)
end
file:close()

local basinSizes = {}
local visited = {}

local function isLowPoint(heightmap, x, y)
    local height = heightmap[y][x]
    if x > 1 and heightmap[y][x-1] <= height then
        return false
    end
    if x < #heightmap[y] and heightmap[y][x+1] <= height then
        return false
    end
    if y > 1 and heightmap[y-1][x] <= height then
        return false
    end
    if y < #heightmap and heightmap[y+1][x] <= height then
        return false
    end
    return true
end

local function exploreBasin(heightmap, x, y, visited)
    if visited[x.."-"..y] or heightmap[y][x] == 9 then
        return 0
    end
    visited[x.."-"..y] = true
    local size = 1

    local directions = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}}
    for _, dir in ipairs(directions) do
        local newX, newY = x+dir[1], y+dir[2]
        if newX >= 1 and newX <= #heightmap[1] and newY >= 1 and newY <= #heightmap then
            size = size + exploreBasin(heightmap, newX, newY, visited)
        end
    end
    return size
end

for y, row in ipairs(heightmap) do
    for x, _ in ipairs(row) do
        if isLowPoint(heightmap, x, y) then
            local size = exploreBasin(heightmap, x, y, visited)
            table.insert(basinSizes, size)
        end
    end
end

table.sort(basinSizes, function(a, b) return a > b end)
local result = basinSizes[1] * basinSizes[2] * basinSizes[3]
print(result)
