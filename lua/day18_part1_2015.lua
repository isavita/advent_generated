local gridSize = 100
local steps = 100

local function countOnNeighbors(grid, x, y)
    local on = 0
    for dx = -1, 1 do
        for dy = -1, 1 do
            if dx == 0 and dy == 0 then
                goto continue
            end
            local nx, ny = x + dx, y + dy
            if nx >= 0 and nx < gridSize and ny >= 0 and ny < gridSize and grid[nx + 1][ny + 1] then
                on = on + 1
            end
            ::continue::
        end
    end
    return on
end

local function step(grid)
    local newGrid = {}
    for i = 1, gridSize do
        newGrid[i] = {}
        for j = 1, gridSize do
            newGrid[i][j] = false
        end
    end

    for x = 1, gridSize do
        for y = 1, gridSize do
            local onNeighbors = countOnNeighbors(grid, x - 1, y - 1)
            if grid[x][y] then
                newGrid[x][y] = onNeighbors == 2 or onNeighbors == 3
            else
                newGrid[x][y] = onNeighbors == 3
            end
        end
    end

    return newGrid
end

local grid = {}
local file = io.open("input.txt", "r")
local y = 1
for line in file:lines() do
    grid[y] = {}
    for x = 1, #line do
        grid[y][x] = line:sub(x, x) == "#"
    end
    y = y + 1
end
file:close()

for i = 1, steps do
    grid = step(grid)
end

local onCount = 0
for _, row in ipairs(grid) do
    for _, light in ipairs(row) do
        if light then
            onCount = onCount + 1
        end
    end
end

print(onCount)