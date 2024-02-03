
local Open = '.'
local Trees = '|'
local Lumberyard = '#'
local Size = 50

function readInput(filename)
    local file = io.open(filename, "r")
    local grid = {}
    for line in file:lines() do
        local row = {}
        for i = 1, #line do
            table.insert(row, line:sub(i, i))
        end
        table.insert(grid, row)
    end
    file:close()
    return grid
end

function transform(grid)
    local newGrid = {}
    for i = 1, #grid do
        newGrid[i] = {}
        for j = 1, #grid[i] do
            newGrid[i][j] = nextAcreState(grid, i, j)
        end
    end
    return newGrid
end

function nextAcreState(grid, i, j)
    local acreType = grid[i][j]
    if acreType == Open then
        if countAdjacent(grid, i, j, Trees) >= 3 then
            return Trees
        end
    elseif acreType == Trees then
        if countAdjacent(grid, i, j, Lumberyard) >= 3 then
            return Lumberyard
        end
    elseif acreType == Lumberyard then
        if countAdjacent(grid, i, j, Lumberyard) >= 1 and countAdjacent(grid, i, j, Trees) >= 1 then
            return Lumberyard
        end
        return Open
    end
    return acreType
end

function countAdjacent(grid, i, j, acreType)
    local count = 0
    for x = -1, 1 do
        for y = -1, 1 do
            if x == 0 and y == 0 then
                goto continue
            end
            if i+x >= 1 and i+x <= #grid and j+y >= 1 and j+y <= #grid[i] and grid[i+x][j+y] == acreType then
                count = count + 1
            end
            ::continue::
        end
    end
    return count
end

function countResources(grid)
    local wooded, lumberyards = 0, 0
    for i = 1, #grid do
        for j = 1, #grid[i] do
            if grid[i][j] == Trees then
                wooded = wooded + 1
            elseif grid[i][j] == Lumberyard then
                lumberyards = lumberyards + 1
            end
        end
    end
    return wooded, lumberyards
end

local grid = readInput("input.txt")
for minute = 1, 10 do
    grid = transform(grid)
end

local wooded, lumberyards = countResources(grid)
print(wooded * lumberyards)
