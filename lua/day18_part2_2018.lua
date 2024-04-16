local SIZE = 50
local OPEN = '.'
local TREES = '|'
local LUMBERYARD = '#'

local function readInput(filename)
    local grid = {}
    for line in io.lines(filename) do
        table.insert(grid, line)
    end
    return grid
end

local function countAdjacent(grid, i, j, acreType)
    local count = 0
    for x = -1, 1 do
        for y = -1, 1 do
            if x ~= 0 or y ~= 0 then
                local ni, nj = i + x, j + y
                if ni >= 1 and ni <= #grid and nj >= 1 and nj <= #grid[1] and grid[ni]:sub(nj, nj) == acreType then
                    count = count + 1
                end
            end
        end
    end
    return count
end

local function nextAcreState(grid, i, j)
    local current = grid[i]:sub(j, j)
    if current == OPEN and countAdjacent(grid, i, j, TREES) >= 3 then
        return TREES
    elseif current == TREES and countAdjacent(grid, i, j, LUMBERYARD) >= 3 then
        return LUMBERYARD
    elseif current == LUMBERYARD then
        if countAdjacent(grid, i, j, LUMBERYARD) >= 1 and countAdjacent(grid, i, j, TREES) >= 1 then
            return LUMBERYARD
        else
            return OPEN
        end
    end
    return current
end

local function transform(grid)
    local newGrid = {}
    for i = 1, #grid do
        newGrid[i] = {}
        for j = 1, #grid[1] do
            newGrid[i][j] = nextAcreState(grid, i, j)
        end
        newGrid[i] = table.concat(newGrid[i])
    end
    return newGrid
end

local function countResources(grid)
    local wooded, lumberyards = 0, 0
    for i = 1, #grid do
        for j = 1, #grid[1] do
            local acre = grid[i]:sub(j, j)
            if acre == TREES then
                wooded = wooded + 1
            elseif acre == LUMBERYARD then
                lumberyards = lumberyards + 1
            end
        end
    end
    return wooded, lumberyards
end

local function gridToString(grid)
    return table.concat(grid, "\n")
end

local function main()
    local grid = readInput("input.txt")
    local seenStates = {}
    local cycleStart, cycleLength
    local minute = 0

    while true do
        local state = gridToString(grid)
        if seenStates[state] then
            cycleStart = seenStates[state]
            cycleLength = minute - cycleStart
            break
        end
        seenStates[state] = minute
        grid = transform(grid)
        minute = minute + 1
    end

    local remainingMinutes = (1000000000 - cycleStart) % cycleLength
    for i = 1, remainingMinutes do
        grid = transform(grid)
    end

    local wooded, lumberyards = countResources(grid)
    print(wooded * lumberyards)
end

main()