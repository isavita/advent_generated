function buildGrid(input, empty)
    local grid = {
        Width = #input[1],
        Height = #input,
        Data = {}
    }

    for y, line in ipairs(input) do
        for x = 1, #line do
            local char = line:sub(x, x)
            if char ~= empty then
                grid.Data[(x - 1) .. "," .. (y - 1)] = char
            end
        end
    end

    return grid
end

function getEmptyRows(grid)
    local emptyRows = {}
    for y = 0, grid.Height - 1 do
        local isEmpty = true
        local x = 0
        while x < grid.Width do
            if grid.Data[x .. "," .. y] then
                isEmpty = false
                break
            end
            x = x + 1
        end
        if isEmpty then
            table.insert(emptyRows, y)
        end
    end
    return emptyRows
end

function getEmptyCols(grid)
    local emptyCols = {}
    for x = 0, grid.Width - 1 do
        local isEmpty = true
        local y = 0
        while y < grid.Height do
            if grid.Data[x .. "," .. y] then
                isEmpty = false
                break
            end
            y = y + 1
        end
        if isEmpty then
            table.insert(emptyCols, x)
        end
    end
    return emptyCols
end

function calculateOffsets(emptyIndexes, bound)
    local offsets = {}
    for i = 0, bound - 1 do
        offsets[i] = 0
    end
    for _, idx in ipairs(emptyIndexes) do
        for i = idx + 1, bound - 1 do
            offsets[i] = offsets[i] + 1
        end
    end
    return offsets
end

function expandGrid(grid, expansionFactor)
    local emptyCols = getEmptyCols(grid)
    local emptyRows = getEmptyRows(grid)
    local numLinesToAdd = expansionFactor - 1

    local newGrid = {
        Width = grid.Width + #emptyCols * numLinesToAdd,
        Height = grid.Height + #emptyRows * numLinesToAdd,
        Data = {}
    }

    local dXs = calculateOffsets(emptyCols, grid.Width)
    local dYs = calculateOffsets(emptyRows, grid.Height)

    for y = 0, grid.Height - 1 do
        for x = 0, grid.Width - 1 do
            local coord = x .. "," .. y
            if grid.Data[coord] then
                local newCoord = (x + dXs[x] * numLinesToAdd) .. "," .. (y + dYs[y] * numLinesToAdd)
                newGrid.Data[newCoord] = grid.Data[coord]
            end
        end
    end

    return newGrid
end

function abs(x)
    return (x < 0) and -x or x
end

function calculateLength(grid, c1, c2)
    local x1, y1 = c1:match("^(%d+),(%d+)$")
    local x2, y2 = c2:match("^(%d+),(%d+)$")
    local dX = abs(tonumber(x2) - tonumber(x1))
    local dY = abs(tonumber(y2) - tonumber(y1))
    return dX + dY
end

function solve(input)
    local grid = buildGrid(input, '.')

    local expandedGrid = expandGrid(grid, 2)

    local res = 0
    local alreadySeen = {}
    for coord1, _ in pairs(expandedGrid.Data) do
        for coord2, _ in pairs(alreadySeen) do
            local length = calculateLength(expandedGrid, coord1, coord2)
            res = res + length
        end
        alreadySeen[coord1] = true
    end

    return res
end

function readFile(fileName)
    local file = io.open(fileName, "r")
    local data = file:read("*all")
    file:close()
    local lines = {}
    for line in data:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    return lines
end

local input = readFile("input.txt")
print(solve(input))