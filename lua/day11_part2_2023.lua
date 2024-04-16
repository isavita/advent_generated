function readFile(fileName)
    local file = io.open(fileName, "r")
    if not file then error("File not found") end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

function buildGrid(input, empty)
    local grid = {
        Width = #input[1],
        Height = #input,
        Data = {}
    }
    
    for y = 1, #input do
        local line = input[y]
        for x = 1, #line do
            local char = line:sub(x, x)
            if char ~= empty then
                grid.Data[(x - 1) .. "," .. (y - 1)] = char
            end
        end
    end
    
    return grid
end

function getEmptyIndices(size, isColumnEmpty)
    local emptyIndices = {}
    for i = 0, size - 1 do
        if isColumnEmpty(i) then
            table.insert(emptyIndices, i)
        end
    end
    return emptyIndices
end

function calculateOffsets(emptyIndices, bound)
    local offsets = {}
    for i = 0, bound - 1 do
        offsets[i] = 0
    end
    for _, idx in ipairs(emptyIndices) do
        for i = idx + 1, bound - 1 do
            offsets[i] = offsets[i] + 1
        end
    end
    return offsets
end

function expandGrid(grid, expansionFactor)
    local function isColumnEmpty(x)
        for y = 0, grid.Height - 1 do
            if grid.Data[x .. "," .. y] then return false end
        end
        return true
    end
    
    local function isRowEmpty(y)
        for x = 0, grid.Width - 1 do
            if grid.Data[x .. "," .. y] then return false end
        end
        return true
    end
    
    local emptyCols = getEmptyIndices(grid.Width, isColumnEmpty)
    local emptyRows = getEmptyIndices(grid.Height, isRowEmpty)
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
                local newX = x + (dXs[x] or 0) * numLinesToAdd
                local newY = y + (dYs[y] or 0) * numLinesToAdd
                newGrid.Data[newX .. "," .. newY] = grid.Data[coord]
            end
        end
    end
    
    return newGrid
end

function abs(x)
    return (x < 0) and -x or x
end

function calculateLength(c1, c2)
    local x1, y1 = c1:match("^(%d+),(%d+)$")
    local x2, y2 = c2:match("^(%d+),(%d+)$")
    x1, y1, x2, y2 = tonumber(x1), tonumber(y1), tonumber(x2), tonumber(y2)
    local dX = abs(x2 - x1)
    local dY = abs(y2 - y1)
    return dX + dY
end

function solve(input, expansionFactor)
    local grid = buildGrid(input, '.')
    local expandedGrid = expandGrid(grid, expansionFactor)
    
    local res = 0
    local alreadySeen = {}
    for coord1, _ in pairs(expandedGrid.Data) do
        for coord2, _ in pairs(alreadySeen) do
            local length = calculateLength(coord1, coord2)
            res = res + length
        end
        alreadySeen[coord1] = true
    end
    
    return res
end

local input = readFile("input.txt")
print(solve(input, 1000000))