local function readInput(filename)
    local lines = {}
    for line in io.lines(filename) do
        lines[#lines + 1] = line
    end
    return lines
end

local function tiltNorth(grid)
    for col = 1, #grid[1] do
        local nextFree = 1
        for row = 1, #grid do
            if grid[row]:sub(col, col) == 'O' then
                if row ~= nextFree then
                    grid[nextFree] = grid[nextFree]:sub(1, col-1) .. 'O' .. grid[nextFree]:sub(col+1)
                    grid[row] = grid[row]:sub(1, col-1) .. '.' .. grid[row]:sub(col+1)
                end
                nextFree = nextFree + 1
            elseif grid[row]:sub(col, col) == '#' then
                nextFree = row + 1
            end
        end
    end
end

local function tiltWest(grid)
    for row = 1, #grid do
        local nextFree = 1
        for col = 1, #grid[1] do
            local char = grid[row]:sub(col, col)
            if char == 'O' then
                if col ~= nextFree then
                    grid[row] = grid[row]:sub(1, nextFree-1) .. 'O' .. grid[row]:sub(nextFree+1, col-1) .. '.' .. grid[row]:sub(col+1)
                end
                nextFree = nextFree + 1
            elseif char == '#' then
                nextFree = col + 1
            end
        end
    end
end

local function tiltSouth(grid)
    for col = 1, #grid[1] do
        local nextFree = #grid
        for row = #grid, 1, -1 do
            if grid[row]:sub(col, col) == 'O' then
                if row ~= nextFree then
                    grid[nextFree] = grid[nextFree]:sub(1, col-1) .. 'O' .. grid[nextFree]:sub(col+1)
                    grid[row] = grid[row]:sub(1, col-1) .. '.' .. grid[row]:sub(col+1)
                end
                nextFree = nextFree - 1
            elseif grid[row]:sub(col, col) == '#' then
                nextFree = row - 1
            end
        end
    end
end

local function tiltEast(grid)
    for row = 1, #grid do
        local nextFree = #grid[1]
        for col = #grid[1], 1, -1 do
            local char = grid[row]:sub(col, col)
            if char == 'O' then
                if col ~= nextFree then
                    grid[row] = grid[row]:sub(1, col-1) .. '.' .. grid[row]:sub(col+1, nextFree-1) .. 'O' .. grid[row]:sub(nextFree+1)
                end
                nextFree = nextFree - 1
            elseif char == '#' then
                nextFree = col - 1
            end
        end
    end
end

local function cycle(grid)
    tiltNorth(grid)
    tiltWest(grid)
    tiltSouth(grid)
    tiltEast(grid)
end

local function calculateLoad(grid)
    local load = 0
    for row = 1, #grid do
        local rowLoad = #grid - row + 1
        load = load + rowLoad * select(2, grid[row]:gsub('O', ''))
    end
    return load
end

local function gridToString(grid)
    return table.concat(grid, '\n')
end

local function main()
    local grid = readInput('input.txt')
    local seen = {}
    local loads = {}
    local cycleStart, cycleLength

    for i = 1, 1000000000 do
        cycle(grid)
        local gridStr = gridToString(grid)
        if seen[gridStr] then
            cycleStart = seen[gridStr]
            cycleLength = i - cycleStart
            break
        end
        seen[gridStr] = i
        loads[i] = calculateLoad(grid)
    end

    local remainingCycles = (1000000000 - cycleStart) % cycleLength
    local finalLoadIndex = cycleStart + remainingCycles

    print(loads[finalLoadIndex])
end

main()
