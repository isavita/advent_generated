-- Read initial state from file
local function readInitialState(filename)
    local initialState = {}
    for line in io.lines(filename) do
        table.insert(initialState, line)
    end
    return initialState
end

-- Initialize the grid from the initial state
local function initializeGrid(initialState)
    local grid = {}
    for x, line in ipairs(initialState) do
        for y = 1, #line do
            local state = line:sub(y, y) == '#' and true or false
            grid[x] = grid[x] or {}
            grid[x][y] = grid[x][y] or {}
            grid[x][y][0] = state
        end
    end
    return grid
end

-- Count active neighbors
local function countActiveNeighbors(grid, x, y, z)
    local count = 0
    for dx = -1, 1 do
        for dy = -1, 1 do
            for dz = -1, 1 do
                if not (dx == 0 and dy == 0 and dz == 0) then
                    if grid[x+dx] and grid[x+dx][y+dy] and grid[x+dx][y+dy][z+dz] then
                        if grid[x+dx][y+dy][z+dz] then
                            count = count + 1
                        end
                    end
                end
            end
        end
    end
    return count
end

-- Perform one simulation cycle
local function performCycle(grid)
    local newGrid = {}
    local minX, maxX = math.huge, -math.huge
    local minY, maxY = math.huge, -math.huge
    local minZ, maxZ = math.huge, -math.huge

    -- Determine the range to check
    for x, plane in pairs(grid) do
        minX, maxX = math.min(minX, x), math.max(maxX, x)
        for y, line in pairs(plane) do
            minY, maxY = math.min(minY, y), math.max(maxY, y)
            for z, state in pairs(line) do
                minZ, maxZ = math.min(minZ, z), math.max(maxZ, z)
            end
        end
    end

    -- Expand the range by 1 for potential growth
    for x = minX-1, maxX+1 do
        for y = minY-1, maxY+1 do
            for z = minZ-1, maxZ+1 do
                local activeNeighbors = countActiveNeighbors(grid, x, y, z)
                local currentState = grid[x] and grid[x][y] and grid[x][y][z] or false
                newGrid[x] = newGrid[x] or {}
                newGrid[x][y] = newGrid[x][y] or {}
                if currentState then
                    newGrid[x][y][z] = (activeNeighbors == 2 or activeNeighbors == 3)
                else
                    newGrid[x][y][z] = (activeNeighbors == 3)
                end
            end
        end
    end

    return newGrid
end

-- Count all active cubes
local function countActiveCubes(grid)
    local count = 0
    for _, plane in pairs(grid) do
        for _, line in pairs(plane) do
            for _, state in pairs(line) do
                if state then
                    count = count + 1
                end
            end
        end
    end
    return count
end

-- Main simulation function
local function simulateCubes(filename, cycles)
    local initialState = readInitialState(filename)
    local grid = initializeGrid(initialState)

    for i = 1, cycles do
        grid = performCycle(grid)
    end

    return countActiveCubes(grid)
end

-- Run the simulation for 6 cycles and print the result
print(simulateCubes("input.txt", 6))