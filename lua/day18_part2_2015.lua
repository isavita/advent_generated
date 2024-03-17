-- Constants
local GRID_SIZE = 100
local STEPS = 100

-- Helper functions
local function count_on_neighbors(grid, x, y)
    local on = 0
    for dx = -1, 1 do
        for dy = -1, 1 do
            if dx == 0 and dy == 0 then
                goto continue
            end
            local nx, ny = x + dx, y + dy
            if nx >= 0 and nx < GRID_SIZE and ny >= 0 and ny < GRID_SIZE and grid[nx + 1][ny + 1] then
                on = on + 1
            end
            ::continue::
        end
    end
    return on
end

local function step(grid)
    local new_grid = {}
    for i = 1, GRID_SIZE do
        new_grid[i] = {}
        for j = 1, GRID_SIZE do
            new_grid[i][j] = false
        end
    end

    for x = 1, GRID_SIZE do
        for y = 1, GRID_SIZE do
            local on_neighbors = count_on_neighbors(grid, x - 1, y - 1)
            if grid[x][y] then
                new_grid[x][y] = on_neighbors == 2 or on_neighbors == 3
            else
                new_grid[x][y] = on_neighbors == 3
            end
        end
    end

    -- Ensure corners are always on
    new_grid[1][1] = true
    new_grid[1][GRID_SIZE] = true
    new_grid[GRID_SIZE][1] = true
    new_grid[GRID_SIZE][GRID_SIZE] = true

    return new_grid
end

-- Main function
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening file")
        return
    end

    local grid = {}
    for i = 1, GRID_SIZE do
        grid[i] = {}
        for j = 1, GRID_SIZE do
            grid[i][j] = false
        end
    end

    local y = 1
    for line in file:lines() do
        for x = 1, #line do
            grid[x][y] = line:sub(x, x) == "#"
        end
        y = y + 1
    end

    -- Initialize corners as always on
    grid[1][1] = true
    grid[1][GRID_SIZE] = true
    grid[GRID_SIZE][1] = true
    grid[GRID_SIZE][GRID_SIZE] = true

    for i = 1, STEPS do
        grid = step(grid)
    end

    local on_count = 0
    for _, row in ipairs(grid) do
        for _, light in ipairs(row) do
            if light then
                on_count = on_count + 1
            end
        end
    end

    print(on_count)

    file:close()
end

main()