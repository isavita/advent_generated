local directions = {
    {0, -1}, -- up
    {1, 0},  -- right
    {0, 1},  -- down
    {-1, 0}  -- left
}

local function turnRight(direction)
    return (direction % 4) + 1
end

local function turnLeft(direction)
    return (direction + 2) % 4 + 1
end

local function readInput(filename)
    local grid = {}
    local y = 1
    for line in io.lines(filename) do
        grid[y] = {}
        for x = 1, #line do
            grid[y][x] = line:sub(x, x)
        end
        y = y + 1
    end
    return grid
end

local function simulateBursts(grid, bursts)
    local infections = 0
    local cx, cy = math.ceil(#grid[1] / 2), math.ceil(#grid / 2)
    local dir = 1  -- facing up initially

    for _ = 1, bursts do
        if grid[cy][cx] == '#' then
            dir = turnRight(dir)
            grid[cy][cx] = '.'
        else
            dir = turnLeft(dir)
            grid[cy][cx] = '#'
            infections = infections + 1
        end

        cx = cx + directions[dir][1]
        cy = cy + directions[dir][2]

        -- Ensure the grid can expand if necessary
        if not grid[cy] then
            grid[cy] = {}
        end
        if not grid[cy][cx] then
            grid[cy][cx] = '.'
        end
    end

    return infections
end

local function main()
    local grid = readInput("input.txt")
    local result = simulateBursts(grid, 10000)
    print("Infections caused after 10000 bursts:", result)
end

main()