function readInput(filename)
    local grid = {}
    for line in io.lines(filename) do
        local row = {}
        for digit in line:gmatch(".") do
            table.insert(row, tonumber(digit))
        end
        table.insert(grid, row)
    end
    return grid
end

function simulateStep(grid)
    local flashes = 0
    local flashed = {}

    -- Increase energy by 1 for all octopuses
    for y = 1, #grid do
        for x = 1, #grid[y] do
            grid[y][x] = grid[y][x] + 1
        end
    end

    -- Flash octopuses with energy greater than 9
    for y = 1, #grid do
        for x = 1, #grid[y] do
            if grid[y][x] > 9 then
                flashes = flashes + flash(grid, x, y, flashed)
            end
        end
    end

    -- Reset energy to 0 for all that flashed
    for coords, _ in pairs(flashed) do
        local y, x = coords:match("^(%d+),(%d+)$")
        y, x = tonumber(y), tonumber(x)
        grid[y][x] = 0
    end

    return flashes
end

function flash(grid, x, y, flashed)
    local key = y .. "," .. x
    if flashed[key] then
        return 0
    end

    flashed[key] = true
    local flashes = 1
    local directions = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}}

    for _, dir in ipairs(directions) do
        local newX, newY = x + dir[1], y + dir[2]
        if newX >= 1 and newX <= #grid[1] and newY >= 1 and newY <= #grid then
            grid[newY][newX] = grid[newY][newX] + 1
            if grid[newY][newX] > 9 then
                flashes = flashes + flash(grid, newX, newY, flashed)
            end
        end
    end

    return flashes
end

function main()
    local grid = readInput("input.txt")
    local step = 0
    while true do
        step = step + 1
        local flashes = simulateStep(grid)
        if flashes == 100 then -- All octopuses have flashed
            break
        end
    end

    print(step)
end

main()