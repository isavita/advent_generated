function read_input(filename)
    local file = io.open(filename, "r")
    local grid = {}
    for line in file:lines() do
        local row = {}
        for digit in line:gmatch(".") do
            table.insert(row, tonumber(digit))
        end
        table.insert(grid, row)
    end
    file:close()
    return grid
end

function simulate_step(grid)
    local flashed = {}
    local flashes = 0

    for y = 1, #grid do
        for x = 1, #grid[y] do
            grid[y][x] = grid[y][x] + 1
        end
    end

    local function flash(x, y)
        if flashed[y] and flashed[y][x] then return end
        if not flashed[y] then flashed[y] = {} end
        flashed[y][x] = true
        flashes = flashes + 1
        local directions = {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}}
        for _, dir in ipairs(directions) do
            local newX, newY = x + dir[1], y + dir[2]
            if newY >= 1 and newY <= #grid and newX >= 1 and newX <= #grid[newY] then
                grid[newY][newX] = grid[newY][newX] + 1
                if grid[newY][newX] > 9 then
                    flash(newX, newY)
                end
            end
        end
    end

    for y = 1, #grid do
        for x = 1, #grid[y] do
            if grid[y][x] > 9 then
                flash(x, y)
            end
        end
    end

    for y, rows in pairs(flashed) do
        for x in pairs(rows) do
            grid[y][x] = 0
        end
    end

    return flashes
end

local grid = read_input("input.txt")
local total_flashes = 0

for step = 1, 100 do
    total_flashes = total_flashes + simulate_step(grid)
end

print(total_flashes)