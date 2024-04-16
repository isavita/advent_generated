function read_input(filename)
    local grid = {}
    for line in io.lines(filename) do
        table.insert(grid, line)
    end
    return grid
end

function simulate(grid)
    local step = 0
    local moved = true
    while moved do
        local east_moved = move_east(grid)
        local south_moved = move_south(grid)
        moved = east_moved or south_moved
        step = step + 1
    end
    return step
end

function move_east(grid)
    local moved = false
    local height = #grid
    local width = #grid[1]
    local old_positions = {}

    for y = 1, height do
        old_positions[y] = {}
        for x = 1, width do
            old_positions[y][x] = grid[y]:sub(x, x)
        end
    end

    for y = 1, height do
        for x = 1, width do
            if old_positions[y][x] == '>' then
                local next_x = x % width + 1
                if old_positions[y][next_x] == '.' then
                    grid[y] = grid[y]:sub(1, x-1) .. '.' .. grid[y]:sub(x+1)
                    grid[y] = grid[y]:sub(1, next_x-1) .. '>' .. grid[y]:sub(next_x+1)
                    moved = true
                    x = x + 1
                end
            end
        end
    end
    return moved
end

function move_south(grid)
    local moved = false
    local height = #grid
    local width = #grid[1]
    local old_positions = {}

    for y = 1, height do
        old_positions[y] = {}
        for x = 1, width do
            old_positions[y][x] = grid[y]:sub(x, x)
        end
    end

    for x = 1, width do
        for y = 1, height do
            if old_positions[y][x] == 'v' then
                local next_y = y % height + 1
                if old_positions[next_y][x] == '.' then
                    grid[y] = grid[y]:sub(1, x-1) .. '.' .. grid[y]:sub(x+1)
                    grid[next_y] = grid[next_y]:sub(1, x-1) .. 'v' .. grid[next_y]:sub(x+1)
                    moved = true
                    y = y + 1
                end
            end
        end
    end
    return moved
end

local grid = read_input("input.txt")
local result = simulate(grid)
print(result)