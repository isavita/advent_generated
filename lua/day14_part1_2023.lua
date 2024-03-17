function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*a")
    file:close()
    return content
end

function buildGrid(input)
    local grid = {}
    grid.data = {}
    grid.width = #input[1]
    grid.height = #input

    for y = 1, grid.height do
        for x = 1, grid.width do
            local char = string.sub(input[y], x, x)
            if char ~= '.' then
                grid.data[x .. "," .. y] = char
            end
        end
    end

    return grid
end

function isInBounds(coord, grid)
    return coord.x >= 1 and coord.x <= grid.width and coord.y >= 1 and coord.y <= grid.height
end

function shiftSingleRock(grid, coord, dir)
    if grid.data[coord.x .. "," .. coord.y] == 'O' then
        local current = {x = coord.x, y = coord.y}
        local before = {x = current.x + dir.x, y = current.y + dir.y}

        while grid.data[before.x .. "," .. before.y] == nil and isInBounds(before, grid) do
            grid.data[before.x .. "," .. before.y] = 'O'
            grid.data[current.x .. "," .. current.y] = nil

            current.x, current.y = before.x, before.y
            before.x, before.y = current.x + dir.x, current.y + dir.y
        end
    end
end

function shiftRocks(grid, dir)
    if dir.x == 0 then
        if dir.y == -1 then
            for x = 1, grid.width do
                for y = 1, grid.height do
                    shiftSingleRock(grid, {x = x, y = y}, dir)
                end
            end
        elseif dir.y == 1 then
            for x = 1, grid.width do
                for y = grid.height, 1, -1 do
                    shiftSingleRock(grid, {x = x, y = y}, dir)
                end
            end
        end
    elseif dir.y == 0 then
        if dir.x == -1 then
            for x = 1, grid.width do
                for y = 1, grid.height do
                    shiftSingleRock(grid, {x = x, y = y}, dir)
                end
            end
        elseif dir.x == 1 then
            for x = grid.width, 1, -1 do
                for y = grid.height, 1, -1 do
                    shiftSingleRock(grid, {x = x, y = y}, dir)
                end
            end
        end
    end
end

function calculateLoad(grid)
    local load = 0
    for x = 1, grid.width do
        for y = 1, grid.height do
            if grid.data[x .. "," .. y] == 'O' then
                load = load + (grid.height - y + 1)
            end
        end
    end
    return load
end

function solve(input)
    local grid = buildGrid(input)
    shiftRocks(grid, {x = 0, y = -1})
    return calculateLoad(grid)
end

local input = {}
for line in io.lines("input.txt") do
    table.insert(input, line)
end

print(solve(input))