local function parseInput(input)
    local grid = {
        Width = #input[1],
        Height = #input,
        Data = {}
    }

    for y, line in ipairs(input) do
        for x = 1, #line do
            local char = line:sub(x, x)
            if char ~= '.' then
                grid.Data[(x - 1) .. "," .. (y - 1)] = char
            end
        end
    end

    return grid
end

local function isInBounds(grid, x, y)
    return x >= 0 and x < grid.Width and y >= 0 and y < grid.Height
end

local function neighbors4(grid, x, y)
    local steps = {{0, -1}, {0, 1}, {1, 0}, {-1, 0}}
    local neighbors = {}

    for _, step in ipairs(steps) do
        local nx, ny = x + step[1], y + step[2]
        if isInBounds(grid, nx, ny) and grid.Data[nx .. "," .. ny] ~= '#' then
            table.insert(neighbors, {nx, ny})
        end
    end

    return neighbors
end

local function breadthFirstSearch(grid, startX, startY)
    local frontier = {{startX, startY}}
    local reached = {[startX .. "," .. startY] = true}
    local distances = {[startX .. "," .. startY] = 0}

    while #frontier > 0 do
        local current = table.remove(frontier, 1)
        local x, y = current[1], current[2]

        for _, next in ipairs(neighbors4(grid, x, y)) do
            local key = next[1] .. "," .. next[2]
            if not reached[key] then
                table.insert(frontier, next)
                reached[key] = true
                distances[key] = distances[x .. "," .. y] + 1
            end
        end
    end

    return distances
end

local function solve(input, numSteps)
    local grid = parseInput(input)
    local start

    for key, value in pairs(grid.Data) do
        if value == 'S' then
            local coords = {}
            for coord in key:gmatch("[^,]+") do
                table.insert(coords, tonumber(coord))
            end
            start = coords
            break
        end
    end

    local distances = breadthFirstSearch(grid, start[1], start[2])
    local cnt = 0

    for _, dist in pairs(distances) do
        if dist <= numSteps and dist % 2 == 0 then
            cnt = cnt + 1
        end
    end

    return cnt
end

local function readFile(fileName)
    local file = io.open(fileName, "r")
    local input = {}
    for line in file:lines() do
        table.insert(input, line)
    end
    file:close()
    return input
end

local input = readFile("input.txt")
print(solve(input, 64))