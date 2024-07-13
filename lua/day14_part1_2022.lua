local function readInput(filename)
    local file = io.open(filename, "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function parsePath(path)
    local points = {}
    for point in path:gmatch("%d+,%d+") do
        local x, y = point:match("(%d+),(%d+)")
        table.insert(points, {tonumber(x), tonumber(y)})
    end
    return points
end

local function createGrid(paths)
    local grid = {}
    local maxX, maxY = 0, 0

    for _, path in ipairs(paths) do
        for i = 1, #path - 1 do
            local x1, y1 = table.unpack(path[i])
            local x2, y2 = table.unpack(path[i + 1])
            
            if x1 == x2 then
                for y = math.min(y1, y2), math.max(y1, y2) do
                    grid[y] = grid[y] or {}
                    grid[y][x1] = '#'
                end
            elseif y1 == y2 then
                for x = math.min(x1, x2), math.max(x1, x2) do
                    grid[y1] = grid[y1] or {}
                    grid[y1][x] = '#'
                end
            end
            
            maxX = math.max(maxX, x1, x2)
            maxY = math.max(maxY, y1, y2)
        end
    end

    return grid, maxX, maxY
end

local function printGrid(grid, maxX, maxY)
    for y = 0, maxY do
        for x = 0, maxX do
            io.write(grid[y] and grid[y][x] or '.')
        end
        io.write('\n')
    end
end

local function simulateSand(grid, maxX, maxY)
    local sourceX, sourceY = 500, 0
    local sandCount = 0
    
    while true do
        local x, y = sourceX, sourceY
        local settled = false
        
        while true do
            if y > maxY then
                return sandCount
            end
            
            if not grid[y + 1] or not grid[y + 1][x] then
                y = y + 1
            elseif not grid[y + 1][x - 1] then
                y = y + 1
                x = x - 1
            elseif not grid[y + 1][x + 1] then
                y = y + 1
                x = x + 1
            else
                grid[y] = grid[y] or {}
                grid[y][x] = 'o'
                sandCount = sandCount + 1
                settled = true
                break
            end
        end
        
        if not settled then
            break
        end
    end
end

local function main()
    local lines = readInput("input.txt")
    local paths = {}
    
    for _, line in ipairs(lines) do
        table.insert(paths, parsePath(line))
    end
    
    local grid, maxX, maxY = createGrid(paths)
    grid[0] = grid[0] or {}
    grid[0][500] = '+'
    
    local sandCount = simulateSand(grid, maxX, maxY)
    print(sandCount)
end

main()
