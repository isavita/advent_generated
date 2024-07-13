local function read_input(filename)
    local file = io.open(filename, "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function parse_paths(lines)
    local paths = {}
    for _, line in ipairs(lines) do
        local path = {}
        for point in string.gmatch(line, "(%d+,%d+)") do
            local x, y = string.match(point, "(%d+),(%d+)")
            table.insert(path, {tonumber(x), tonumber(y)})
        end
        table.insert(paths, path)
    end
    return paths
end

local function create_grid(paths)
    local max_y = 0
    for _, path in ipairs(paths) do
        for _, point in ipairs(path) do
            if point[2] > max_y then
                max_y = point[2]
            end
        end
    end
    local floor_y = max_y + 2
    local grid = {}
    for y = 0, floor_y do
        grid[y] = {}
        for x = 500 - floor_y - 1, 500 + floor_y + 1 do
            grid[y][x] = '.'
        end
    end
    for _, path in ipairs(paths) do
        for i = 1, #path - 1 do
            local x1, y1 = path[i][1], path[i][2]
            local x2, y2 = path[i + 1][1], path[i + 1][2]
            if x1 == x2 then
                for y = math.min(y1, y2), math.max(y1, y2) do
                    grid[y][x1] = '#'
                end
            elseif y1 == y2 then
                for x = math.min(x1, x2), math.max(x1, x2) do
                    grid[y1][x] = '#'
                end
            end
        end
    end
    for x = 500 - floor_y - 1, 500 + floor_y + 1 do
        grid[floor_y][x] = '#'
    end
    return grid, floor_y
end

local function simulate_sand(grid, floor_y)
    local sand_count = 0
    local x, y = 500, 0
    while true do
        if grid[y + 1] and grid[y + 1][x] == '.' then
            y = y + 1
        elseif grid[y + 1] and grid[y + 1][x - 1] == '.' then
            y = y + 1
            x = x - 1
        elseif grid[y + 1] and grid[y + 1][x + 1] == '.' then
            y = y + 1
            x = x + 1
        else
            grid[y][x] = 'o'
            sand_count = sand_count + 1
            if x == 500 and y == 0 then
                break
            end
            x, y = 500, 0
        end
    end
    return sand_count
end

local function main()
    local lines = read_input("input.txt")
    local paths = parse_paths(lines)
    local grid, floor_y = create_grid(paths)
    local sand_count = simulate_sand(grid, floor_y)
    print(sand_count)
end

main()
