
local function read_file(filename)
    local file = io.open(filename, "r")
    if not file then return nil end
    local content = file:read("*all")
    file:close()
    return content
end

local function parse_map(input)
    local map = {}
    local lines = {}
    for line in input:gmatch("([^\n]+)") do
        table.insert(lines, line)
    end
    for y, line in ipairs(lines) do
        map[y] = {}
        for x = 1, #line do
            map[y][x] = line:sub(x, x)
        end
    end
    return map
end

local function find_start(map)
    for x = 1, #map[1] do
        if map[1][x] == "." then
            return 1, x
        end
    end
end

local function find_end(map)
    local last_row = #map
    for x = 1, #map[last_row] do
        if map[last_row][x] == "." then
            return last_row, x
        end
    end
end

local function get_neighbors(map, y, x)
    local neighbors = {}
    local current = map[y][x]
    local directions = {
        {y = y - 1, x = x, char = "^"},
        {y = y + 1, x = x, char = "v"},
        {y = y, x = x - 1, char = "<"},
        {y = y, x = x + 1, char = ">"}
    }
    for _, dir in ipairs(directions) do
        local ny, nx = dir.y, dir.x
        if map[ny] and map[ny][nx] then
            local next_char = map[ny][nx]
            if next_char == "." or next_char == dir.char then
                table.insert(neighbors, {y = ny, x = nx})
            end
        end
    end
    return neighbors
end

local function find_longest_path(map, start_y, start_x, end_y, end_x)
    local max_steps = 0
    local visited = {}

    local function dfs(y, x, steps)
        if y == end_y and x == end_x then
            max_steps = math.max(max_steps, steps)
            return
        end

        visited[y .. "," .. x] = true
        local neighbors = get_neighbors(map, y, x)
        for _, neighbor in ipairs(neighbors) do
            local ny, nx = neighbor.y, neighbor.x
            if not visited[ny .. "," .. nx] then
                dfs(ny, nx, steps + 1)
            end
        end
        visited[y .. "," .. x] = nil
    end

    dfs(start_y, start_x, 0)
    return max_steps
end

local input = read_file("input.txt")
if not input then
    print("Error: Could not read input.txt")
    os.exit(1)
end

local map = parse_map(input)
local start_y, start_x = find_start(map)
local end_y, end_x = find_end(map)

local longest_path = find_longest_path(map, start_y, start_x, end_y, end_x)
print(longest_path)
