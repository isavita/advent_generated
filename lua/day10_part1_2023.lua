
local function read_input(filename)
    local file = io.open(filename, "r")
    if not file then return nil end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function find_start(grid)
    for r, row in ipairs(grid) do
        for c = 1, #row do
            if row:sub(c, c) == "S" then
                return r, c
            end
        end
    end
    return nil, nil
end

local function get_neighbors(grid, r, c)
    local neighbors = {}
    local char = grid[r]:sub(c, c)

    if char == "|" or char == "S" then
        if r > 1 then table.insert(neighbors, {r - 1, c}) end
        if r < #grid then table.insert(neighbors, {r + 1, c}) end
    end
    if char == "-" or char == "S" then
        if c > 1 then table.insert(neighbors, {r, c - 1}) end
        if c < #grid[r] then table.insert(neighbors, {r, c + 1}) end
    end
    if char == "L" or char == "S" then
        if r > 1 then table.insert(neighbors, {r - 1, c}) end
        if c < #grid[r] then table.insert(neighbors, {r, c + 1}) end
    end
    if char == "J" or char == "S" then
        if r > 1 then table.insert(neighbors, {r - 1, c}) end
        if c > 1 then table.insert(neighbors, {r, c - 1}) end
    end
    if char == "7" or char == "S" then
        if r < #grid then table.insert(neighbors, {r + 1, c}) end
        if c > 1 then table.insert(neighbors, {r, c - 1}) end
    end
    if char == "F" or char == "S" then
        if r < #grid then table.insert(neighbors, {r + 1, c}) end
        if c < #grid[r] then table.insert(neighbors, {r, c + 1}) end
    end
    return neighbors
end

local function solve()
    local grid = read_input("input.txt")
    if not grid then
        print("Error reading input file.")
        return
    end

    local start_r, start_c = find_start(grid)
    if not start_r then
        print("Start position 'S' not found.")
        return
    end

    local distances = {}
    local queue = {{start_r, start_c, 0}}
    local visited = {}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local r, c, dist = current[1], current[2], current[3]

        local key = r .. "," .. c
        if visited[key] then
            goto continue
        end
        visited[key] = true
        distances[key] = dist

        local neighbors = get_neighbors(grid, r, c)
        for _, neighbor in ipairs(neighbors) do
            local nr, nc = neighbor[1], neighbor[2]
            local nkey = nr .. "," .. nc
            local valid_neighbor = false
            for _, nn in ipairs(get_neighbors(grid, nr, nc)) do
                if nn[1] == r and nn[2] == c then
                    valid_neighbor = true
                    break
                end
            end
            if valid_neighbor then
                table.insert(queue, {nr, nc, dist + 1})
            end
        end
        ::continue::
    end

    local max_dist = 0
    for _, dist in pairs(distances) do
        if dist > max_dist then
            max_dist = dist
        end
    end

    print(max_dist)
end

solve()
