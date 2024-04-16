local function read_map(filename)
    local file = io.open(filename, "r")
    local map = {}
    local points = {}
    if file then
        local y = 1
        for line in file:lines() do
            map[y] = {}
            for x = 1, #line do
                local char = line:sub(x, x)
                map[y][x] = char
                if char:match("%d") then
                    points[char] = {x = x, y = y}
                end
            end
            y = y + 1
        end
        file:close()
    end
    return map, points
end

local function bfs(map, start)
    local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}
    local queue = {{x = start.x, y = start.y, dist = 0}}
    local visited = {}
    local distances = {}
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local key = current.x .. "," .. current.y
        if not visited[key] then
            visited[key] = true
            if map[current.y][current.x]:match("%d") then
                distances[map[current.y][current.x]] = current.dist
            end
            for _, dir in ipairs(directions) do
                local nx, ny = current.x + dir[1], current.y + dir[2]
                if map[ny] and map[ny][nx] and map[ny][nx] ~= '#' and not visited[nx .. "," .. ny] then
                    table.insert(queue, {x = nx, y = ny, dist = current.dist + 1})
                end
            end
        end
    end
    return distances
end

local function tsp(distances, start, must_return_to_start)
    local memo = {}
    local function recurse(path, last, visited)
        if visited == (1 << #path) - 1 then
            return must_return_to_start and distances[path[last]][start] or 0
        end
        local key = last .. "," .. visited
        if not memo[key] then
            local min_dist = math.huge
            for i, point in ipairs(path) do
                if not (visited & (1 << (i - 1)) > 0) then
                    local next_dist = recurse(path, i, visited | (1 << (i - 1))) + distances[path[last]][path[i]]
                    min_dist = math.min(min_dist, next_dist)
                end
            end
            memo[key] = min_dist
        end
        return memo[key]
    end
    local path = {}
    for point in pairs(distances[start]) do
        table.insert(path, point)
    end
    table.insert(path, start) -- ensure start is included
    return recurse(path, #path, 1 << (#path - 1))
end

local function solve(filename)
    local map, points = read_map(filename)
    local all_distances = {}
    for point, coords in pairs(points) do
        all_distances[point] = bfs(map, coords)
    end
    local part1 = tsp(all_distances, '0', false)
    local part2 = tsp(all_distances, '0', true)
    print("Part 1:", part1)
    print("Part 2:", part2)
end

solve("input.txt")