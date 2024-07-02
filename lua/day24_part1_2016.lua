-- Function to read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local map = {}
    for line in file:lines() do
        table.insert(map, line)
    end
    file:close()
    return map
end

-- Function to find all points of interest
local function find_points(map)
    local points = {}
    for y, row in ipairs(map) do
        for x = 1, #row do
            local char = row:sub(x, x)
            if char:match("%d") then
                points[tonumber(char)] = {x = x, y = y}
            end
        end
    end
    return points
end

-- BFS to find shortest path between two points
local function bfs(map, start, goal)
    local queue = {{x = start.x, y = start.y, dist = 0}}
    local visited = {}
    local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local key = current.x .. "," .. current.y

        if current.x == goal.x and current.y == goal.y then
            return current.dist
        end

        if not visited[key] then
            visited[key] = true
            for _, dir in ipairs(directions) do
                local nx, ny = current.x + dir[1], current.y + dir[2]
                local nchar = map[ny] and map[ny]:sub(nx, nx) or "#"
                if nchar ~= "#" then
                    table.insert(queue, {x = nx, y = ny, dist = current.dist + 1})
                end
            end
        end
    end
    return nil
end

-- Function to generate all permutations
local function permutations(t)
    local n = #t
    return coroutine.wrap(function()
        if n == 0 then
            coroutine.yield({})
        else
            for i = 1, n do
                local first = t[i]
                local rest = {}
                for j = 1, n do
                    if j ~= i then
                        table.insert(rest, t[j])
                    end
                end
                for p in permutations(rest) do
                    table.insert(p, 1, first)
                    coroutine.yield(p)
                end
            end
        end
    end)
end

-- Function to print the map with points highlighted
local function print_map(map, points)
    for y, row in ipairs(map) do
        local line = ""
        for x = 1, #row do
            local char = row:sub(x, x)
            local is_point = false
            for num, point in pairs(points) do
                if point.x == x and point.y == y then
                    line = line .. "\27[31m" .. num .. "\27[0m"
                    is_point = true
                    break
                end
            end
            if not is_point then
                line = line .. char
            end
        end
        print(line)
    end
end

-- Main function
local function main()
    local map = read_input()
    local points = find_points(map)
    local num_points = 0
    for _ in pairs(points) do num_points = num_points + 1 end

    -- Calculate distances between all points
    local distances = {}
    for i = 0, num_points - 1 do
        distances[i] = {}
        for j = 0, num_points - 1 do
            if i ~= j then
                local dist = bfs(map, points[i], points[j])
                if not dist then
                    print("Error: No path found between points", i, "and", j)
                    return
                end
                distances[i][j] = dist
            end
        end
    end

    -- Generate all possible routes starting from 0
    local routes = {}
    local other_points = {}
    for i = 1, num_points - 1 do
        table.insert(other_points, i)
    end
    for perm in permutations(other_points) do
        table.insert(perm, 1, 0)
        table.insert(routes, perm)
    end

    -- Find the shortest route
    local min_distance = math.huge
    local shortest_route = nil
    for _, route in ipairs(routes) do
        local distance = 0
        for i = 1, #route - 1 do
            local from, to = route[i], route[i+1]
            distance = distance + distances[from][to]
        end
        if distance < min_distance then
            min_distance = distance
            shortest_route = route
        end
    end

    print("\nShortest path length:", min_distance)
end

main()
