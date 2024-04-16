-- Simple queue implementation
local Queue = {}
function Queue.new()
    return {first = 0, last = -1}
end

function Queue.push(queue, value)
    local last = queue.last + 1
    queue.last = last
    queue[last] = value
end

function Queue.pop(queue)
    local first = queue.first
    if first > queue.last then error("Queue is empty") end
    local value = queue[first]
    queue[first] = nil        -- to allow garbage collection
    queue.first = first + 1
    return value
end

function Queue.isEmpty(queue)
    return queue.first > queue.last
end

local function parseMap(filename)
    local file = io.open(filename, "r")
    if not file then error("Failed to open file: " .. filename) end
    local map = {}
    local keys = {}
    local doors = {}
    local start

    local y = 1
    for line in file:lines() do
        map[y] = {}
        for x = 1, #line do
            local char = line:sub(x, x)
            map[y][x] = char
            if char == '@' then
                start = {x = x, y = y}
            elseif char:match("%l") then
                keys[char] = {x = x, y = y}
            elseif char:match("%u") then
                doors[char] = {x = x, y = y}
            end
        end
        y = y + 1
    end
    file:close()
    return map, start, keys, doors
end

local function bfs(map, start, keys, doors)
    local directions = {{0,1}, {1,0}, {0,-1}, {-1,0}}
    local keyCount = 0
    local allKeysMask = 0
    for k, _ in pairs(keys) do
        allKeysMask = allKeysMask | (1 << (k:byte() - 97))
        keyCount = keyCount + 1
    end

    local queue = Queue.new()
    Queue.push(queue, {pos = start, steps = 0, keys = 0})
    local visited = {}
    visited[start.y] = {}
    visited[start.y][start.x] = {}

    while not Queue.isEmpty(queue) do
        local current = Queue.pop(queue)
        local cx, cy = current.pos.x, current.pos.y
        local ckeys = current.keys

        for _, dir in ipairs(directions) do
            local nx, ny = cx + dir[1], cy + dir[2]
            local cell = map[ny] and map[ny][nx]
            if cell and cell ~= '#' and not (visited[ny] and visited[ny][nx] and visited[ny][nx][ckeys]) then
                if cell:match("%l") then  -- It's a key
                    local newKeys = ckeys | (1 << (cell:byte() - 97))
                    if newKeys == allKeysMask then
                        return current.steps + 1
                    end
                    Queue.push(queue, {pos = {x = nx, y = ny}, steps = current.steps + 1, keys = newKeys})
                    visited[ny] = visited[ny] or {}
                    visited[ny][nx] = visited[ny][nx] or {}
                    visited[ny][nx][newKeys] = true
                elseif cell:match("%u") then  -- It's a door
                    if ckeys & (1 << (cell:byte() - 65)) ~= 0 then  -- Has key for door
                        Queue.push(queue, {pos = {x = nx, y = ny}, steps = current.steps + 1, keys = ckeys})
                        visited[ny] = visited[ny] or {}
                        visited[ny][nx] = visited[ny][nx] or {}
                        visited[ny][nx][ckeys] = true
                    end
                else  -- It's an open path or the start
                    Queue.push(queue, {pos = {x = nx, y = ny}, steps = current.steps + 1, keys = ckeys})
                    visited[ny] = visited[ny] or {}
                    visited[ny][nx] = visited[ny][nx] or {}
                    visited[ny][nx][ckeys] = true
                end
            end
        end
    end
end

local function main()
    local map, start, keys, doors = parseMap("input.txt")
    local steps = bfs(map, start, keys, doors)
    print("Shortest path to collect all keys is:", steps)
end

main()