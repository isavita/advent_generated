local file = io.open("input.txt", "r")
if not file then error("Failed to open input.txt") end
local input = file:read("*a")
file:close()

local nodes = {}
for line in input:gmatch("[^\n]+") do
    local x, y, used, avail = line:match("x(%d+)-y(%d+)%s+%d+T%s+(%d+)T%s+(%d+)T")
    if x and y then
        x, y, used, avail = tonumber(x), tonumber(y), tonumber(used), tonumber(avail)
        nodes[y] = nodes[y] or {}
        nodes[y][x] = {used = used, avail = avail}
    end
end

local function findHole(nodes)
    for y, row in pairs(nodes) do
        for x, node in pairs(row) do
            if node.used == 0 then
                return x, y
            end
        end
    end
    return nil, "no hole"
end

local function dim(nodes)
    local w, h = 0, 0
    for y, row in pairs(nodes) do
        h = math.max(h, y)
        for x in pairs(row) do
            w = math.max(w, x)
        end
    end
    return w, h
end

local function moves(nodes, goalX, goalY, fromX, fromY, toX, toY)
    local w, h = dim(nodes)
    local queue = {{x = fromX, y = fromY, steps = 0}}
    local visited = {}
    visited[fromY] = {}
    visited[fromY][fromX] = true
    local directions = {{0,1}, {0,-1}, {1,0}, {-1,0}}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        if current.x == toX and current.y == toY then
            return current.steps
        end
        for _, dir in ipairs(directions) do
            local nx, ny = current.x + dir[1], current.y + dir[2]
            if nx >= 0 and ny >= 0 and nx <= w and ny <= h and nodes[ny] and nodes[ny][nx] and nodes[ny][nx].used <= 400 and not (nx == goalX and ny == goalY) then
                if not visited[ny] then visited[ny] = {} end
                if not visited[ny][nx] then
                    visited[ny][nx] = true
                    table.insert(queue, {x = nx, y = ny, steps = current.steps + 1})
                end
            end
        end
    end
    return nil, "no possible path"
end

local function minmoves(nodes)
    local w, _ = dim(nodes)
    local goalX, goalY = w, 0
    local holeX, holeY = findHole(nodes)
    local sum = 0
    while not (goalX == 0 and goalY == 0) do
        local nextX, nextY = goalX - 1, goalY
        local m = moves(nodes, goalX, goalY, holeX, holeY, nextX, nextY)
        sum = sum + m
        holeX, holeY = nextX, nextY
        m = moves(nodes, goalX, goalY, goalX, goalY, holeX, holeY)
        sum = sum + m
        goalX, goalY, holeX, holeY = holeX, holeY, goalX, goalY
    end
    return sum
end

print(minmoves(nodes))