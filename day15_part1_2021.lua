
local PriorityQueue = {}
PriorityQueue.__index = PriorityQueue

function PriorityQueue:new()
    local pq = {data = {}}
    setmetatable(pq, PriorityQueue)
    return pq
end

function PriorityQueue:len()
    return #self.data
end

function PriorityQueue:empty()
    return #self.data == 0
end

function PriorityQueue:push(item)
    table.insert(self.data, item)
    table.sort(self.data, function(a, b) return a[3] < b[3] end)
end

function PriorityQueue:pop()
    return table.remove(self.data, 1)
end

function dijkstra(grid)
    local pq = PriorityQueue:new()
    pq:push({1, 1, 0})

    local rows = #grid
    local cols = #grid[1]
    local dist = {}
    for i = 1, rows do
        dist[i] = {}
        for j = 1, cols do
            dist[i][j] = 2^31 - 1
        end
    end
    dist[1][1] = 0

    local directions = {{1, 0, 0}, {0, 1, 0}, {-1, 0, 0}, {0, -1, 0}}

    while not pq:empty() do
        local curr = pq:pop()
        if curr[1] == rows and curr[2] == cols then
            return curr[3]
        end
        for _, d in ipairs(directions) do
            local nx, ny = curr[1] + d[1], curr[2] + d[2]
            if nx >= 1 and ny >= 1 and nx <= rows and ny <= cols then
                local nextRisk = curr[3] + grid[nx][ny]
                if nextRisk < dist[nx][ny] then
                    dist[nx][ny] = nextRisk
                    pq:push({nx, ny, nextRisk})
                end
            end
        end
    end
    return -1
end

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local grid = {}
for line in file:lines() do
    local row = {}
    for i = 1, #line do
        table.insert(row, tonumber(line:sub(i, i)))
    end
    table.insert(grid, row)
end
file:close()

print(dijkstra(grid))
