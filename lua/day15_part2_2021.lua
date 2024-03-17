local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local function dijkstra(grid)
    local pq = {}
    local function push(x)
        table.insert(pq, x)
        table.sort(pq, function(a, b) return a.risk < b.risk end)
    end
    local function pop()
        return table.remove(pq, 1)
    end

    push { x = 1, y = 1, risk = 0 }

    local rows, cols = #grid, #grid[1]
    local dist = {}
    for i = 1, rows do
        dist[i] = {}
        for j = 1, cols do
            dist[i][j] = math.huge
        end
    end
    dist[1][1] = 0

    local directions = { { 1, 0, 0 }, { 0, 1, 0 }, { -1, 0, 0 }, { 0, -1, 0 } }

    while #pq > 0 do
        local curr = pop()
        if curr.x == rows and curr.y == cols then
            return curr.risk
        end
        for _, d in ipairs(directions) do
            local nx, ny = curr.x + d[1], curr.y + d[2]
            if nx >= 1 and ny >= 1 and nx <= rows and ny <= cols then
                local nextRisk = curr.risk + grid[nx][ny]
                if nextRisk < dist[nx][ny] then
                    dist[nx][ny] = nextRisk
                    push { x = nx, y = ny, risk = nextRisk }
                end
            end
        end
    end
    return -1
end

local function extendGrid(initialGrid)
    local rows, cols = #initialGrid, #initialGrid[1]
    local extendedGrid = {}
    for i = 1, rows * 5 do
        extendedGrid[i] = {}
        for j = 1, cols * 5 do
            local newRisk = initialGrid[(i - 1) % rows + 1][(j - 1) % cols + 1] + (i - 1) // rows + (j - 1) // cols
            if newRisk > 9 then
                newRisk = newRisk - 9
            end
            extendedGrid[i][j] = newRisk
        end
    end
    return extendedGrid
end

local initialGrid = {}
for line in file:lines() do
    local row = {}
    for char in line:gmatch(".") do
        table.insert(row, tonumber(char))
    end
    table.insert(initialGrid, row)
end
file:close()

local extendedGrid = extendGrid(initialGrid)
print(dijkstra(extendedGrid))