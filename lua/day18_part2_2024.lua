
local N = 71

local function canReach(grid)
    local visited = {}
    for i = 0, N*N - 1 do
        visited[i] = false
    end
    local dirs = {{1,0}, {-1,0}, {0,1}, {0,-1}}
    local queue = {}
    local front = 1
    local rear = 0
    local start_idx = 0 + 0 * N
    if grid[start_idx] then
        return false
    end
    visited[start_idx] = true
    queue[rear + 1] = start_idx
    rear = rear + 1
    while front <= rear do
        local idx = queue[front]
        front = front + 1
        local x = idx % N
        local y = math.floor((idx - x) / N)
        if x == N-1 and y == N-1 then
            return true
        end
        for _, d in ipairs(dirs) do
            local dx, dy = d[1], d[2]
            local nx = x + dx
            local ny = y + dy
            if nx >= 0 and nx < N and ny >= 0 and ny < N then
                local nidx = nx + ny * N
                if not grid[nidx] and not visited[nidx] then
                    visited[nidx] = true
                    queue[rear + 1] = nidx
                    rear = rear + 1
                end
            end
        end
    end
    return false
end

local grid = {}
for i = 0, N*N - 1 do
    grid[i] = false
end

local found = false
for line in io.lines("input.txt") do
    local parts = {}
    for part in line:gmatch("%d+") do
        table.insert(parts, part)
    end
    if #parts ~= 2 then
        goto continue
    end
    local x = tonumber(parts[1])
    local y = tonumber(parts[2])
    if x >= 0 and x < N and y >= 0 and y < N then
        local idx = x + y * N
        grid[idx] = true
        if not canReach(grid) then
            print(string.format("%d,%d", x, y))
            found = true
            break
        end
    end
    ::continue::
end
if not found then
    print("No cutoff found")
end
