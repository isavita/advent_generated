
local grid = {}
local start, end_pos
local as = {}
local y = 0

for line in io.lines("input.txt") do
    for x = 1, #line do
        local b = line:sub(x, x)
        local p = {x = x - 1, y = y}
        grid[p.y * 1000 + p.x] = b
        if b == 'S' then start = p
        elseif b == 'E' then end_pos = p
        elseif b == 'a' then table.insert(as, p) end
    end
    y = y + 1
end

grid[start.y * 1000 + start.x] = 'a'
grid[end_pos.y * 1000 + end_pos.x] = 'z'

local function neighbors(curr)
    return {
        {x = curr.x, y = curr.y + 1},
        {x = curr.x, y = curr.y - 1},
        {x = curr.x + 1, y = curr.y},
        {x = curr.x - 1, y = curr.y}
    }
end

local function dijkstra(grid, end_pos)
    local pq = {}
    local dist = {}
    dist[end_pos.y * 1000 + end_pos.x] = 0
    table.insert(pq, {pos = end_pos, dist = 0})

    while #pq > 0 do
        local curr = table.remove(pq)
        local curr_pos = curr.pos
        for _, n in ipairs(neighbors(curr_pos)) do
            local next_key = n.y * 1000 + n.x
            if grid[next_key] and (grid[curr_pos.y * 1000 + curr_pos.x]:byte() - grid[next_key]:byte() <= 1) then
                local next_dist = dist[curr_pos.y * 1000 + curr_pos.x] + 1
                if not dist[next_key] or next_dist < dist[next_key] then
                    dist[next_key] = next_dist
                    table.insert(pq, {pos = n, dist = next_dist})
                end
            end
        end
    end
    return dist
end

local dists = dijkstra(grid, end_pos)
local answer = dists[start.y * 1000 + start.x]
print(answer)
