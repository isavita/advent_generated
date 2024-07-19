
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

local function neighbors(p)
    return {
        {x = p.x, y = p.y + 1},
        {x = p.x, y = p.y - 1},
        {x = p.x + 1, y = p.y},
        {x = p.x - 1, y = p.y}
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
        local curr_key = curr_pos.y * 1000 + curr_pos.x

        for _, n in ipairs(neighbors(curr_pos)) do
            local next_key = n.y * 1000 + n.x
            if not grid[next_key] then goto continue end
            if (grid[curr_key]:byte() - grid[next_key]:byte()) > 1 then goto continue end
            
            local next_dist = dist[curr_key] + 1
            if not dist[next_key] or next_dist < dist[next_key] then
                dist[next_key] = next_dist
                table.insert(pq, {pos = n, dist = next_dist})
            end
            ::continue::
        end
    end
    return dist
end

local dists = dijkstra(grid, end_pos)
local l = dists[start.y * 1000 + start.x] or math.huge

for _, a in ipairs(as) do
    local d = dists[a.y * 1000 + a.x]
    if d then l = math.min(l, d) end
end

print(l)
