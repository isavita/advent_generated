local favoriteNumber = 1362 -- Replace with your puzzle input

local function isWall(x, y)
    local num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    local bits = 0
    while num > 0 do
        if num % 2 == 1 then
            bits = bits + 1
        end
        num = math.floor(num / 2)
    end
    return bits % 2 ~= 0
end

local function bfsMaxSteps(start, maxSteps)
    local visited = {}
    local queue = {start}
    visited[start.x .. "," .. start.y] = true
    local steps = 0

    while #queue > 0 and steps < maxSteps do
        local size = #queue
        for i = 1, size do
            local point = queue[i]
            local deltas = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}}

            for _, delta in ipairs(deltas) do
                local next = {x = point.x + delta[1], y = point.y + delta[2]}
                local key = next.x .. "," .. next.y
                if next.x >= 0 and next.y >= 0 and not isWall(next.x, next.y) and not visited[key] then
                    visited[key] = true
                    table.insert(queue, next)
                end
            end
        end
        queue = {table.unpack(queue, size + 1)}
        steps = steps + 1
    end

    local count = 0
    for _ in pairs(visited) do count = count + 1 end
    return count
end

local start = {x = 1, y = 1}
local reachableLocations = bfsMaxSteps(start, 50)
print(reachableLocations)