
function isWall(favoriteNumber, x, y)
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

function bfs(start, target, favoriteNumber)
    local visited = {}
    local queue = {start}
    local steps = 0

    while #queue > 0 do
        local size = #queue
        for i = 1, size do
            local point = queue[i]
            if point.x == target.x and point.y == target.y then
                return steps
            end

            local deltas = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}}
            for _, delta in ipairs(deltas) do
                local next = {x = point.x + delta[1], y = point.y + delta[2]}
                if next.x >= 0 and next.y >= 0 and not isWall(favoriteNumber, next.x, next.y) and not visited[next.x .. "," .. next.y] then
                    visited[next.x .. "," .. next.y] = true
                    table.insert(queue, next)
                end
            end
        end
        for i = 1, size do
            table.remove(queue, 1)
        end
        steps = steps + 1
    end

    return -1
end

local file = io.open("input.txt", "r")
local favoriteNumber = tonumber(file:read("*line"))
file:close()

local start = {x = 1, y = 1}
local target = {x = 31, y = 39}
local steps = bfs(start, target, favoriteNumber)
print(steps)
