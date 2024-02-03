
local Neighbors4 = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

local grid = {}
local y = 0
for line in io.lines("input.txt") do
    for x = 1, #line do
        grid[x-1 .. "," .. y] = tonumber(line:sub(x, x))
    end
    y = y + 1
end

local maxScore = 0
for p, _ in pairs(grid) do
    local score = 1
    for _, n in ipairs(Neighbors4) do
        local next, view = {tonumber(p:match("(%d+),")), tonumber(p:match(",(%d+)"))}, 0
        while true do
            next[1], next[2] = next[1] + n[1], next[2] + n[2]
            if grid[next[1] .. "," .. next[2]] then
                view = view + 1
                if grid[next[1] .. "," .. next[2]] >= grid[p] then
                    score = score * view
                    break
                end
            else
                score = score * view
                break
            end
        end
    end

    if score > maxScore then
        maxScore = score
    end
end

print(maxScore)
