
local Neighbors4 = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

local grid = {}
local visible = {}
local y = 0

for line in io.lines("input.txt") do
    for x = 1, #line do
        grid[x-1 .. "," .. y] = tonumber(line:sub(x, x))
    end
    y = y + 1
end

for p, _ in pairs(grid) do
    for _, n in ipairs(Neighbors4) do
        local next = {p:match("([^,]+),([^,]+)")}
        next[1], next[2] = tonumber(next[1]), tonumber(next[2])
        repeat
            next[1], next[2] = next[1] + n[1], next[2] + n[2]
            local nextKey = next[1] .. "," .. next[2]
            if grid[nextKey] ~= nil then
                if grid[nextKey] >= grid[p] then
                    break
                end
            else
                visible[p] = true
                break
            end
        until false
    end
end

local count = 0
for _ in pairs(visible) do
    count = count + 1
end

print(count)
