
local grid = {}
local startX, startY
local y = 0
local file = io.open("input.txt", "r")
for line in file:lines() do
    for x = 1, #line do
        if line:sub(x, x) == '#' then
            grid[x .. "," .. y] = 2
        end
    end
    startX = #line // 2 + 1
    startY = y // 2
    y = y + 1
end
file:close()

local dx = {0, 1, 0, -1}
local dy = {-1, 0, 1, 0}
local x, y, dir = startX, startY, 1
local infectedCount = 0

for i = 1, 10000000 do
    local pos = x .. "," .. y
    local state = grid[pos] or 0
    if state == 0 then
        dir = (dir - 2) % 4 + 1
        grid[pos] = 1
    elseif state == 1 then
        grid[pos] = 2
        infectedCount = infectedCount + 1
    elseif state == 2 then
        dir = dir % 4 + 1
        grid[pos] = 3
    else
        dir = (dir + 1) % 4 + 1
        grid[pos] = 0
    end
    x = x + dx[dir]
    y = y + dy[dir]
end

print(infectedCount)
