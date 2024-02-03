
local file = io.open("input.txt", "r")
local target = tonumber(file:read("*all"))

local grid = {}
grid[0] = {}
grid[0][0] = 1

local x, y = 0, 0
local dx, dy = 0, -1

while true do
    if x == y or (x < 0 and x == -y) or (x > 0 and x == 1 - y) then
        dx, dy = -dy, dx
    end

    x = x + dx
    y = y + dy

    local value = 0
    for i = -1, 1 do
        for j = -1, 1 do
            if grid[x + i] and grid[x + i][y + j] then
                value = value + grid[x + i][y + j]
            end
        end
    end
    if not grid[x] then
        grid[x] = {}
    end
    grid[x][y] = value

    if value > target then
        print(value)
        break
    end
end
