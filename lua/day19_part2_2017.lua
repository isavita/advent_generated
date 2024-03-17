local file = io.open("input.txt", "r")
local grid = {}

for line in file:lines() do
    local row = {}
    for i = 1, #line do
        table.insert(row, line:sub(i, i))
    end
    table.insert(grid, row)
end

local x, y = 1, 1

for i = 1, #grid[1] do
    if grid[1][i] == '|' then
        x = i
        break
    end
end

local dx, dy = 0, 1
local steps = 0

while true do
    if x < 1 or x > #grid[1] or y < 1 or y > #grid then
        break
    end

    local cell = grid[y][x]

    if cell == ' ' then
        break
    end

    steps = steps + 1

    if cell == '+' then
        if dx == 0 then
            if x > 1 and (grid[y][x - 1] == '-' or (grid[y][x - 1] >= 'A' and grid[y][x - 1] <= 'Z')) then
                dx, dy = -1, 0
            else
                dx, dy = 1, 0
            end
        else
            if y > 1 and (grid[y - 1][x] == '|' or (grid[y - 1][x] >= 'A' and grid[y - 1][x] <= 'Z')) then
                dx, dy = 0, -1
            else
                dx, dy = 0, 1
            end
        end
    end

    x = x + dx
    y = y + dy
end

print(steps)