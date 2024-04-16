local file = io.open("input.txt", "r")
if not file then
    error("Failed to open input.txt")
end

local grid = {}
for line in file:lines() do
    table.insert(grid, line)
end
file:close()

local x, y = 0, 1
for i = 1, #grid[1] do
    if grid[1]:sub(i, i) == '|' then
        x = i
        break
    end
end

local dx, dy = 0, 1
local letters = {}

while true do
    if x < 1 or x > #grid[1] or y < 1 or y > #grid then
        break
    end

    local cell = grid[y]:sub(x, x)

    if cell == ' ' then
        break
    end

    if cell:match('[A-Z]') then
        table.insert(letters, cell)
    end

    if cell == '+' then
        if dx == 0 then
            if x > 1 and (grid[y]:sub(x-1, x-1) == '-' or grid[y]:sub(x-1, x-1):match('[A-Z]')) then
                dx, dy = -1, 0
            else
                dx, dy = 1, 0
            end
        else
            if y > 1 and (grid[y-1]:sub(x, x) == '|' or grid[y-1]:sub(x, x):match('[A-Z]')) then
                dx, dy = 0, -1
            else
                dx, dy = 0, 1
            end
        end
    end

    x = x + dx
    y = y + dy
end

print(table.concat(letters))